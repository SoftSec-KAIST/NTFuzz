namespace DLLAnalysis.Summary

open DLLAnalysis
open DLLAnalysis.Const
open DLLAnalysis.AbstractDomain

type Substitution = {
  IntSubst : Map<Symbol,AbsInt>
  LocSubst : Map<Symbol,AbsLocSet>
  FuncSubst : Map<Symbol,FunctionSet>
  TypSubst: Map<Symbol,ArgTypeSet>
}

module Substitution =

  let empty = {
    IntSubst = Map.empty
    LocSubst = Map.empty
    FuncSubst = Map.empty
    TypSubst = Map.empty
  }

  /// Apply a substitution to a value, by replacing symbols.
  let apply subst v : AbsVal =
    let intSubst = subst.IntSubst
    let locSubst = subst.LocSubst
    let funcSubst = subst.FuncSubst
    let typSubst = subst.TypSubst
    AbsVal.substitute v intSubst locSubst funcSubst typSubst

  /// Add a mapping from symbol to value.
  let private addMapping prefixSym v subst =
    let intSym = Symbol.appendIntSuffix prefixSym
    let locSym = Symbol.appendLocSuffix prefixSym
    let funcSym = Symbol.appendFunctionSuffix prefixSym
    let typSym = Symbol.appendTypSuffix prefixSym
    let absInt = AbsVal.getInt v
    let prevAbsInt =
      match Map.tryFind intSym subst.IntSubst with
      | None -> AbsInt.bot
      | Some i -> i
    let newAbsInt = AbsInt.join prevAbsInt absInt
    let locs = AbsVal.getLoc v
    let prevLocs =
      match Map.tryFind locSym subst.LocSubst with
      | None -> AbsLocSet.bot
      | Some locs -> locs
    let newLocs = AbsLocSet.join locs prevLocs
    let funcs = AbsVal.getFunc v
    let prevFuncs =
      match Map.tryFind funcSym subst.FuncSubst with
      | None -> FunctionSet.bot
      | Some funcs -> funcs
    let newFuncs = FunctionSet.join funcs prevFuncs
    let typs = TypeConstr.getArgTypes (AbsVal.getConstr v)
    let prevTyps =
      match Map.tryFind typSym subst.TypSubst with
      | None -> ArgTypeSet.bot
      | Some typs -> typs
    let newTyps = ArgTypeSet.join typs prevTyps
    { subst with
        IntSubst = Map.add intSym newAbsInt subst.IntSubst
        LocSubst = Map.add locSym newLocs subst.LocSubst
        FuncSubst = Map.add funcSym newFuncs subst.FuncSubst
        TypSubst = Map.add typSym newTyps subst.TypSubst }

  /// Add mappings from symbol to value by investigating register map. We only
  /// have to investigate initialized registers (cf. RegMap.initialize).
  let private collectFromRegMap callerState subst =
    List.fold (fun accSubst reg ->
      let prefixSymbol = Register.decideSymbol reg
      // No side-effect on def/uset set.
      let vCaller = State.read reg callerState
      addMapping prefixSymbol vCaller accSubst
    ) subst Register.inputs

  /// Convert stack argument location from callee's perspective to caller's
  /// perspective.
  let private convertStackLoc callerStackPtrs loc =
    match loc with
    | AbsLoc.Stack (_, offset) ->
      // Considering of alloca(), we should allow a single stack location to be
      // converted into multiple locations.
      Set.toList (AbsLocSet.addOffset callerStackPtrs offset)
    | AbsLoc.Global _ -> [loc]
    | AbsLoc.Heap _ -> [loc]
    | AbsLoc.SLoc _ -> [loc]

  /// Convert stack argument locations in 'locMap'.
  let private convertStackLocMap callerStackPtrs locMap =
    let folder accMap l s =
      let convertedLocs = convertStackLoc callerStackPtrs l
      List.fold (fun accMap l -> Map.add l s accMap) accMap convertedLocs
    Map.fold folder Map.empty locMap

  let rec private collectFromArgLocsAux depth localInfo state locSymMap subst loc =
    if depth >= STRUCT_DEPTH_LIMIT then subst
    else
      let absMem = State.getAbsMem state
      let record = LocalInfo.getRecord localInfo
      // First, should replace 'loc' based on current substitution.
      let intSubst = subst.IntSubst
      let locSubst = subst.LocSubst
      let locs = AbsLocSet.substitute (AbsLocSet.make [loc]) intSubst locSubst
      let nLocs = AbsLocSet.count locs
      let vCaller = if nLocs > READ_WRITE_NLOC_LIMIT then AbsVal.unknownInt
                    else AbsMem.load record absMem locs // Not State.load().
      let sym = Map.find loc locSymMap
      let subst = addMapping sym vCaller subst
      // Now, recurse over children locations.
      let isChild l = // Check child (i.e. location derived from 'loc'.)
        AbsLoc.isSymbolic l && AbsLoc.getSymbol l = Symbol.appendLocSuffix sym
      let childLocs = Map.keys locSymMap |> List.filter isChild
      let depth = depth + 1
      let folder = collectFromArgLocsAux depth localInfo state locSymMap
      List.fold folder subst childLocs

  let private collectFromArgLocs localInfo state locSymMap subst rootLocs =
    List.fold (collectFromArgLocsAux 0 localInfo state locSymMap) subst rootLocs

  /// Add mappings from symbol to value by investigating memory. We only have to
  /// investigate argument locations recorded in 'locSymMap'. Note that this
  /// 'locSymMap' is from callee subroutine.
  let collectFromMemory localInfo callerState locSymMap subst =
    let argLocs = Map.keys locSymMap
    let rootLocs = List.filter AbsLoc.isRootArgument argLocs
    let callerStackPtrs = State.getStackPtrLoc callerState
    // Should preprocess argLocMap and rootLocs by converting stack locations.
    let locSymMap = convertStackLocMap callerStackPtrs locSymMap
    let rootLocs = List.collect (convertStackLoc callerStackPtrs) rootLocs
    // Now start recursive traversal and collect mappings.
    collectFromArgLocs localInfo callerState locSymMap subst rootLocs

  let toString subst =
    let intSyms = Map.keys subst.IntSubst
    let locSyms = Map.keys subst.LocSubst
    let funcSyms = Map.keys subst.FuncSubst
    sprintf "(intSyms)\n%s\n" (String.concat ", " intSyms) +
    sprintf "(locSyms)\n%s\n" (String.concat ", " locSyms) +
    sprintf "(funcSyms)\n%s\n" (String.concat ", " funcSyms)

  let make localInfo callerState calleeLocSymMap =
    // First, add mappings in register map.
    let subst = collectFromRegMap callerState empty
    // Now try to add add mappings from memory.
    collectFromMemory localInfo callerState calleeLocSymMap subst
