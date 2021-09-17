namespace DLLAnalysis.Summary

open WinHeader
open DLLAnalysis
open DLLAnalysis.Const
open DLLAnalysis.AbstractDomain
open DLLAnalysis.Utils

// Mapping from argument index to their type constraint.
type Constraints = Map<int, Set<ArgType>>

// Since we apply constraints after applying side effects (cf. Call.runSubrtn
// function), we should snapshot the (1) argument index, (2) equal-to registers,
// and (3) equal-to locations.
type CallSnapshot = Map<int, AbsVal * Set<Register> * AbsLocSet>

module Constraints =

  let empty: Constraints = Map.empty

  let toString (constrs: Constraints) =
    let mapper (i, typs) =
      let typStrs = Set.map (ArgType.toJson false "") typs
      sprintf "Arg%d: %s" (i + 1) (String.concat ", " typStrs)
    Map.toList constrs |> List.map mapper |> String.concat "\n"

  let private takeSnapshotAux record state accMap argIdx: CallSnapshot =
    let argVal = State.getArgument None record argIdx state
    let eqRegs = State.getRegsEqualToArg argIdx state |> Set.remove Register.ret
    let eqLocs = State.getLocsEqualToArg argIdx state
    Map.add argIdx (argVal, eqRegs, eqLocs) accMap

  let takeSnapshot localInfo state (constrs: Constraints): CallSnapshot =
    let record = LocalInfo.getRecord localInfo
    let indices = Map.keys constrs
    List.fold (takeSnapshotAux record state) Map.empty indices

  let makeFromAPI funcTyp: Constraints =
    // Caution: Size and inout information are not valid in this context.
    let folder accMap i t =
      ArgType.removeCtxSize t |> ArgType.removeInOut |> Set.singleton
      |> Map.add i <| accMap
    List.foldi folder empty funcTyp

  let private recordArgConstrAux record argTyps sym =
    try Record.addConstr (Convert.symbolToArgIdx sym record) argTyps record with
    | ConvertException -> ()

  let private recordArgConstr localInfo argVal argTyps =
    let record = LocalInfo.getRecord localInfo
    let typSyms = AbsVal.getConstr argVal |> TypeConstr.collectSymbols
    Set.iter (recordArgConstrAux record argTyps) typSyms

  let private addConstrToReg argTyps accState reg =
    // No side-effect on EQ relation or def/use set.
    let curVal = State.read reg accState
    let newConstr = TypeConstr.ofConcrTypes argTyps
    let newVal = AbsVal.setConstr newConstr curVal
    State.assign reg newVal Variable.NopVar accState

  let private addConstrToLoc localInfo argTyps accState loc =
    let record = LocalInfo.getRecord localInfo
    // No side-effect on EQ relation or def/use set.
    let absMem = State.getAbsMem accState
    let locSet = AbsLocSet.make [loc]
    let curVal = AbsMem.load record absMem locSet
    let newConstr = TypeConstr.ofConcrTypes argTyps
    let newVal = AbsVal.setConstr newConstr curVal
    let newAbsMem = AbsMem.store record locSet newVal absMem
    State.setAbsMem accState newAbsMem

  // Add type constraint to arguments with equal-to relation.
  let private addConstrWithEQs localInfo eqRegs eqLocs argTyps accState =
    Set.fold (addConstrToReg argTyps) accState eqRegs
    |> Set.fold (addConstrToLoc localInfo argTyps) <| eqLocs

  let private addSizeHint locs structSize state =
    match structSize with
    | Fixed ui64 -> State.addSizeHint locs (NUInt.ofUInt64 ui64) state
    | _ -> state

  let rec private addConstrToMemAux localInfo depth typ accState loc =
    let mem = State.getAbsMem accState
    let curVal = AbsMem.find loc mem
    let newDepth = depth + 1
    let accState = if depth >= STRUCT_DEPTH_LIMIT then accState
                   else addPtrConstrAux localInfo newDepth curVal accState typ
    if AbsVal.isZero curVal then accState
    else // To avoid spawning unnecessary symbolic values, maniuplate abstract
         // memory directly, without using State.load() or State.store().
         let newConstr = TypeConstr.ofConcrTypes (Set.singleton typ)
         let newVal = AbsVal.setConstr newConstr curVal
         let newMem = AbsMem.add loc newVal mem
         State.setAbsMem accState newMem

  and private addConstrToMem localInfo depth accState locs argTyp =
    Set.fold (addConstrToMemAux localInfo depth argTyp) accState locs

  and private addStructConstrAux localInfo depth structFields accState baseLoc =
    let folder acc fldOffset fldTyp =
      let fldOffset = Offset.ofUInt64 fldOffset
      let fldLoc = AbsLoc.addOffset baseLoc fldOffset
      addConstrToMemAux localInfo depth fldTyp acc fldLoc
    Map.fold folder accState structFields

  and private addStructConstr localInfo depth size fields state baseLocs =
    Set.fold (addStructConstrAux localInfo depth fields) state baseLocs
    |> addSizeHint baseLocs size

  // Add type constraint by recursively following into a pointer type.
  and private addPtrConstrAux localInfo depth v accState typ =
    match typ with
    | Ptr (Struct (structSize, structFields), _) ->
      let locs = AbsVal.getLoc v
      addStructConstr localInfo depth structSize structFields accState locs
    | Ptr (Array (_, _, innerTyp), _) // Let's just update the first element.
    | Ptr (innerTyp, _) ->
      let locs = AbsVal.getLoc v
      addConstrToMem localInfo depth accState locs innerTyp
    | _ -> accState

  and private addPtrConstr localInfo argTyps argVal state =
    Set.fold (addPtrConstrAux localInfo 0 argVal) state argTyps

  let private applyAux localInfo snapshot accState argIdx argTyps =
    let argVal, eqRegs, eqLocs = Map.find argIdx snapshot
    // Avoid adding constraint to NULL.
    if AbsVal.isZero argVal then accState
    else recordArgConstr localInfo argVal argTyps
         addConstrWithEQs localInfo eqRegs eqLocs argTyps accState
         |> addPtrConstr localInfo argTyps argVal

  let apply localInfo snapshot constrs state =
    Map.fold (applyAux localInfo snapshot) state constrs
