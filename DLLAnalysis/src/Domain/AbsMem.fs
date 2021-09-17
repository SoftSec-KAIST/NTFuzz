module DLLAnalysis.AbsDom.AbsMem

open System.Collections.Generic
open DLLAnalysis
open DLLAnalysis.FrontEnd // For struct lookup.
open DLLAnalysis.AbsDom.Functor
open DLLAnalysis.AbsDom.AbsLoc
open DLLAnalysis.AbsDom.AbsVal
open DLLAnalysis.AbsDom.Record
open DLLAnalysis.AbsDom.Function

type AbsMem = Map<AbsLoc,AbsVal>

type AbsMemModule() =
  inherit FunDomain<AbsLoc,AbsVal>(AbsLoc,AbsVal)

  // Memoization log, since many subroutines will request the abstract memory
  // initialization of the same binary.
  let initializeMemo: Dictionary<string,AbsMem> =
    new Dictionary<string,AbsMem> ()

  member private __.pruneVal l v =
    let offset = AbsLoc.getOffset l
    if not (Offset.isAligned offset) then AbsVal.removeNonInt v
    elif AbsLoc.isHeap l then AbsVal.removeStackLoc v
    else v

  member __.add loc v mem: AbsMem =
    if AbsLoc.isInvalid loc then mem
    else Map.add loc (__.pruneVal loc v) mem

  member __.find loc mem: AbsVal =
    if AbsLoc.isInvalid loc then AbsVal.bot
    else try Map.find loc mem with :? KeyNotFoundException -> AbsVal.bot
         |> __.pruneVal loc

  member __.findSet locs mem: AbsVal =
    let folder accVal l = AbsVal.join (__.find l mem) accVal
    Set.fold folder AbsVal.bot locs

  member private __.initImportTableAux binInfo accMem rawAddr targSubrtn =
    let binName = binInfo.BinName
    let loc = Global (binName, rawAddr)
    let f = Function.ofAddr targSubrtn
    __.add loc (AbsVal.ofFunc f) accMem

  member __.initImportTable mem binInfo =
    let importMap = BinInfo.getImportMap binInfo
    Map.fold (__.initImportTableAux binInfo) mem importMap

  member __.initialize binInfo =
    if initializeMemo.ContainsKey(binInfo.BinName) then
      initializeMemo.[binInfo.BinName]
    else
      let mem = __.initImportTable __.bot binInfo
      initializeMemo.Add(binInfo.BinName, mem)
      mem

  member __.joinWithRecord record (mem1: AbsMem) (mem2: AbsMem) : AbsMem =
    // 'accNewMem': Memory to return.
    // 'accMem'': The other (counterpart) memory not being folded now.
    let folder (accNewMem, accMem') loc v =
      let v', accMem2 =
        match Map.tryFind loc accMem' with
        | Some v' -> (v', Map.remove loc accMem') // To avoid redundant work.
        | None ->
          // If the key was not found in the counterpart memory, we should look
          // into 'Record' for this location.
          if Record.hasLoc loc record
          then (AbsVal.ofSymbol (Record.findLoc loc record), accMem')
          else (AbsVal.bot, accMem')
      (Map.add loc (AbsVal.join v v') accNewMem, accMem2)
    let newMem, mem2 = Map.fold folder (__.bot, mem2) mem1
    // Note that we should also process the remaining keys in 'mem2', as well.
    Map.fold folder (newMem, mem1) mem2 |> fst

  member private __.loadAux record mem loc =
    let v = __.find loc mem
    if AbsLoc.isInvalid loc then AbsVal.bot // Caution. This is not redundant.
    elif not (AbsVal.isBot v) then v
    elif not (AbsLoc.isArgument loc) then AbsVal.unknownInt // Uninitialized.
    elif Record.hasLoc loc record then // If already encountered argument loc.
      AbsVal.ofSymbol (Record.findLoc loc record)
    else // Otherwise, spawn a symbolic value, which gets memorized in record.
      Record.spawnSymbolicVal loc record

  member __.load record mem locs: AbsVal =
    let folder accVal loc = AbsVal.join accVal (__.loadAux record mem loc)
    Set.fold folder AbsVal.bot locs

  member __.weakStore record loc v mem =
    let oldVal = __.loadAux record mem loc
    let newVal = AbsVal.join oldVal v
    __.add loc newVal mem

  member __.strongStore record loc v mem =
    // Should load even for strong store, to memorize the location in 'record'.
    // For example, if a function strong-updates a pointer argument in a
    // conditional branch, the summary should "weak" update it.
    let _ = __.loadAux record mem loc
    __.add loc v mem

  // Perform strong store only if the location set is singleton.
  member __.store record locs v mem : AbsMem =
    if Set.count locs = 1
    then __.strongStore record (Set.minElement locs) v mem
    else Set.fold (fun accMem l -> __.weakStore record l v accMem) mem locs

  member __.findSameBase loc mem =
    Map.fold (fun accLocs l _ ->
      if AbsLoc.isSameBase loc l then l :: accLocs else accLocs
    ) [] mem

let AbsMem = AbsMemModule () // Now we can use 'AbsMem' like a module.
