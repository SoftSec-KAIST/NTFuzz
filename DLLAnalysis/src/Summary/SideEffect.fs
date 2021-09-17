namespace DLLAnalysis.Summary

open DLLAnalysis
open DLLAnalysis.Const
open DLLAnalysis.Utils
open DLLAnalysis.AbstractDomain

/// Side effect of executing a subroutine. Note that 'Updates' can naturally
/// distinguish strong update and weak update, since in the case of weak update,
/// 'AbsVal' of the tuple will contain the original symbolic value.
type SideEffect = {
  Abort : bool
  RetVal : AbsVal
  StackLift : Offset option
  Updates : (AbsLocSet * AbsVal) array
}

module SideEffect =

  let abort =
    { Abort = true
      RetVal = AbsVal.unknownInt
      StackLift = None
      Updates = [||] }

  let unknown =
    { Abort = false
      RetVal = AbsVal.unknownInt
      StackLift = None
      Updates = [||] }

  let toString sideEffect =
    if sideEffect.Abort then "No-return (aborting) function"
    else
      let updateToStr (locs, v) =
        sprintf "%s |-> %s" (AbsLocSet.toString locs) (AbsVal.toString v)
      let updates = Array.sortBy (AbsLocSet.toString << fst) sideEffect.Updates
      let updateStr =
        Array.map updateToStr updates
        |> String.concat "\n"
        |> sprintf "(Memory update)\n%s"
      let stackLiftStr =
        match sideEffect.StackLift with
        | None ->"(Stack lift)\nUnknown"
        | Some delta -> sprintf "(Stack lift)\n%d" delta
      let retStr = sprintf "(RetVal)\n%s" (AbsVal.toString sideEffect.RetVal)
      updateStr + "\n\n" + stackLiftStr + "\n\n" + retStr

  let rec private collectArgLocs record retMem depth accLocs loc =
    if depth >= STRUCT_DEPTH_LIMIT then accLocs
    elif not (Record.hasLoc loc record) then Set.add loc accLocs
    else
      let symbol = Record.findLoc loc record |> Symbol.appendLocSuffix
      let isChild l = // Check child (i.e. location derived from 'loc'.)
        AbsLoc.isSymbolic l && AbsLoc.getSymbol l = symbol
      let childLocs = Map.keys retMem |> List.filter isChild
      let depth = depth + 1
      let accLocs = Set.add loc accLocs
      List.fold (collectArgLocs record retMem depth) accLocs childLocs

  let private checkArgLoc record initMem retMem accUpdates l =
    // Note that stack location can be ignored when calculating side effect.
    if (AbsLoc.isSymbolic l) && Record.hasLoc l record then
      // Means the location was first loaded and thereby initialized in record.
      // Should use 'load' to fetch the returned value.
      let v = AbsMem.load record retMem (AbsLocSet.make [l])
      let initV = AbsVal.ofSymbol (Record.findLoc l record)
      // Prevent updating caller's memory with current stack frame location.
      let update = (Set.singleton l, AbsVal.removeStackLoc v)
      if initV = v then accUpdates else Array.add update accUpdates
    elif AbsLoc.isSymbolic l then
      // In this case, the location was (strong-) updated w/o load. Here, we
      // should use 'read' to fetch the returned value.
      let v = AbsMem.find l retMem
      let initV = AbsMem.find l initMem
      // Prevent updating caller's memory with current stack frame location.
      let update = (Set.singleton l, AbsVal.removeStackLoc v)
      if v = initV then accUpdates else Array.add update accUpdates
    else accUpdates

  let private findArgUpdates record initMem retMem =
    Record.getLocs record
    |> List.filter AbsLoc.isRootArgument
    |> List.fold (collectArgLocs record retMem 0) Set.empty
    |> Set.fold (checkArgLoc record initMem retMem) [||]

  let private getHeapBases absVal =
    AbsVal.getLoc absVal
    |> AbsLocSet.filter AbsLoc.isHeap
    |> AbsLocSet.map AbsLoc.getBase

  // Find updates on heap locations that will become reachable from caller.
  let private findHeapUpdates retVal argUpdates retMem =
    let retHeapBases = getHeapBases retVal
    // Caution: locations in 'update value', not 'updated location'.
    let updateHeapBases = Array.map (fun (_, v) -> getHeapBases v) argUpdates
                          |> Array.fold AbsLocSet.join AbsLocSet.bot
    let reachableBases = AbsLocSet.join retHeapBases updateHeapBases
    let isReachable l = AbsLoc.isHeap l &&
                        Set.contains (AbsLoc.getBase l) reachableBases
    let reachableHeapLocs = List.filter isReachable (Map.keys retMem)
    let folder acc l = Array.add (Set.singleton l, AbsMem.find l retMem) acc
    List.fold folder [||] reachableHeapLocs

  let private decideRetVal retState =
    State.getReturnValue retState
    // Prevent returning current stack frame locations.
    |> AbsVal.removeStackLoc

  // Note that it's dangerous to rely on Subroutine.tryFindStackLiftWithName()
  // when we fail to calculate the stack delta (name suffix often give incorrect
  // information). So just leave it as 'None'.
  let private decideStackLift subrtn retState =
    let subrtnStr = Subroutine.toString subrtn
    let initSP = AbsLoc.makeStackLoc subrtn Offset.ZERO
    let retSPs = State.getStackPtrLoc retState
    if State.isStackPtrValid retState then
      let retSP = AbsLocSet.getSingleton retSPs
      let delta = AbsLoc.findDistance retSP initSP
      if delta > Offset.ZERO then Some delta else None
    else None

  let private findAux localInfo retState =
    let initMem = AbsMem.bot
    let record = LocalInfo.getRecord localInfo
    let subrtn = LocalInfo.getSubrtn localInfo
    let retVal = decideRetVal retState
    let delta = decideStackLift subrtn retState
    let retMem = State.getAbsMem retState
    let argUpdates = findArgUpdates record initMem retMem
    let argUpdates = if SIDE_EFFECT_LIMIT < 0 then argUpdates
                     elif SIDE_EFFECT_LIMIT = 0 then [||]
                     elif argUpdates.Length <= SIDE_EFFECT_LIMIT then argUpdates
                     else argUpdates.[..(SIDE_EFFECT_LIMIT - 1)]
    let remainCount = SIDE_EFFECT_LIMIT - argUpdates.Length
    let heapUpdates = findHeapUpdates retVal argUpdates retMem
    let heapUpdates = if SIDE_EFFECT_LIMIT < 0 then heapUpdates
                      elif remainCount = 0 then [||]
                      elif heapUpdates.Length <= remainCount then heapUpdates
                      else heapUpdates.[..(remainCount - 1)]
    let updates = Array.append argUpdates heapUpdates
    { Abort = false; RetVal = retVal; StackLift = delta; Updates = updates }

  let find localInfo retState =
    if State.isBot retState then abort // If no return state found.
    else findAux localInfo retState

  let private applyStackLift localInfo bb stackLift state =
    let binName = LocalInfo.getBinName localInfo
    match stackLift with
    | None -> // x86
      if BasicBlock.nextBBClearsStack binName bb
      then State.addStack Offset.WORD_SIZE state // Rely on caller-side clear.
      else State.restoreStackPtr state // Rely on stack pointer backup.
    | Some delta -> // Use summarized side effect. We can also update backup.
      State.addStack delta state |> State.backupStackPtr

  let private applyUpdate localInfo accState (locs, v) =
    let record = LocalInfo.getRecord localInfo
    let footPrint = Some (LocalInfo.getFootPrint localInfo)
    let nLocs = AbsLocSet.count locs
    if nLocs > READ_WRITE_NLOC_LIMIT then accState
    else State.store footPrint record Variable.NoneVar locs v accState

  let apply localInfo bb sideEffect state =
    if sideEffect.Abort then raise ExecutionHaltException
    else let retVal = sideEffect.RetVal
         let state = State.setRetVal retVal state
         let state = applyStackLift localInfo bb sideEffect.StackLift state
         Array.fold (applyUpdate localInfo) state sideEffect.Updates

  let private instantiateUpdate subst (locs, v) =
    let intSubst = subst.IntSubst
    let locSubst = subst.LocSubst
    (AbsLocSet.substitute locs intSubst locSubst, Substitution.apply subst v)

  let instantiate localInfo sideEffect subst =
    let substRetVal = Substitution.apply subst sideEffect.RetVal
    let updates = sideEffect.Updates
    let substUpdates = Array.map (instantiateUpdate subst) updates
    { sideEffect with RetVal = substRetVal; Updates = substUpdates }

  /// Collect symbols used in side effect.
  let collectSymbols sideEffect =
    let retValSyms = AbsVal.collectSymbols sideEffect.RetVal
    let folder accSet (locs, updateVal) =
      let locSyms = AbsLocSet.collectSymbols locs
      let valSyms = AbsVal.collectSymbols updateVal
      Set.union (Set.union locSyms valSyms) accSet
    let sideEffectSyms = Array.fold folder Set.empty sideEffect.Updates
    Set.union sideEffectSyms retValSyms
