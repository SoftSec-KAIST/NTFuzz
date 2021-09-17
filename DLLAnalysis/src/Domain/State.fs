module DLLAnalysis.AbsDom.State

open DLLAnalysis
open DLLAnalysis.Const
open DLLAnalysis.Utils
open DLLAnalysis.AbsDom.Functor
open DLLAnalysis.AbsDom.PPoint
open DLLAnalysis.AbsDom.AbsLoc
open DLLAnalysis.AbsDom.TypeConstr
open DLLAnalysis.AbsDom.AbsVal
open DLLAnalysis.AbsDom.Register
open DLLAnalysis.AbsDom.RegMap
open DLLAnalysis.AbsDom.AbsMem
open DLLAnalysis.DataFlow
open DLLAnalysis.AbsDom.Relation
open DLLAnalysis.AbsDom.Pruning

type Elem1 = RegMap * AbsMem
type Elem2 = EQRelation * PruningMap
type State = Elem1 * Elem2

let Dom1 = Prod2Domain<RegMap,AbsMem>(RegMap, AbsMem)
let Dom2 = Prod2Domain<EQRelation,PruningMap>(EQRelation, PruningMap)

type StateModule () =
  inherit Prod2Domain<Elem1,Elem2>(Dom1, Dom2)

  // Override stringfy function for readability.
  override __.toString state =
    let regMap = __.getRegMap state
    let absMem = __.getAbsMem state
    let regMapStr = RegMap.toString regMap
    let memStr = AbsMem.toString absMem
    "=== Register Map ===\n" + regMapStr + "\n=== Memory ===\n" + memStr

  override __.leq state1 state2 =
    // Has to consider 'record' for leq, but in our current design (that unrolls
    // loop), we don't need LEQ operation to find fixpoint.
    failwith "Abstract state leq() is unimplemented"

  override __.join state1 state2 =
    failwith "Abstract state join should use State.joinWithRecord() instead"

  member __.joinWithRecord record (state1: State) (state2: State) : State =
    let regMap1, regMap2 = __.getRegMap state1, __.getRegMap state2
    let absMem1, absMem2 = __.getAbsMem state1, __.getAbsMem state2
    let eqRel1, eqRel2 = __.getEQRel state1, __.getEQRel state2
    let pruneMap1, pruneMap2 = __.getPruneMap state1, __.getPruneMap state2
    let newRegMap = RegMap.join regMap1 regMap2
    let newAbsMem = AbsMem.joinWithRecord record absMem1 absMem2
    let newEQRel = EQRelation.join eqRel1 eqRel2
    let newPruneMap = PruningMap.join pruneMap1 pruneMap2
    let newState = ((newRegMap, newAbsMem), (newEQRel,newPruneMap))
    // Now refine stack pointer value with hint flags.
    let stackPtr1 = __.read Register.stackPtr state1
    let stackPtr2 = __.read Register.stackPtr state2
    let stackImprFlag1 = __.read Register.stackImpreciseFlag state1
    let stackImprFlag2 = __.read Register.stackImpreciseFlag state2
    // If stack pointer did not diverge, then leave as it is.
    if AbsVal.leq stackPtr1 stackPtr2 || AbsVal.leq stackPtr2 stackPtr1 then
      newState
    // If only one side has imprecise stack pointer, rely on the other side.
    elif AbsVal.isBot stackImprFlag1 && not (AbsVal.isBot stackImprFlag2) then
      __.setStackPtr stackPtr1 newState
      |> __.assign Register.stackImpreciseFlag AbsVal.bot NoneVar
    elif not (AbsVal.isBot stackImprFlag1) && AbsVal.isBot stackImprFlag2 then
      __.setStackPtr stackPtr2 newState
      |> __.assign Register.stackImpreciseFlag AbsVal.bot NoneVar
    // Otherwise, let the stack pointer diverge and handle error later.
    else newState

  member __.multipleJoinWithRecord record states =
    match states with
    | [] -> failwith "Empty state list"
    | headState :: tailStates ->
      List.fold (__.joinWithRecord record) headState tailStates

  (* Low-level getter functions. *)

  member __.getRegMap (x: State) : RegMap = fst (fst x)
  member __.getAbsMem (x: State) : AbsMem = snd (fst x)
  member __.getEQRel (x: State) : EQRelation = fst (snd x)
  member __.getPruneMap (x: State) : PruningMap = snd (snd x)

  (* Low-level setter functions. *)

  member __.setRegMap (x: State) regMap : State =
    let (_, absMem), (eqRel, prMap) = x
    (regMap, absMem), (eqRel, prMap)

  member __.setAbsMem (x: State) absMem : State =
    let (regMap, _), (eqRel, prMap) = x
    (regMap, absMem), (eqRel, prMap)

  member __.setEQRel (x: State) eqRel : State =
    let (regMap, absMem), (_, prMap) = x
    (regMap, absMem), (eqRel, prMap)

  member __.setPruneMap (x: State) prMap : State =
    let (regMap, absMem), (eqRel, _) = x
    (regMap, absMem), (eqRel, prMap)

  (* High-level getter functions for abstract semantics. *)

  member __.read reg x: AbsVal =
    let regMap = __.getRegMap x
    RegMap.find reg regMap

  member __.load footPrintOpt record locs x =
    // First, update footprint if provided.
    match footPrintOpt with
    | None -> ()
    | Some footPrint -> FootPrint.addLoad (__.getPC x) locs footPrint
    // Now, read the value from memory.
    let absMem = __.getAbsMem x
    let v = AbsMem.load record absMem locs
    let constr = AbsVal.getConstr v
    AbsVal.setConstr (TypeConstr.removeSizeHint constr) v

  member __.getStackPtrVal x =
    // No update on def/use set.
    __.read Register.stackPtr x

  member __.getStackPtrLoc x =
    // No update on def/use set.
    __.getStackPtrVal x |> AbsVal.getLoc

  member __.getReturnValue x =
    // No update on def/use set.
    __.read Register.ret x

  member __.getPC x =
    // No update on def/use set.
    let pcVal = __.read Register.pgmCounter x
    let pcSet = AbsVal.getFunc pcVal
    if Set.count pcSet <> 1 then failwith "Non-singleton PC"
    match Set.minElement pcSet with
    | Function.PC ppoint -> ppoint
    | Function.Static _ | Function.SymCode _ ->
      failwith "Invalid function value for PC"

  member __.getPCAddr x =
    PPoint.getAddr (__.getPC x)

  member __.isStackPtrValid x =
    let stackPtrs = __.getStackPtrLoc x
    let count = AbsLocSet.count stackPtrs
    count = 1 && AbsLocSet.forall (AbsLoc.isStack) stackPtrs

  member __.getArgument footPrintOpt record argIdx state =
    if argIdx < NUMREGARG then // Register argument.
      let argReg = Register.args.[argIdx]
      __.read argReg state
    else // Stack argument.
      let offset = Offset.WORD_SIZE * (Offset.ofInt (argIdx + 1))
      let stackPtrs = __.getStackPtrLoc state
      if not (__.isStackPtrValid state) then
        let pcStr = PPoint.toString (__.getPC state)
        logWarning "Invalid stack pointer @ getArgument(%s)" pcStr
        raise InvalidStackPtrException
      let argLoc = AbsLocSet.addOffset stackPtrs offset
      __.load footPrintOpt record argLoc state

  member __.getArguments footPrintOpt record state argNum =
    let mapper argIdx = __.getArgument footPrintOpt record argIdx state
    let ranges = List.ofSeq (seq { for i in 0 .. (argNum - 1) do yield i})
    List.map mapper ranges

  member __.getRegsEqualToReg reg state =
    let eqRel = __.getEQRel state
    EQRelation.getRegsEqualToReg reg eqRel

  member __.getLocsEqualToReg reg state =
    let eqRel = __.getEQRel state
    EQRelation.getLocsEqualToReg reg eqRel

  member __.getRegsEqualToArg argIdx state =
    let eqRel = __.getEQRel state
    if argIdx < NUMREGARG then // Register argument.
      let argReg = Register.args.[argIdx]
      EQRelation.getRegsEqualToReg argReg eqRel
    else // Stack argument
      let offset = Offset.WORD_SIZE * (Offset.ofInt (argIdx + 1))
      let stackPtrs = __.getStackPtrLoc state
      if AbsLocSet.count stackPtrs = 1 then
        let stackPtrLoc = AbsLocSet.getSingleton stackPtrs
        let argLoc = AbsLoc.addOffset stackPtrLoc offset
        EQRelation.getRegsEqualToLoc argLoc eqRel
      else Set.empty

  member __.getLocsEqualToArg argIdx state =
    let eqRel = __.getEQRel state
    if argIdx < NUMREGARG then // Register argument.
      let argReg = Register.args.[argIdx]
      EQRelation.getLocsEqualToReg argReg eqRel
    else // Stack argument
      let offset = Offset.WORD_SIZE * (Offset.ofInt (argIdx + 1))
      let stackPtrs = __.getStackPtrLoc state
      if AbsLocSet.count stackPtrs = 1 then
        let stackPtrLoc = AbsLocSet.getSingleton stackPtrs
        let argLoc = AbsLoc.addOffset stackPtrLoc offset
        EQRelation.getLocsEqualToLoc argLoc eqRel
      else Set.empty

  (* High-level update functions for abstract semantics. *)

  member __.store footPrintOpt record srcVar dstLocs value state =
    // First, update footprint if provided.
    match footPrintOpt with
    | None -> ()
    | Some footPrint -> FootPrint.addStore (__.getPC state) dstLocs footPrint
    // Now, update the memory state itself.
    let mem = __.getAbsMem state
    let newMem = AbsMem.store record dstLocs value mem
    let newState = __.setAbsMem state newMem
    // Finally, update EQ relation.
    match srcVar with
    | NopVar -> newState
    | RegVar _ | LocVar _ when AbsLocSet.count dstLocs = 1 ->
      let dstLoc = AbsLocSet.getSingleton dstLocs
      let newEQRel = EQRelation.updateLoc dstLoc srcVar (__.getEQRel state)
      __.setEQRel newState newEQRel
    | RegVar _ | LocVar _ | NoneVar ->
      let newEQRel = EQRelation.cleanUpLocs dstLocs (__.getEQRel state)
      __.setEQRel newState newEQRel

  member __.assign dstReg value srcVar state =
    // Now, update the register map itself.
    let regMap = __.getRegMap state
    let newRegMap = RegMap.add dstReg value regMap
    let newState =__.setRegMap state newRegMap
    // Finally, update the EQ relation.
    match srcVar with
    | NopVar -> newState // No side-effect on EQ relation.
    | NoneVar ->
      let newEQRel = EQRelation.cleanUpReg dstReg (__.getEQRel state)
      __.setEQRel newState newEQRel
    | RegVar _ | LocVar _ ->
      let newEQRel = EQRelation.assignReg dstReg srcVar (__.getEQRel state)
      __.setEQRel newState newEQRel

  // Backup stack pointer value.
  member __.backupStackPtr x =
    let stackPtrVal = __.read Register.stackPtr x
    let stackPtrLocs = AbsVal.getLoc stackPtrVal
    if AbsLocSet.count stackPtrLocs <> 1 then failwith "Failed to backup SP"
    __.assign Register.stackImpreciseFlag AbsVal.bot NoneVar x
    |> __.assign Register.ghostStackPtr stackPtrVal NoneVar

  // Restores the stack pointer register with the backup value.
  member __.restoreStackPtr x =
    let backupPtrVal = __.read Register.ghostStackPtr x
    // Mark that stack pointer can be imprecise from now on.
    let x = __.assign Register.stackImpreciseFlag AbsVal.unknownInt NoneVar x
    if AbsVal.isBot backupPtrVal then x
    else __.assign Register.stackPtr backupPtrVal NoneVar x

  member __.setPC x ppoint =
    let pcVal = AbsVal.ofFunc (Function.PC ppoint)
    // No update on def/use set.
    __.assign Register.pgmCounter pcVal NoneVar x

  member __.incrPC x =
    // No update on def/use set.
    let pcVal = __.read Register.pgmCounter x
    let pcSet = AbsVal.getFunc pcVal
    if Set.count pcSet <> 1 then failwith "Non-singleton PC"
    match Set.minElement pcSet with
    | Function.PC ppoint -> __.setPC x (PPoint.incr ppoint)
    | Function.Static _ | Function.SymCode _ ->
      failwith "Invalid function value for PC"

  member __.setRetVal retVal state =
    let retReg = Register.ret
    // Update RAX register, and clean up EQ relations with RAX register.
    __.assign retReg retVal NoneVar state

  member __.setStackPtr stackPtrVal state =
    let stackPtrReg = Register.stackPtr
    // Update RSP register, and clean up EQ relations with RSP register.
    __.assign stackPtrReg stackPtrVal NoneVar state

  member __.addStack offset state =
    // No update on def/use set.
    let stackPtr = __.read Register.stackPtr state
    let newStackPtr = AbsVal.addImmediate stackPtr (Offset.toNUInt offset)
    __.setStackPtr newStackPtr state

  member __.addCondition reg condOpt state =
    let pruneMap = __.getPruneMap state
    let newPruneMap = PruningMap.add reg condOpt pruneMap
    __.setPruneMap state newPruneMap

  member __.tryFindCondition reg state =
    let pruneMap = __.getPruneMap state
    PruningMap.tryFind reg pruneMap

  member private __.addSizeHintAux sizeHint accState loc =
    // To avoid spawning unnecessary symbolic values, maniuplate abstract memory
    // directly, without using load() or store().
    let mem = __.getAbsMem accState
    let curVal = AbsMem.find loc mem
    let curConstr = AbsVal.getConstr curVal
    let newConstr = TypeConstr.addSizeHint sizeHint curConstr
    let newVal = AbsVal.setConstr newConstr curVal
    let newMem = AbsMem.add loc newVal mem
    __.setAbsMem accState newMem

  member __.addSizeHint (locs: AbsLocSet) sizeHint state: State =
    Set.fold (__.addSizeHintAux sizeHint) state locs

  member __.tryGetSizeHint loc state =
    let mem = __.getAbsMem state
    let v = AbsMem.find loc mem
    let constr = AbsVal.getConstr v
    TypeConstr.tryGetSizeHint constr

  (* Constructor function. *)

  member __.initialize binInfo subroutine : State =
    let initEspVal = AbsLoc.makeStackLoc subroutine Offset.ZERO |> AbsVal.ofLoc
    let initRegMap = RegMap.initialize ()
    let initMem = AbsMem.initialize binInfo
    let state = ((initRegMap, initMem), (EQRelation.empty, PruningMap.empty))
    __.setStackPtr initEspVal state

let State = StateModule () // Now we can use 'State' like a module.

type StateMap = Map<PPoint,State>
type StateMapModule () = inherit FunDomain<PPoint,State>(PPoint,State)
let StateMap = StateMapModule () // Now we can use 'StateMap' like a module.
