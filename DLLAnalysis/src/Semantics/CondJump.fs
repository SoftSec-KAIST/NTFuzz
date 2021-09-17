module DLLAnalysis.AbstractSemantics.CondJump

open B2R2
open B2R2.BinIR
open B2R2.BinIR.LowUIR
open DLLAnalysis
open DLLAnalysis.AbstractDomain

let private pruneRegsWithEq regs state =
  let folder acc reg = State.assign reg AbsVal.zero Variable.NoneVar acc
  Set.fold folder state regs

let private pruneLocWithEq locs state =
  let folder accState loc =
    State.getAbsMem accState
    |> AbsMem.add loc AbsVal.zero
    |> State.setAbsMem accState
  Set.fold folder state locs

let private pruneEq vars state =
  let regs = PruningCond.getRegs vars
  let locs = PruningCond.getLocs vars
  pruneRegsWithEq regs state |> pruneLocWithEq locs

let private pruneRegsWithMaskedEq regs state =
  let folder accState reg =
    let newVal = State.read reg accState
                 |> AbsVal.setLoc AbsLocSet.bot
                 |> AbsVal.setFunc FunctionSet.bot
                 |> AbsVal.setConstr TypeConstr.bot
    State.assign reg newVal Variable.NoneVar accState
  Set.fold folder state regs

let private pruneLocsWithMaskedEq locs state =
  let folder accState loc =
    let absMem = State.getAbsMem accState
    let newVal = AbsMem.find loc absMem
                 |> AbsVal.setLoc AbsLocSet.bot
                 |> AbsVal.setFunc FunctionSet.bot
                 |> AbsVal.setConstr TypeConstr.bot
    AbsMem.add loc newVal absMem
    |> State.setAbsMem accState
  Set.fold folder state locs

let private pruneMaskedEq vars state =
  let regs = PruningCond.getRegs vars
  let locs = PruningCond.getLocs vars
  pruneRegsWithMaskedEq regs state |> pruneLocsWithMaskedEq locs

let private condJumpAux reg state jmpIfTrue =
  let trueState, falseState =
    match State.tryFindCondition reg state with
    | Some (PruningCond.EqZero vars) -> (pruneEq vars state, state)
    | Some (PruningCond.MaskedEqZero vars) -> (pruneMaskedEq vars state, state)
    | Some _ | None -> (state, state)
  if jmpIfTrue then (trueState, falseState) else (falseState, trueState)

let condJump condExp state =
  match condExp with
  | Var (_, _, reg, _) -> condJumpAux reg state true
  | RelOp (RelOpType.EQ, Var (_, _, reg, _), Num bitv, _, _)
  | RelOp (RelOpType.EQ, Num bitv, Var (_, _, reg, _), _, _) ->
    if BitVector.isZero bitv then condJumpAux reg state false
    else (state, state)
  | _ -> (state, state)
