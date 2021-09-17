module DLLAnalysis.AbstractSemantics.Assign

open B2R2
open B2R2.BinIR
open B2R2.BinIR.LowUIR
open DLLAnalysis
open DLLAnalysis.Utils
open DLLAnalysis.AbstractDomain
open DLLAnalysis.AbstractSemantics.Eval

let private findCondFromAndRegs reg state =
  match State.tryFindCondition reg state with
  | Some (PruningCond.And vars) ->
    // E.g., 'AND R0, R0; MOV R1, R0; AND R1, R1' case.
    let eqRegs = State.getRegsEqualToReg reg state |> Set.add reg
    let eqLocs = State.getLocsEqualToReg reg state
    Some (PruningCond.makeAnd eqRegs eqLocs vars)
  | Some (PruningCond.AndWithMask vars) ->
    // E.g., 'AND R0, 0xFFFF...; MOV R1, R0; AND R1, R1' case.
    let eqRegs = State.getRegsEqualToReg reg state |> Set.add reg
    let eqLocs = State.getLocsEqualToReg reg state
    Some (PruningCond.makeMaskedAnd eqRegs eqLocs vars)
  | Some (PruningCond.EqZero _) | Some (PruningCond.MaskedEqZero _) | None ->
    // Other plain 'And R0, R0' cases.
    let eqRegs = State.getRegsEqualToReg reg state |> Set.add reg
    let eqLocs = State.getLocsEqualToReg reg state
    Some (PruningCond.makeAnd eqRegs eqLocs PruningCond.emptyVars)

let private findCondFromAndRegWithMask reg state =
  let eqRegs = State.getRegsEqualToReg reg state |> Set.add reg
  let eqLocs = State.getLocsEqualToReg reg state
  Some (PruningCond.makeMaskedAnd eqRegs eqLocs PruningCond.emptyVars)

let private findCondFromEQRegZero reg state =
  match State.tryFindCondition reg state with
  | None -> None
  | Some pruningCond -> PruningCond.tryMakeEqZero pruningCond

let private tryFindCondition valExp state =
  match valExp with
  // 'MOV R1, R2' case, which can propagate condition.
  | Var (_, _, reg, _) -> State.tryFindCondition reg state
  | TempVar (_, regNo) ->
    let reg = Register.ofTempVar regNo
    State.tryFindCondition reg state
  // 'AND R1, R2' case.
  | BinOp (BinOpType.AND, _, Var (_, _, r1, _), Var (_, _, r2, _), _, _) ->
    if r1 = r2 then // 'AND R0, R0' case.
      findCondFromAndRegs r1 state
    elif AbsVal.isAndMask (State.read r1 state) then
      // 'MOV R1, 0xffff...; AND R1, R2' case.
      findCondFromAndRegWithMask r2 state
    elif AbsVal.isAndMask (State.read r2 state) then
      // 'MOV R2, 0xffff...; AND R1, R2' case.
      findCondFromAndRegWithMask r1 state
    else None
  | BinOp (BinOpType.AND, _, TempVar (_, num1) , TempVar (_, num2), _, _) ->
    if num1 = num2 // 'AND TMP_0, TMP_0' case.
    then findCondFromAndRegs (Register.ofTempVar num1) state
    else None
  // 'AND R0, 0xffff...' or 'AND 0xffff..., R0' case.
  | BinOp (BinOpType.AND, _, Var (_, _, r, _), Num bitv, _, _)
  | BinOp (BinOpType.AND, _, Num bitv, Var (_, _, r, _), _, _) ->
    if NUInt.isAndMask (BitVector.castToNUInt bitv)
    then findCondFromAndRegWithMask r state
    else None
  | BinOp (BinOpType.AND, _, TempVar (_, num), Num bitv, _, _)
  | BinOp (BinOpType.AND, _, Num bitv, TempVar (_, num), _, _) ->
    if NUInt.isAndMask (BitVector.castToNUInt bitv)
    then findCondFromAndRegWithMask (Register.ofTempVar num) state
    else None
  // 'EQ R0, 0' or 'EQ 0, R0' case.
  | RelOp (RelOpType.EQ, Var (_, _, reg, _), Num bitv, _, _)
  | RelOp (RelOpType.EQ, Num bitv, Var (_, _, reg, _), _, _) ->
    if BitVector.isZero bitv then findCondFromEQRegZero reg state else None
  | RelOp (RelOpType.EQ, TempVar (_, regNum), Num bitv, _, _)
  | RelOp (RelOpType.EQ, Num bitv, TempVar (_, regNum), _, _) ->
    if BitVector.isZero bitv
    then findCondFromEQRegZero (Register.ofTempVar regNum) state
    else None
  | _ -> None

let assignCondition localInfo regExp valExp state =
  let condOpt = tryFindCondition valExp state
  match regExp with
  | Var (_, _, reg, _) ->
    State.addCondition reg condOpt state
  | TempVar (_, regNum) ->
    let reg = Register.ofTempVar regNum
    State.addCondition reg condOpt state
  | _ -> state

let assignValue localInfo regExp valExp state =
  match regExp with
  | Var (_, _, reg, _) when Register.isFlag reg -> state // ignore flags.
  | PCVar (_, reg) | Var (_, _, reg, _) ->
    let value = eval localInfo state valExp
    let srcVar = decideSrcVar localInfo state valExp
    State.assign reg value srcVar state
  | TempVar (_, regNum) ->
    let reg = Register.ofTempVar regNum
    let value = eval localInfo state valExp
    let srcVar = decideSrcVar localInfo state valExp
    State.assign reg value srcVar state
  | _ -> failwith "Invalid assignment form"
