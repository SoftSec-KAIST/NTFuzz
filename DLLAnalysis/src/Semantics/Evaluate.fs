module DLLAnalysis.AbstractSemantics.Eval

open B2R2
open B2R2.BinIR
open B2R2.BinIR.LowUIR
open DLLAnalysis
open DLLAnalysis.Const
open DLLAnalysis.FrontEnd
open DLLAnalysis.AbstractDomain

let evalConst binInfo bitv =
  try let rawAddr = RawAddr.ofBitVector bitv
      if BinInfo.isCodeAddr binInfo rawAddr then
        AbsVal.ofFunc (Function.ofAddr (binInfo.BinName, rawAddr))
      elif BinInfo.isGlobalAddr binInfo rawAddr then
        AbsVal.ofLoc (AbsLoc.makeGlobalLoc (binInfo.BinName, rawAddr))
      else
        let width = BitVector.getBitWidth bitv
        let ui64 = BitVector.castToNUInt bitv
        AbsVal.ofUInt64 width ui64
  with Failure _ -> AbsVal.unknownInt

let rec evalBinOp localInfo op regTyp exp1 exp2 state =
  let bitWidth = NUInt.ofInt (RegType.toBitWidth regTyp)
  match op, exp1, exp2 with
  // First, handle some add-hoc cases.
  | (BinOpType.ADD, Num bv, e) | (BinOpType.ADD, e, Num bv)
  | (BinOpType.SUB, e, Num bv) when BitVector.zero regTyp = bv ->
    eval localInfo state e
  | (BinOpType.ADD, _, _) when exp1 = exp2 ->
    eval localInfo state exp1 // Evaluate and multiply by two.
    |> AbsVal.binOp bitWidth BinOpType.MUL (AbsVal.ofUInt64 bitWidth NUInt.TWO)
  | (BinOpType.MUL, Num bv, e) | (BinOpType.MUL, e, Num bv)
    when BitVector.one regTyp = bv ->
    eval localInfo state e
  | (BinOpType.AND, Num bv, _) | (BinOpType.AND, _, Num bv)
    when BitVector.zero regTyp = bv ->
    AbsVal.ofUInt64 bitWidth NUInt.ZERO
  | (BinOpType.OR, Num bv, _) | (BinOpType.OR, _, Num bv)
    when BitVector.castToNUInt bv = NUInt.ARCH_MAX_UINT ->
    AbsVal.ofUInt64 bitWidth NUInt.ARCH_MAX_UINT
  | (BinOpType.XOR, Var (_, _, r1, _), Var (_, _, r2, _)) when r1 = r2 ->
    AbsVal.ofUInt64 bitWidth NUInt.ZERO
  | (BinOpType.XOR, TempVar (_, tmp1), TempVar (_, tmp2)) when tmp1 = tmp2 ->
    AbsVal.ofUInt64 bitWidth NUInt.ZERO
  // These case should be handled as abstract location offset calculation.
  | (BinOpType.ADD, exp, Num bitv) | (BinOpType.ADD, Num bitv, exp)
  | (BinOpType.SUB, exp, Num bitv) ->
    let v = eval localInfo state exp
    let constVal = evalConst (LocalInfo.getBinInfo localInfo) bitv
    if AbsVal.isConst constVal then
      let imm = AbsInt.getConst (AbsVal.getInt constVal)
      AbsVal.binOpImmediate op v imm
    else AbsVal.binOp bitWidth op v constVal
  // Another cases that should be handled as abstract location offset
  // calculation. This can be removed when constant propagation of temporary
  // register is stabilized and checked enough.
  | (BinOpType.ADD, exp, TempVar (_, tmpRegNum))
  | (BinOpType.ADD, TempVar (_, tmpRegNum), exp)
  | (BinOpType.SUB, exp, TempVar (_, tmpRegNum))  ->
    let v = eval localInfo state exp
    let reg = Register.ofTempVar tmpRegNum
    let regVal = State.read reg state
    if AbsVal.isConst regVal then
      let imm = AbsInt.getConst (AbsVal.getInt regVal)
      AbsVal.binOpImmediate op v imm
    else AbsVal.binOp bitWidth op v regVal
  | _ ->
    let v1 = eval localInfo state exp1
    let v2 = eval localInfo state exp2
    AbsVal.binOp bitWidth op v1 v2

and eval localInfo state exp =
  match exp with
  | Num bitv -> evalConst (LocalInfo.getBinInfo localInfo) bitv
  | PCVar (_, reg) -> State.read reg state
  | Var (regTyp, _, reg, _) ->
    let bitWidth = NUInt.ofInt (RegType.toBitWidth regTyp)
    State.read reg state |> AbsVal.truncate bitWidth
  | TempVar (_, tmpRegNum) ->
    let reg = Register.ofTempVar tmpRegNum
    State.read reg state
  | Name _ -> AbsVal.bot // Ignore since we've already resolved CFG.
  | FuncName _ -> AbsVal.bot // Unsupported
  | UnOp (op, e, _, _) -> AbsVal.unOp op (eval localInfo state e)
  | BinOp (op, rTyp, e1, e2, _, _) -> evalBinOp localInfo op rTyp e1 e2 state
  | RelOp _ -> AbsVal.bot // Ignore since we have no pruning as of now.
  | Load (_, regTyp, addr, _, _) ->
    let bitWidth = NUInt.ofInt (RegType.toBitWidth regTyp)
    let footPrint = LocalInfo.getFootPrint localInfo
    let record = LocalInfo.getRecord localInfo
    let locs = AbsVal.getLoc (eval localInfo state addr)
    State.load (Some footPrint) record locs state |> AbsVal.truncate bitWidth
  | Ite (_, e1, e2, _, _) ->
    AbsVal.join (eval localInfo state e1) (eval localInfo state e2)
  | Cast (_, _, e, _, _) -> eval localInfo state e
  | Extract (e, regTyp, _, _, _) ->
    let bitWidth = NUInt.ofInt (RegType.toBitWidth regTyp)
    AbsVal.truncate bitWidth (eval localInfo state e)
  | Undefined _ ->  AbsVal.unknownInt
  | Nil _ -> AbsVal.bot // Unsupported

let rec decideSrcVar localInfo state valExp =
  match valExp with
  | PCVar (_, srcReg) | Var (_, _, srcReg, _) -> // '<dstVar> := RXX' case.
    Variable.RegVar srcReg
  | TempVar (_, regNum) -> // '<dstVar> := TMP_N' case.
    let srcReg = Register.ofTempVar regNum
    Variable.RegVar srcReg
  | Load (_, _, addr, _, _) -> // 'dstReg := load(addr)' case.
    let locs = AbsVal.getLoc (eval localInfo state addr)
    if AbsLocSet.count locs <> 1 then Variable.NoneVar
    else Variable.LocVar (AbsLocSet.getSingleton locs)
  | Extract(exp, regTyp, 0, _, _) ->
    // For '<dstVar> := RXX[N:0]' case.
    let width = NUInt.ofInt (RegType.toBitWidth regTyp)
    if width < NUInt.BYTE_WIDTH then Variable.NoneVar
    else decideSrcVar localInfo state exp
  | Cast (_, _, Extract(exp, regTyp, 0, _, _), _, _) ->
    // For '<dstVar> := ext(RXX[N:0])' case.
    let bitWidth = NUInt.ofInt (RegType.toBitWidth regTyp)
    if bitWidth < NUInt.BYTE_WIDTH then Variable.NoneVar
    else decideSrcVar localInfo state exp
  | _ -> Variable.NoneVar
