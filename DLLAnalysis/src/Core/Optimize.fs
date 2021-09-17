module DLLAnalysis.Optimize

open B2R2
open B2R2.BinIR
open B2R2.BinIR.LowUIR

// TODO : Move to B2R2
let rec constFoldExp exp =
  match exp with
  // Base cases
  | Num _ | Var _ | PCVar _ | TempVar _
  | RelOp _ | Name _ | Ite _ | Cast _ | Undefined _ -> exp
  // Unary constant folding.
  | UnOp (UnOpType.NEG, Num bv, _, _) -> Num (BitVector.neg bv)
  | UnOp (UnOpType.NOT, Num bv, _, _) -> Num (BitVector.bnot bv)
  // Handle special constants that can be absorbed.
  | BinOp (BinOpType.ADD, regType, Num bv, e, _, _)
  | BinOp (BinOpType.ADD, regType, e, Num bv, _, _)
    when bv = BitVector.zero regType -> e
  | BinOp (BinOpType.SUB, regType, e, Num bv, _, _)
    when bv = BitVector.zero regType -> e
  | BinOp (BinOpType.MUL, regType, Num bv, _, _, _)
  | BinOp (BinOpType.MUL, regType, _, Num bv, _, _)
    when bv = BitVector.zero regType -> Num (BitVector.zero regType)
  | BinOp (BinOpType.MUL, regType, Num bv, e, _, _)
  | BinOp (BinOpType.MUL, regType, e, Num bv, _, _)
    when bv = BitVector.one regType -> e
  // Binary constant folding.
  | BinOp (BinOpType.ADD, _, Num bv1, Num bv2, _, _) ->
    Num (BitVector.add bv1 bv2)
  | BinOp (BinOpType.SUB, _, Num bv1, Num bv2, _, _) ->
    Num (BitVector.sub bv1 bv2)
  | BinOp (BinOpType.SHL, _, Num bv1, Num bv2, _, _) ->
    Num (BitVector.shl bv1 bv2)
  | BinOp (BinOpType.SHR, _, Num bv1, Num bv2, _, _) ->
    Num (BitVector.shr bv1 bv2)
  | BinOp (BinOpType.AND, _, Num bv1, Num bv2, _, _) ->
    Num (BitVector.band bv1 bv2)
  | BinOp (BinOpType.OR, _, Num bv1, Num bv2, _, _) ->
    Num (BitVector.bor bv1 bv2)
  | BinOp (BinOpType.XOR, _, Num bv1, Num bv2, _, _) ->
    Num (BitVector.bxor bv1 bv2)
  | BinOp (BinOpType.DIV, _, Num bv1, Num bv2, _, _) ->
    Num (BitVector.div bv1 bv2)
  | BinOp (BinOpType.SDIV, _, Num bv1, Num bv2, _, _) ->
    Num (BitVector.sdiv bv1 bv2)
  // Recursive cases.
  | UnOp (op, e, info, cons) ->
    let e' = constFoldExp e
    if e = e' then exp else constFoldExp (UnOp (op, e', info, cons))
  | BinOp (op, regType, e1, e2, info, cons) ->
    let e1', e2' = constFoldExp e1, constFoldExp e2
    if e1 = e1' && e2 = e2' then exp
    else constFoldExp (BinOp (op, regType, e1', e2', info, cons))
  | Load (endian, retTyp, e, info, cons) ->
    Load (endian, retTyp, constFoldExp e, info, cons)
  | Extract _ | FuncName _ | Nil _ -> exp

let constFold stmt =
  match stmt with
  | ISMark _ | IEMark _ | LMark _ | SideEffect _ -> stmt
  | Put (regExp, valExp) -> Put (regExp, constFoldExp valExp)
  | Store (endian, addrExp, valExp) ->
    Store (endian, constFoldExp addrExp, constFoldExp valExp)
  | InterJmp (pcExp, dstExp, jmpInfo) ->
    InterJmp (constFoldExp pcExp, constFoldExp dstExp, jmpInfo)
  | Jmp _ | CJmp _ | InterCJmp _ -> stmt
