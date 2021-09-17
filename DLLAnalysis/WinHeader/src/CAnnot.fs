namespace WinHeader.C

open WinHeader.Utils
open WinHeader.Constants

exception VariableArraySizeException

type SizeExp =
  | DontKnow
  | Const of int
  | SizeOf of typName: string
  | DirectVar of varName: string
  | DerefVar of varName: string
  | Plus of SizeExp * SizeExp
  | Minus of SizeExp * SizeExp
  | Times of SizeExp * SizeExp

module SizeExp =

  let isConst = function
    | Const _ -> true
    | DontKnow | SizeOf _ | DirectVar _ | DerefVar _
    | Plus _ | Minus _ | Times _ -> false

  let rec toString = function
    | DontKnow -> "?"
    | Const i -> sprintf "%d" i
    | SizeOf s -> sprintf "sizeof(%s)" s
    | DirectVar v -> v
    | DerefVar v -> sprintf "*%s" v
    | Plus (e1, e2) -> sprintf "(%s) + (%s)" (toString e1) (toString e2)
    | Minus (e1, e2) -> sprintf "(%s) - (%s)" (toString e1) (toString e2)
    | Times (e1, e2) -> sprintf "(%s) * (%s)" (toString e1) (toString e2)

  let rec tryEval = function
    | DontKnow -> raise VariableArraySizeException
    | Const i -> uint64 i
    | SizeOf _ -> failwith "[Unimplemented] sizeof() in array field size"
    | DirectVar v ->
      if Map.containsKey v macros then Map.find v macros
      else raise VariableArraySizeException
    | DerefVar _ -> raise VariableArraySizeException
    | Plus (e1, e2) -> tryEval e1 + tryEval e2
    | Minus (e1, e2) -> tryEval e1 - tryEval e2
    | Times (e1, e2) -> tryEval e1 * tryEval e2

// Represents whether a pointer directly points to the buffer, or it is a double
// pointer to the buffer.
type BufferPtrType =
  | SinglePtr
  | DoublePtr

type SizeAnnot =
  | NoSizeAnnot
  | ByteBuffer of BufferPtrType * SizeExp
  | CountBuffer of BufferPtrType * SizeExp

module SizeAnnot =
  let isConst = function
    | NoSizeAnnot -> false
    | ByteBuffer (_, exp) | CountBuffer (_, exp) -> SizeExp.isConst exp

type IOAnnot =
  | NoIOAnnot
  | InArg
  | OutArg
  | InOutArg
