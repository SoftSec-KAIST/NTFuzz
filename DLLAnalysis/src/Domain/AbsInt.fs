module DLLAnalysis.AbsDom.AbsInt

open DLLAnalysis
open DLLAnalysis.Utils
open DLLAnalysis.AbsDom.Signature

exception NotConstException
exception NotSymbolicException
exception NotSingularException

type Coeff = NUInt
type Const = NUInt
type LinearExp =
  | Bot
  | Top
  | ConcInt of NUInt
  | SymInt of (Coeff * Symbol * Const)

module LinearExp =

  let leq x y =
    match x, y with
    | Bot, _ | _, Top -> true
    | _, Bot | Top, _ -> false
    | ConcInt i1, ConcInt i2 -> i1 = i2
    | SymInt (a1, s1, b1), SymInt (a2, s2, b2) -> a1 = a2 && s1 = s2 && b1 = b2
    | ConcInt i, SymInt(a, _, b) | SymInt(a, _, b), ConcInt i ->
      a = NUInt.ZERO && i = b

  let rec join x y =
    match x, y with
    | Bot, _ | _, Top -> y
    | _, Bot | Top, _ -> x
    | ConcInt i1, ConcInt i2 -> if i1 = i2  then ConcInt i1 else Top
    | SymInt (a1, s1, b1), SymInt (a2, s2, b2) ->
      if a1 = a2 && s1 = s2 && b1 = b2 then x else Top
    | ConcInt i, SymInt(a, _, b) | SymInt(a, _, b), ConcInt i ->
      if a = NUInt.ZERO  then join (ConcInt i) (ConcInt b) else Top

  let toString = function
    | Bot -> "_"
    | Top -> "T"
    | ConcInt i -> sprintf "0x%x" i
    | SymInt (a, s, b) ->
      if a = NUInt.ZERO then sprintf "0x%x" b
      elif a = NUInt.ONE && b = NUInt.ZERO then s
      elif a = NUInt.ONE then sprintf "%s + 0x%x" s b
      elif b = NUInt.ZERO then sprintf "0x%x * %s" a s
      else sprintf "0x%x * %s + 0x%x"  a s b

  // Checkers.

  let isBot = function
    | Bot -> true
    | Top | ConcInt _ | SymInt _ -> false

  let isZero = function
    | Bot | Top -> false
    | ConcInt i -> i = NUInt.ZERO
    | SymInt (a, _, b) -> a = NUInt.ZERO && b = NUInt.ZERO

  let isConst = function
    | Bot | Top -> false
    | ConcInt _ -> true
    | SymInt (a, _, _) -> a = NUInt.ZERO

  let isAndMask = function
    | Bot | Top -> false
    | ConcInt i -> NUInt.isAndMask i
    | SymInt (a, _, b) -> a = NUInt.ZERO && NUInt.isAndMask b

  let isSymbolic = function
    | Bot | Top | ConcInt _ -> false
    | SymInt (a, _, _) -> a <> NUInt.ZERO

  let isSingularSymbolic = function
    | Bot | Top | ConcInt _ -> false
    | SymInt (a, _, _) -> a = NUInt.ONE

  // Getters.

  let getConst = function
    | Bot | Top -> raise NotConstException
    | ConcInt i -> i
    | SymInt (a, _, b) ->
      if a = NUInt.ZERO then b else raise NotConstException

  let getConstPart = function
    | Bot | Top -> raise NotConstException
    | ConcInt i -> i
    | SymInt (_, _, b) -> b

  let getSymbol = function
    | Bot | Top | ConcInt _ -> raise NotSymbolicException
    | SymInt (a, s, _) ->
      if a <> NUInt.ZERO then s else raise NotSingularException

  let getSingularSymbol = function
    | Bot | Top | ConcInt _ -> raise NotSingularException
    | SymInt (a, s, _) ->
      if a = NUInt.ONE then s else raise NotSingularException

  // Binary operations.

  let simpleBinOp op x y =
    match x, y with
    | Bot, _ | _, Bot -> Bot
    | Top, _ | _, Top -> Top
    | ConcInt i1, ConcInt i2 -> ConcInt (op i1 i2)
    | SymInt (a, s, b), ConcInt i | ConcInt i, SymInt (a, s, b) ->
      SymInt (a, s, op b i)
    | SymInt (a1, s1, b1), SymInt (a2, s2, b2) ->
      if s1 = s2 then SymInt (op a1 a2, s1, op b1 b2) else Top

  let add x y = simpleBinOp (fun i1 i2 -> i1 + i2) x y
  let sub x y = simpleBinOp (fun i1 i2 -> i1 - i2) x y

  let rec mul x y =
    match x, y with
    | Bot, _ | _, Bot -> Bot
    | Top, _ | _, Top -> Top
    | ConcInt i1, ConcInt i2 -> ConcInt (i1 * i2)
    | SymInt (a, s, b), ConcInt i | ConcInt i, SymInt (a, s, b) ->
      SymInt (a * i , s, b * i)
    | SymInt (a1, _, b1), SymInt (a2, _, b2) ->
      if a1 = NUInt.ZERO then mul (ConcInt b1) y
      elif a2 = NUInt.ZERO then mul (ConcInt b2) x
      else Top

  let shl x y =
    // Handle it as x * (2 ^ y) if y is a constant.
    if isConst y && getConst y < NUInt.SHL_LIMIT
    then mul x (ConcInt (NUInt.powOfTwo (getConst y)))
    else Top

  let shr x y =
    // Handle it as x / (2 ^ y) if both x and y are constants.
    if isConst x && isConst y
    then ConcInt (getConst x >>> int32 (getConst y))
    else Top

  // Symbol operation.
  let substSymbol newLin x =
    match x with
    | Bot | Top | ConcInt _ -> x
    | SymInt (a, _, b) -> add (mul (ConcInt a) newLin) (ConcInt b)

type AbsInt = {
  // Represents symbolic expression part ('ax + b').
  IntPart : LinearExp
  // Records allocsites that use this integer as allocation size. This will
  // serve as a hint when inferring argument type of a syscall.
  Tags : Set<Addr>
}

type AbsIntModule () =

  inherit AbstractDomain<AbsInt>()

  // Abstract domain functions.

  override __.bot =
    { IntPart = LinearExp.Bot; Tags = Set.empty }

  override __.leq x y =
    let intLeq = LinearExp.leq x.IntPart y.IntPart
    let tagLeq = Set.isSubset x.Tags y.Tags
    intLeq && tagLeq

  override __.join x y =
    let newIntPart = LinearExp.join x.IntPart y.IntPart
    let newTags = Set.union x.Tags y.Tags
    { IntPart = newIntPart; Tags = newTags  }

  override __.toString x =
    let intStr = LinearExp.toString x.IntPart
    let tagAddrs = Set.map Addr.toString x.Tags
    let tagStr = if Set.isEmpty tagAddrs then ""
                 else sprintf "( = SizeOf{ %s })" (String.concat ", " tagAddrs)
    intStr + tagStr

  override __.isBot x =
    LinearExp.isBot x.IntPart && Set.isEmpty x.Tags

  // Constants.

  member __.top =
    { IntPart = LinearExp.Top; Tags = Set.empty }

  member __.zero =
    { IntPart = LinearExp.ConcInt NUInt.ZERO; Tags = Set.empty }

  // Checker functions.

  member __.isZero x =
    LinearExp.isZero x.IntPart

  member __.isConst x =
    LinearExp.isConst x.IntPart

  /// Check if an integer has a symbolic term.
  member __.isSymbolic x =
    LinearExp.isSymbolic x.IntPart

  /// Check if an integer has form of 'x + C'.
  member __.isSingularSymbolic x =
    LinearExp.isSingularSymbolic x.IntPart

  member __.isAndMask x =
    LinearExp.isAndMask x.IntPart

  // Getter function.

  member __.getConst x =
    LinearExp.getConst x.IntPart

  member __.getConstPart x =
    LinearExp.getConstPart x.IntPart

  member __.getSingularSymbol x =
    LinearExp.getSingularSymbol x.IntPart

  // Binary operation functions.

  member __.add x y =
    let intPart = LinearExp.simpleBinOp NUInt.add x.IntPart y.IntPart
    { IntPart = intPart; Tags = Set.empty }

  member __.sub x y =
    let intPart = LinearExp.simpleBinOp NUInt.sub x.IntPart y.IntPart
    { IntPart = intPart; Tags = Set.empty }

  member __.mul x y =
    let intPart = LinearExp.mul x.IntPart y.IntPart
    { IntPart = intPart; Tags = Set.empty }

  member __.shl x y =
    let intPart = LinearExp.shl x.IntPart y.IntPart
    { IntPart = intPart; Tags = Set.empty }

  member __.shr x y =
    let intPart = LinearExp.shr x.IntPart y.IntPart
    { IntPart = intPart; Tags = Set.empty }

  // Creator functions.

  member __.ofNUInt ui =
    let intPart = LinearExp.ConcInt ui
    { IntPart = intPart; Tags = Set.empty }

  member __.ofSymbol sym =
    let intPart = LinearExp.SymInt (NUInt.ONE, sym, NUInt.ZERO)
    { IntPart = intPart; Tags = Set.empty }

  /// Symbol substitution function.
  member __.substitute x substMap =
    let intPart = x.IntPart
    if not (LinearExp.isSymbolic intPart) then x
    else match Map.tryFind (LinearExp.getSymbol intPart) substMap with
         | None -> { IntPart = LinearExp.Top; Tags = Set.empty }
         | Some substInt ->
           let newIntPart = LinearExp.substSymbol substInt.IntPart intPart
           { IntPart = newIntPart; Tags = Set.empty }

  /// Collect symbols used in 'x'.
  member __.collectSymbols x =
    if LinearExp.isSymbolic x.IntPart
    then Set.singleton (LinearExp.getSymbol x.IntPart)
    else Set.empty

  member __.addTag x allocsite =
    { x with Tags = Set.add allocsite x.Tags }

  member __.getTags x =
    x.Tags

let AbsInt = AbsIntModule () // Now we can use 'AbsInt' like a module.
