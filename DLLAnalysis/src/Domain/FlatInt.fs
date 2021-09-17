module DLLAnalysis.AbsDom.FlatInt

open DLLAnalysis
open DLLAnalysis.AbsDom.Signature

exception NotConstException

type FlatInt = Bot | Top | Const of NUInt

type FlatIntModule () =

  inherit AbstractDomain<FlatInt>()

  // Abstract domain functions.

  override __.bot = Bot

  override __.leq x y =
    match x, y with
    | Bot, _ | _, Top -> true
    | _, Bot | Top, _ -> false
    | Const i1, Const i2 -> i1 = i2

  override __.join x y =
    match x, y with
    | Bot, _ | _, Top -> y
    | _, Bot | Top, _ -> x
    | Const a, Const b -> if a = b  then Const a else Top

  override __.toString x =
    match x with
    | Bot -> "_"
    | Top -> "T"
    | Const i -> sprintf "0x%x" i

  override __.isBot x =
    match x with
    | Bot -> true
    | Top | Const _ -> false

  member __.isTop x =
    match x with
    | Top -> true
    | Bot | Const _ -> false

  // Constants.

  member __.unknown = Top

  member __.zero = Const NUInt.ZERO

  // Construct functions.

  member __.ofUInt64 i64 = Const i64

  // Checker functions.

  member __.isZero = function
    | Bot _ | Top _ -> false
    | Const i -> i = NUInt.ZERO

  member __.isConst = function
    | Bot _ | Top _ -> false
    | Const _ -> true

  member __.isAndMask = function
    | Bot _ | Top _ -> false
    | Const i -> NUInt.isAndMask i

  // Getter function.

  member __.getConst = function
    | Bot _ | Top _ -> raise NotConstException
    | Const i -> i

  // Binary operation functions.

  member __.add x y =
    match x, y with
    | Bot, _  | _, Bot -> Bot
    | Top, _  | _, Top -> Top
    | Const ix, Const iy -> Const (ix + iy)

  member __.sub x y =
    match x, y with
    | Bot, _  | _, Bot -> Bot
    | Top, _  | _, Top -> Top
    | Const ix, Const iy -> Const (ix - iy)

  member __.mul x y =
    match x, y with
    | Bot, _  | _, Bot -> Bot
    | Top, _  | _, Top -> Top
    | Const ix, Const iy -> Const (ix * iy)

  member __.shl x y =
    match x, y with
    | Bot, _  | _, Bot -> Bot
    | Top, _  | _, Top -> Top
    | Const ix, Const iy -> Const (ix <<< int32 (iy))

  member __.shr x y =
    match x, y with
    | Bot, _  | _, Bot -> Bot
    | Top, _  | _, Top -> Top
    | Const ix, Const iy -> Const (ix >>> int32 (iy))

let FlatInt = FlatIntModule() // Now we can use 'FlatInt' like a module.
