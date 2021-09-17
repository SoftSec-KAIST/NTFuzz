type AbstractDomain<'t> =
  abstract member leq: 't -> 't -> bool
  abstract member bot: 't

type Itv = Btw of int * int | Bot | TOP

type ItvDom() =
  interface AbstractDomain<Itv> with
    member __.leq x y =
      match x, y with
      | Bot, _ -> true
      | _ -> false

    member __.bot = Bot

type ALoc = Stack of int | Heap of int

type ALocSet = Set<ALoc>

type ALocDom() =
  interface AbstractDomain<ALocSet> with
    member __.leq x y =
      Set.isSubset x y

    member __.bot = Set<ALoc>([])

type ProdDom<'a, 'b>
  (aDom : 'aDom when 'aDom :> AbstractDomain<'a>,
   bDom : 'bDom when 'bDom :> AbstractDomain<'b>)
  =
  interface AbstractDomain<'a * 'b> with
    member __.leq x y =
      let xa, xb = x
      let ya, yb = y
      let absDomA = aDom :> AbstractDomain<'a>
      let absDomB = bDom :> AbstractDomain<'b>
      absDomA.leq xa ya && absDomB.leq xb yb

    member __.bot = (aDom.bot, bDom.bot)

(* The following line raises error *)
/// let itvBot = ItvDom().bot

(* So we have to cast it to interface, but what if I want to call
 * itvDom.isPositive() method?
 *)
let itvDom = ItvDom() :> AbstractDomain<Itv>
let aLocDom = ALocDom() :> AbstractDomain<ALocSet>
let absValDom = ProdDom<Itv,ALocSet>(itvDom, aLocDom) :> AbstractDomain<Itv*ALocSet>
let bot = absValDom.bot
let b = absValDom.leq bot bot
