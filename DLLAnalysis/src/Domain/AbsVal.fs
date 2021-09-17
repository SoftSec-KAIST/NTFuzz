module DLLAnalysis.AbsDom.AbsVal

open B2R2.BinIR
open DLLAnalysis
open DLLAnalysis.Const
open DLLAnalysis.AbsDom.Functor
open DLLAnalysis.AbsDom.AbsInt
open DLLAnalysis.AbsDom.AbsLoc
open DLLAnalysis.AbsDom.Function
open DLLAnalysis.AbsDom.TypeConstr

type Elem1 = AbsInt * AbsLocSet
type Elem2 = FunctionSet * TypeConstr
type AbsVal = Elem1 * Elem2

// Shortcuts.
let LSet = AbsLocSet
let FSet = FunctionSet
let Typ = TypeConstr

let Dom1 = Prod2Domain<AbsInt,AbsLocSet>(AbsInt, LSet)
let Dom2 = Prod2Domain<FunctionSet,TypeConstr>(FSet, Typ)

type AbsValModule() =
  inherit Prod2Domain<Elem1,Elem2>(Dom1, Dom2)

  // Override stringfy function for readability.
  override __.toString (x: AbsVal) =
    let absInt = __.getInt x
    let absLoc = __.getLoc x
    let func = __.getFunc x
    let typ = __.getConstr x
    let typStr = if TypeConstr.isBot typ then ""
                 else sprintf ": %s" (TypeConstr.toString typ)
    if __.isBot x then "_" // For conciseness.
    elif LSet.isBot absLoc && FSet.isBot func then
      sprintf "(int)%s%s" (AbsInt.toString absInt ) typStr
    elif AbsInt.isBot absInt && FSet.isBot func then
      sprintf "%s%s" (LSet.toString absLoc) typStr
    elif AbsInt.isBot absInt && LSet.isBot absLoc then
      sprintf "%s%s" (FSet.toString func) typStr
    else
      let intStr = AbsInt.toString absInt
      let locStr = LSet.toString absLoc
      let funcStr = FSet.toString func
      sprintf "<%s, %s, %s>%s" intStr locStr funcStr typStr

  (* Getter functions. *)

  member __.getInt (x: AbsVal) : AbsInt =
    let (i, _) = fst x
    i

  member __.getLoc (x: AbsVal) : AbsLocSet =
    let (_, locs) = fst x
    locs

  member __.getFunc (x: AbsVal) : FunctionSet =
    let (funcs, _) = snd x
    funcs

  member __.getConstr (x: AbsVal) : TypeConstr =
    let (_, typ) = snd x
    typ

  (* Setter functions. *)

  member __.setInt i (x: AbsVal) : AbsVal =
    let (_, l), (f, typ) = x
    (i, l), (f, typ)

  member __.setLoc l (x: AbsVal) : AbsVal =
    let (i, _), (f, typ) = x
    (i, l), (f, typ)

  member __.setFunc f (x: AbsVal) : AbsVal =
    let (i, l), (_, typ) = x
    (i, l), (f, typ)

  member __.setConstr typ (x: AbsVal) : AbsVal =
    let (i, l), (f, _) = x
    ((i, l), (f, typ))

  /// Remove stack locations from the abstract location set.
  member __.removeStackLoc v : AbsVal =
    let newLocSet = AbsLocSet.nullifyStackLoc (__.getLoc v)
    __.setLoc newLocSet v

  /// Remove non-integer entities from the abstract value.
  member __.removeNonInt v : AbsVal =
    let int = __.getInt v
    let typ = __.getConstr v
    let newTyp = TypeConstr.leaveInt typ
    ((int, LSet.bot), (FSet.bot, newTyp))

  member __.addTag v allocsite : AbsVal =
    let newAbsInt = AbsInt.addTag (__.getInt v) allocsite
    __.setInt newAbsInt v

  (* Constants. *)

  member __.unknownInt : AbsVal =
    (AbsInt.top, LSet.bot), (FSet.bot,  Typ.bot)

  member __.unknownGlobal : AbsVal =
    (AbsInt.top, LSet.bot), (FSet.bot, Typ.bot)

  member __.zero : AbsVal =
    (AbsInt.zero, LSet.bot), (FSet.bot, Typ.bot)

  (* Checker functions. *)

  member __.isZero x =
    AbsInt.isZero (__.getInt x) &&
    AbsLocSet.isBot (__.getLoc x) &&
    FunctionSet.isBot (__.getFunc x)

  member __.isConst x =
    AbsInt.isConst (__.getInt x) &&
    AbsLocSet.isBot (__.getLoc x) &&
    FunctionSet.isBot (__.getFunc x)

  member __.isAndMask x =
    AbsInt.isAndMask (__.getInt x) &&
    AbsLocSet.isBot (__.getLoc x) &&
    FunctionSet.isBot (__.getFunc x)

  (* Constructor functions. *)

  member __.ofUInt64 width i64 : AbsVal =
    // Note that we should be careful not to generate integer constraints for
    // constants, because some constants can be used as special pointers (e.g.
    // NULL, 0x7fff...) or as handles (e.g. 0xffff...).
    let absInt = AbsInt.ofNUInt i64
    let lowBound = NUInt.ZERO
    let upperBound = NUInt.MAX_LIKELY_INT
    let typ =
      if width < NUInt.WORD_WIDTH then TypeConstr.makeInt width
      elif lowBound < i64 && i64 < upperBound then TypeConstr.makeInt width
      else TypeConstr.bot
    __.bot |> __.setInt absInt |> __.setConstr typ

  member __.ofLoc loc : AbsVal =
    let locs = Set.singleton loc
    __.setLoc locs __.bot

  member __.ofFunc func : AbsVal =
    let funcs = Set.singleton func
    let typ = if Function.isStatic func then TypeConstr.funcPtr
              else TypeConstr.bot
    __.bot |> __.setFunc funcs |> __.setConstr typ

  member __.ofSymbol prefixSymbol : AbsVal =
    let i = AbsInt.ofSymbol (Symbol.appendIntSuffix prefixSymbol)
    let locSym = Symbol.appendLocSuffix prefixSymbol
    let locs = AbsLocSet.make [AbsLoc.makeSymLoc locSym Offset.ZERO]
    let funcSym = Symbol.appendFunctionSuffix prefixSymbol
    let func = FunctionSet.make [Function.ofSymbol funcSym]
    let typSym = Symbol.appendTypSuffix prefixSymbol
    let typ = TypeConstr.ofSymbol typSym
    (i, locs), (func, typ)

  (* Unary operation functions. *)

  member __.unOp op v =
    match op with
    | UnOpType.NEG -> __.unknownInt
    | UnOpType.NOT -> __.unknownInt
    | _ -> failwith "Invalid UnOpType enum"

  (* Binary operation functions. *)

  member private __.add width v1 v2 : AbsVal =
    let i1 = __.getInt v1
    let i2 = __.getInt v2
    let locs1 = __.getLoc v1
    let locs2 = __.getLoc v2
    let i = AbsInt.add i1 i2
    // The key design choice of our abstrct location domain.
    let locs = AbsLocSet.join locs1 locs2
    let typ = if width = NUInt.WORD_WIDTH then Typ.bot
              else TypeConstr.makeInt width
    (i, locs), (FSet.bot, typ)

  member private __.sub width v1 v2 : AbsVal =
    let i1 = __.getInt v1
    let i2 = __.getInt v2
    let locs1 = __.getLoc v1
    let locs2 = __.getLoc v2
    let bases1 = AbsLocSet.map AbsLoc.getBase locs1
    let bases2 = AbsLocSet.map AbsLoc.getBase locs2
    let i = AbsInt.sub i1 i2
    // Consider as pointer subtract if two locs are the same singleton set.
    let delta, locs = if AbsLocSet.count bases1 = 1 && bases1 = bases2 then
                        let loc1 = AbsLocSet.maxBy AbsLoc.getOffset locs1
                        let loc2 = AbsLocSet.minBy AbsLoc.getOffset locs1
                        let distance = AbsLoc.findDistance loc1 loc2
                        if distance < NInt.ZERO then failwith "Unreachable"
                        let distance = NUInt.ofNInt distance
                        (AbsInt.ofNUInt distance, AbsLocSet.bot)
                      else (AbsInt.bot, locs1)
    let typ = if width = NUInt.WORD_WIDTH then Typ.bot
              else TypeConstr.makeInt width
    (AbsInt.join delta i, locs), (FSet.bot, typ)

  member private __.mul width v1 v2 : AbsVal =
    let i1 = __.getInt v1
    let i2 = __.getInt v2
    let i = AbsInt.mul i1 i2
    let typ = TypeConstr.makeInt width
    // Multiplication cannot yield abs-loc or function address.
    (i, LSet.bot), (FSet.bot, typ)

  member private __.andOp bitWidth v1 v2 : AbsVal =
    let locs1 = if __.isAndMask v2 then __.getLoc v1 else AbsLocSet.bot
    let locs2 = if __.isAndMask v1 then __.getLoc v2 else AbsLocSet.bot
    let locs = AbsLocSet.join locs1 locs2
    let typ = if __.isAndMask v2 then __.getConstr v1
              elif __.isAndMask v1 then __.getConstr v2
              else TypeConstr.makeInt bitWidth
    (AbsInt.top, locs), (FSet.bot, typ)

  member private __.orOp bitWidth _ _ : AbsVal =
    let typ = TypeConstr.makeInt bitWidth
    (AbsInt.top, LSet.bot), (FSet.bot, typ)

  member private __.shl bitWidth v1 v2 : AbsVal =
    let i1 = __.getInt v1
    let i2 = __.getInt v2
    let i = AbsInt.shl i1 i2
    let typ = TypeConstr.makeInt bitWidth
    // Shifting cannot yield abs-loc or function address.
    (i, LSet.bot), (FSet.bot, typ)

  member private __.shr bitWidth v1 v2 : AbsVal =
    let i1 = __.getInt v1
    let i2 = __.getInt v2
    let i = AbsInt.shr i1 i2
    let typ = TypeConstr.makeInt bitWidth
    // Shifting cannot yield abs-loc or function address.
    (i, LSet.bot), (FSet.bot, typ)

  member __.binOp bitWidth op v1 v2 =
    match op with
    | BinOpType.ADD -> __.add bitWidth v1 v2
    | BinOpType.SUB -> __.sub bitWidth v1 v2
    | BinOpType.MUL -> __.mul bitWidth v1 v2
    | BinOpType.AND -> __.andOp bitWidth v1 v2
    | BinOpType.OR -> __.orOp bitWidth v1 v2
    | BinOpType.SHL -> __.shl bitWidth v1 v2
    | BinOpType.SHR -> __.shr bitWidth v1 v2
    | BinOpType.CONCAT -> __.setInt AbsInt.top (__.join v1 v2)
    | BinOpType.DIV | BinOpType.SDIV | BinOpType.SAR | BinOpType.XOR
    | BinOpType.MOD | BinOpType.SMOD ->
      __.setConstr (TypeConstr.makeInt bitWidth) __.unknownInt
    | BinOpType.APP | BinOpType.CONS -> failwith "Unsupported"
    | _ -> failwith "Invalid BinOpType enum"

  (* Binary operation functions with an immediate operand. *)

  member __.addImmediate v (c: NUInt) : AbsVal =
    let i = AbsInt.add (__.getInt v) (AbsInt.ofNUInt c)
    let locs = AbsLocSet.addOffset (__.getLoc v) (Offset.ofNUInt c)
    (i, locs), (FSet.bot, Typ.bot)

  member __.subImmediate v (c: NUInt) : AbsVal =
    let i = AbsInt.sub (__.getInt v) (AbsInt.ofNUInt c)
    let locs = AbsLocSet.subOffset (__.getLoc v) (Offset.ofNUInt c)
    (i, locs), (FSet.bot, Typ.bot)

  member __.binOpImmediate op v c : AbsVal =
    match op with
    | BinOpType.ADD -> __.addImmediate v c
    | BinOpType.SUB -> __.subImmediate v c
    | _ -> failwith "Unreachable"

  (* Symbol related functions. *)

  /// Symbol substitution function.
  member __.substitute v intMap locMap funcMap typMap : AbsVal =
    let absInt = AbsInt.substitute (__.getInt v) intMap
    let locSet = AbsLocSet.substitute (__.getLoc v) intMap locMap
    let funcSet = FunctionSet.substitute (__.getFunc v) funcMap
    let typSet = TypeConstr.substitute (__.getConstr v) typMap
    let newVal = (absInt, locSet), (funcSet, typSet)
    __.syncType newVal

  /// Collect symbols used in 'v'.
  member __.collectSymbols v =
    let intSyms = AbsInt.collectSymbols (__.getInt v)
    let locSyms = AbsLocSet.collectSymbols (__.getLoc v)
    let funcSyms = FunctionSet.collectSymbols (__.getFunc v)
    let typSyms = TypeConstr.collectSymbols (__.getConstr v)
    Set.union intSyms locSyms |> Set.union funcSyms |> Set.union typSyms

  (* Type related functions *)

  member __.truncate width v : AbsVal =
    // If 'width' is wider than (WORD - BYTE), assume that the truncated value
    // can retain the memory address (e.g. AbsLoc). This is because the
    // least-significant byte is often abandoned for the alignment.
    let vTruncated = if width >= NUInt.WORD_WIDTH - NUInt.BYTE_WIDTH then v
                     else __.bot
    // For abstract integer domain, the threshold is the width of a byte.
    let absInt = if width >= NUInt.BYTE_WIDTH then __.getInt v else AbsInt.top
    let newTyp = __.getConstr v |> TypeConstr.truncate width
    vTruncated |> __.setInt absInt |> __.setConstr newTyp

  member __.syncType v =
    let typs = __.getConstr v
    let locs = __.getLoc v
    let newTyps = if not (AbsLocSet.isBot locs) then typs
                  else TypeConstr.leaveNonPtr typs
    __.setConstr newTyps v

let AbsVal = AbsValModule() // Now we can use 'AbsVal' like a module.
