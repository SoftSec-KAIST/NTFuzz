module DLLAnalysis.AbsDom.AbsLoc

open DLLAnalysis
open DLLAnalysis.Utils
open DLLAnalysis.Const
open DLLAnalysis.AbsDom.Signature
open DLLAnalysis.AbsDom.Functor
open DLLAnalysis.AbsDom.Register
open DLLAnalysis.AbsDom.AbsInt

exception NotSymbolicException
exception NonComparableLocsException

type AbsLoc =
  | Stack of Subroutine * Offset
  | Global of Addr
  | Heap of Allocsite: Addr * Offset * Size: AbsInt
  | SLoc of Symbol * Offset

type AbsLocModule () =
  inherit Elem<AbsLoc>()

  override __.toString loc =
    match loc with
    | Stack (sub, offset) when offset >= Offset.ZERO ->
      sprintf "Stack_%s+%s" (Subroutine.toString sub) (Offset.toString offset)
    | Stack (sub, offset) -> // when offset < 0
      let offset' = -offset
      sprintf "Stack_%s-%s" (Subroutine.toString sub) (Offset.toString offset')
    | Global addr -> Addr.toString addr
    | Heap (allocsite, offset, size) when offset >= Offset.ZERO ->
      let allocStr = Addr.toString allocsite
      let offsetStr = Offset.toString offset
      let sizeStr = AbsInt.toString size
      sprintf "Heap_%s+%s(size=%s)" allocStr offsetStr sizeStr
    | Heap (allocst, offset, _) -> // when offset < Offset.zero
      let allocAddr = Addr.getRawAddr allocst
      let offsetStr = Offset.toString (-offset)
      sprintf "Heap_%s-%s" (RawAddr.toString allocAddr)  offsetStr
    | SLoc (sym, offset) when offset >= Offset.ZERO ->
      sprintf "%s+%s" (Symbol.toString sym) (Offset.toString offset)
    | SLoc (sym, offset) -> // when offset < Offset.zero
      let offsetStr = Offset.toString (-offset)
      sprintf "%s-%s" (Symbol.toString sym) offsetStr

  /// Return a string * offset pair that can be used to sort abstract locations.
  member __.getOrder loc =
    match loc with
    | Stack (sub, offset) -> ("Stack_" + (Addr.toString sub), offset)
    | Global addr ->
      let bin = Addr.getBinary addr
      let rawAddr = Addr.getRawAddr addr
      ("Global_" + bin, Offset.ofNUInt rawAddr)
    | Heap (allocst, offset, _) ->
      let allocStr = Addr.toString allocst
      ("Heap_" + allocStr, offset)
    | SLoc (sym, offset) -> (sprintf "Sym_%s" (Symbol.toString sym), offset)

  // Create functions.

  member __.makeGlobalLoc addr = Global addr
  member __.makeStackLoc subrtn offset = Stack (subrtn, offset)
  member __.makeHeapLoc allocst offset size = Heap (allocst, offset, size)
  member __.makeSymLoc sym offset = SLoc (sym, offset)

  // Binary operation functions.

  member __.addOffset loc i =
    match loc with
    | Stack (sub, offset) -> Stack (sub, Offset.add offset i)
    | Global addr -> Global (Addr.addOffset addr i)
    | Heap (allocst, offset, size) -> Heap (allocst, Offset.add offset i, size)
    | SLoc (sym, offset) -> SLoc (sym, Offset.add offset i)

  member __.subOffset loc i =
    match loc with
    | Stack (sub, offset) -> Stack (sub, Offset.sub offset i)
    | Global addr -> Global (Addr.subOffset addr i)
    | Heap (allocst, offset, size) -> Heap (allocst, Offset.sub offset i, size)
    | SLoc (sym, offset) -> SLoc (sym, Offset.sub offset i)

  member __.isSameBase loc1 loc2 =
    match loc1, loc2 with
    | Stack (sub1, _), Stack (sub2, _) -> sub1 = sub2
    | Global addr1, Global addr2 -> Addr.getBinary addr1 = Addr.getBinary addr2
    | Heap (a1, _, _), Heap (a2, _, _) -> a1 = a2
    | SLoc (sym1, _), SLoc (sym2, _) -> sym1 = sym2
    | _ -> false

  member __.findDistance loc1 loc2 =
    match loc1, loc2 with
    | Stack (sub1, offset1), Stack (sub2, offset2) when sub1 = sub2 ->
      offset1 - offset2
    | Global a1, Global a2 when Addr.getBinary a1 = Addr.getBinary a2 ->
      Offset.ofNUInt (Addr.getRawAddr a1 - Addr.getRawAddr a2)
    | Heap (a1, offset1, _), Heap (a2, offset2, _) when a1 = a2 ->
      offset1 - offset2
    | SLoc (sym1, offset1), SLoc (sym2, offset2) when sym1 = sym2 ->
      offset1 - offset2
    | _ -> raise NonComparableLocsException

  // Checker functions.

  member __.isStack = function
    | Stack _ -> true
    | Global _ -> false
    | Heap _ -> false
    | SLoc _ -> false

  member __.isGlobal = function
    | Stack _ -> false
    | Global _ -> true
    | Heap _ -> false
    | SLoc _ -> false

  member __.isTIB = function
    | Stack _ -> false
    | Global _ -> false
    | Heap _ -> false
    | SLoc _ -> false

  member __.isHeap = function
    | Stack _ -> false
    | Global _ -> false
    | Heap _ -> true
    | SLoc _ -> false

  member __.isSymbolic = function
    | Stack _ -> false
    | Global _ -> false
    | Heap _ -> false
    | SLoc _ -> true

  /// Check if a given location should be considered as an argument.
  member __.isArgument = function
    | Stack (_, offset) -> offset >= Offset.ZERO // x86
    | Global _ -> false // For scalability, ignore side effect on globals.
    | Heap _ -> false // Caution: heap memory is not an argument.
    | SLoc _ -> true // Symbolic location is always an argument.

  /// Check if a given location is a 'root' argument location, which is not
  /// derived from another argument location.
  member __.isRootArgument = function
    | Stack (_, offset) -> offset >= Offset.ZERO // x86
    | Global _ -> false // For scalability, ignore side effect on globals.
    | Heap _ -> false
    | SLoc (sym, _) -> // Only register symbols should be considered as roots.
      let isMatching r = Symbol.appendLocSuffix (Register.decideSymbol r) = sym
      List.exists isMatching Register.inputs

  /// Check if this location is for n-th argument. Note that symbolic locations
  /// with register symbol are not our interest here.
  member __.isNthArgument n = function
    | Stack (_, offset) ->
      (Offset.ofInt (n + 1)) * NInt.WORD_SIZE = offset
    | Global _ | Heap _ | SLoc _ -> false

  member __.isInvalid = function
    // Filter out locations that are likely to be spurious (FP), since access to
    // these locations indicate buffer underflow.
    | Heap (_, offset, _) | SLoc (_, offset) -> offset < Offset.ZERO
    | Stack _ | Global _ -> false

  // Getter functions.

  member __.getSymbol = function
    | Stack _ | Global _ | Heap _ -> raise NotSymbolicException
    | SLoc (sym, _) -> sym

  member __.getBase = function
    | Stack (subrtn, _) -> Stack (subrtn, Offset.ZERO)
    | Global addr ->
      let bin = Addr.getBinary addr
      let baseAddr = Addr.makeWithRawAddr bin RawAddr.ZERO
      Global baseAddr
    | Heap (allocsite, _, _) -> Heap (allocsite, Offset.ZERO, AbsInt.bot)
    | SLoc (sym, _) -> SLoc (sym, Offset.ZERO)

  member __.getOffset = function
    | Stack (_, offset) -> offset
    | Global addr -> Offset.ofNUInt (Addr.getRawAddr addr)
    | Heap (_, offset, _) -> offset
    | SLoc (_, offset) -> offset

  /// Get the argument index of a stack argument location. Note that symbolic
  /// locations with register symbol are not our interest here.
  member __.tryGetRootArgIdx = function
    | Stack (_, offset) ->
      if offset < (Offset.ofInt (NUMREGARG + 1)) * Offset.WORD_SIZE
      then None
      else Some (Offset.toInt (offset / Offset.WORD_SIZE) - 1)
    | Global _ -> None
    | Heap _ -> None
    | SLoc _ -> None

  member __.setSize loc size =
    match loc with
    | Stack _ -> loc
    | Global _ -> loc
    | Heap (allocsite, offset, _) -> Heap (allocsite, offset, size)
    | SLoc _ -> loc

let AbsLoc = AbsLocModule () // Now we can use 'AbsLoc' like a module.

type AbsLocSet = Set<AbsLoc>

type AbsLocSetModule() =
  inherit SetDomain<AbsLoc>(AbsLoc)

  member __.addOffset locSet i =
    Set.map (fun loc -> AbsLoc.addOffset loc i) locSet

  member __.subOffset locSet i =
    Set.map (fun loc -> AbsLoc.subOffset loc i) locSet

  member __.resolveSymbol loc intSymMap locSymMap =
    match loc with
    | Stack _ | Global _ -> Set.singleton loc
    | Heap (allocsite, offset, size) ->
      let newSize = AbsInt.substitute size intSymMap
      Set.singleton (Heap (allocsite, offset, newSize))
    | SLoc (sym, offset) ->
      if not (Map.containsKey sym locSymMap) then Set.empty
      else Set.map (fun l -> AbsLoc.addOffset l offset) (Map.find sym locSymMap)

  /// Symbol substitution function.
  member __.substitute locSet intSymMap locSymMap =
    Set.fold (fun accSet loc ->
      __.join (__.resolveSymbol loc intSymMap locSymMap) accSet
    ) Set.empty locSet

  /// Collect symbols used in 'locSet'.
  member __.collectSymbols locSet =
    let folder accSet = function
      | Stack _ | Global _ -> accSet
      | Heap (_, _, size) -> Set.union (AbsInt.collectSymbols size) accSet
      | SLoc (sym, _) -> Set.add sym accSet
    Set.fold folder Set.empty locSet

  /// Remove stack locations from the set.
  member __.nullifyStackLoc locSet =
    Set.filter (not << AbsLoc.isStack) locSet

 let AbsLocSet = AbsLocSetModule () // Now we can use 'AbsLocSet' like a module.
