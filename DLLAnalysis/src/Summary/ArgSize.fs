namespace DLLAnalysis.Summary

open WinHeader
open DLLAnalysis
open DLLAnalysis.AbstractDomain
open DLLAnalysis.Utils

exception SizeResolveException

type ArgIndex =
  | DirectArgVar of int
  | DerefArgVar of int

type SizeArgMap = {
  SymArgMap : Map<Symbol,ArgIndex>
  TagArgMap : Map<Addr,ArgIndex>
  ConstArgMap : Map<NUInt,ArgIndex>
}

module SizeArgMap =

  let empty =
    { SymArgMap = Map.empty; TagArgMap = Map.empty; ConstArgMap = Map.empty }

  let toString sizeArgMap =
    let symArgEntries = Map.toList sizeArgMap.SymArgMap
    let tagArgEntries = Map.toList sizeArgMap.TagArgMap
    let constArgEntries = Map.toList sizeArgMap.ConstArgMap
    let symArgToStr (sym, idx) = sprintf "%s -> %A" sym idx
    let tagArgToStr (addr, idx) = sprintf "%s -> %A" (Addr.toString addr) idx
    let constArgToStr (ui64, idx) = sprintf "%x -> %A" ui64 idx
    List.map symArgToStr symArgEntries @
    List.map tagArgToStr tagArgEntries @
    List.map constArgToStr constArgEntries
    |> String.concat "\n"

  let private addSymArgDirect absInt argIdx sizeMap =
    if AbsInt.isSingularSymbolic absInt then
      let sym = AbsInt.getSingularSymbol absInt
      Map.add sym (DirectArgVar argIdx) sizeMap
    else sizeMap

  let private addSymArgDeref absInt argIdx sizeMap =
    if AbsInt.isSingularSymbolic absInt then
      let sym = AbsInt.getSingularSymbol absInt
      if Map.containsKey sym sizeMap then sizeMap
      else Map.add sym (DerefArgVar argIdx) sizeMap
    else sizeMap

  let private addTagArgDirect tags argIdx sizeMap =
    let folder accMap t =
      Map.add t (DirectArgVar argIdx) accMap
    Set.fold folder sizeMap tags

  let private addTagArgDeref tags argIdx sizeMap =
    let folder accMap t =
      if Map.containsKey t accMap then accMap
      else Map.add t (DerefArgVar argIdx) accMap
    Set.fold folder sizeMap tags

  let private addConstArgDirect absInt argIdx sizeMap =
    if not (AbsInt.isConst absInt) then sizeMap
    else let constSize = AbsInt.getConst absInt
         let argVar = DirectArgVar argIdx
         if NUInt.WORD_SIZE < constSize && constSize < NUInt.ARCH_MAX_INT
         then Map.add constSize argVar sizeMap
         else sizeMap

  let private addConstArgDeref absInt argIdx sizeMap =
    if not (AbsInt.isConst absInt) then sizeMap
    else let constSize = AbsInt.getConst absInt
         let argVar = DerefArgVar argIdx
         if NUInt.WORD_SIZE < constSize && constSize < NUInt.ARCH_MAX_INT
         then Map.add constSize argVar sizeMap
         else sizeMap

  let private makeAux mem accMap argIdx argVal =
    let absIntDirect = AbsVal.getInt argVal
    let vDeref = AbsMem.findSet (AbsVal.getLoc argVal) mem
    let absIntDeref = AbsVal.getInt vDeref
    let newSymMap = addSymArgDirect absIntDirect argIdx accMap.SymArgMap
    let newSymMap = addSymArgDeref absIntDeref argIdx newSymMap
    let tagsDirect = AbsInt.getTags absIntDirect
    let tagsDeref = AbsInt.getTags absIntDeref
    let newTagMap = addTagArgDirect tagsDirect argIdx accMap.TagArgMap
    let newTagMap = addTagArgDeref tagsDeref argIdx newTagMap
    let newCosntMap = addConstArgDirect absIntDirect argIdx accMap.ConstArgMap
    let newCosntMap = addConstArgDeref absIntDeref argIdx newCosntMap
    { SymArgMap = newSymMap; TagArgMap = newTagMap; ConstArgMap = newCosntMap }

  /// Check whether 'arg' or '*arg' can be a size of a memory allocated in heap.
  /// Return a tuple of map that record such arguments.
  let make mem argVals =
    List.foldi (makeAux mem) empty argVals

  let substituteAux subst accSymMap sym v =
    match Map.tryFind sym subst.IntSubst with
    | None -> accSymMap
    | Some newAbsInt ->
      if AbsInt.isSingularSymbolic newAbsInt
      then Map.add (AbsInt.getSingularSymbol newAbsInt) v accSymMap
      else accSymMap

  let substitute subst sizeMap =
    let symArgMap = sizeMap.SymArgMap
    let newSymMap = Map.fold (substituteAux subst) Map.empty symArgMap
    { sizeMap with SymArgMap = newSymMap }

type SymbolicSize =
  | ConcSize of Size
  | SymSize of AbsInt

module SymbolicSize =

  let unknown = ConcSize UnknownSize

  let ofAbsInt absInt = SymSize absInt

  let ofNUInt i =
    if i > NUInt.ARCH_MAX_INT then ConcSize UnknownSize
    else ConcSize (Fixed (NUInt.toUInt64 i))

  let toString = function
    | ConcSize s -> Size.toJson s
    | SymSize i -> AbsInt.toString i

  let private inferWithAllocsite (offset: Offset) allocSite tagArgMap =
    let size = match Map.find allocSite tagArgMap with
               | DirectArgVar idx -> ArgField (idx, [])
               | DerefArgVar idx -> ArgField (idx, [0UL])
    let offset = if offset <= Offset.ZERO then Offset.ZERO else offset
    let size = if offset = Offset.ZERO then size
               else Sub (size, Fixed (uint64 offset))
    ConcSize size

  let private containsSymArg allocSize symArgMap =
    if AbsInt.isSingularSymbolic allocSize then
      let sym = AbsInt.getSingularSymbol allocSize
      Map.containsKey sym symArgMap
    else false

  let private inferSymArgSize (offset: Offset) allocSize symArgMap =
    let sym = AbsInt.getSingularSymbol allocSize
    let cTerm = AbsInt.getConstPart allocSize |> Offset.ofNUInt
    let size = match Map.find sym symArgMap with
               | DirectArgVar i -> ArgField (i, [])
               | DerefArgVar i -> ArgField (i, [0UL])
    let offset = if offset <= Offset.ZERO then Offset.ZERO else offset
    let alpha = Offset.sub cTerm offset // Additional size.
    let size = if alpha = Offset.ZERO then size
               elif alpha > Offset.ZERO then Add (size, Fixed (uint64 alpha))
               else Sub (size, Fixed (uint64 alpha))
    ConcSize size

  let private inferConstArgSize hintOpt offset allocSize constArgMap =
    let size = match hintOpt with
               | Some hintSize -> hintSize
               | None -> AbsInt.getConst allocSize - Offset.toNUInt offset
    match Map.tryFind size constArgMap with
    | None -> ofNUInt size
    | Some (DirectArgVar i) -> ConcSize (ArgField (i, []))
    | Some (DerefArgVar i) -> ConcSize (ArgField (i, [0UL]))

  /// Infer the size of a struct, with the given pointer that points to a heap
  /// memory chunk.
  let inferFromHeap hintOpt sizeMap allocSite allocSize offset =
    let constArgMap = sizeMap.ConstArgMap
    let symArgMap = sizeMap.SymArgMap
    let tagArgMap = sizeMap.TagArgMap
    if Map.containsKey allocSite tagArgMap then // Allocsite tag entry exists.
      inferWithAllocsite offset allocSite sizeMap.TagArgMap
    elif containsSymArg allocSize symArgMap then // Symbol entry exists.
      inferSymArgSize offset allocSize symArgMap
    elif AbsInt.isConst allocSize || Option.isSome hintOpt then
      inferConstArgSize hintOpt offset allocSize constArgMap
    else
      Offset.toNUInt offset |> AbsInt.ofNUInt
      |> AbsInt.sub allocSize |> ofAbsInt

  let inferFromStack hintOpt sizeMap baseLoc fields =
    let size = match hintOpt with
               | Some hintSize -> hintSize
               | None -> AbsLoc.findDistance (List.last fields) baseLoc
                         |> Offset.add NInt.WORD_SIZE
                         |> Offset.alignDown
                         |> Offset.toNUInt
    match Map.tryFind size sizeMap.ConstArgMap with
    | None -> unknown
    | Some (DirectArgVar i) -> ConcSize (ArgField (i, []))
    | Some (DerefArgVar i) -> ConcSize (ArgField (i, [0UL]))

  let instantiate subst = function
    | ConcSize s -> ConcSize s
    | SymSize i -> SymSize (AbsInt.substitute i subst.IntSubst)

  let collectSymbols = function
    | ConcSize _ -> Set.empty
    | SymSize i -> AbsInt.collectSymbols i

  let private resolveSizeArg sizeSymbol sizeArgMap =
    let sizeIntSymbol = Symbol.appendIntSuffix sizeSymbol
    match Map.tryFind sizeIntSymbol sizeArgMap.SymArgMap with
    | None -> raise SizeResolveException
    | Some (DirectArgVar idx) -> ArgField (idx, [])
    | Some (DerefArgVar idx) -> ArgField (idx, [0UL])

  let rec private resolveAux withinStruct record sizeArgMap size =
    match size with
    | UnknownSize | Fixed _ -> size
    | ArgField (idx, offsets) ->
      let sizeSymbol = Convert.argOffsetsToSymbol idx offsets record
      resolveSizeArg sizeSymbol sizeArgMap
    | AdjacentField offsets when withinStruct -> AdjacentField offsets
    | AdjacentField _ -> UnknownSize
    | Add (size1, size2) ->
      let newSize1 = resolveAux withinStruct record sizeArgMap size1
      let newSize2 = resolveAux withinStruct record sizeArgMap size2
      Add (newSize1, newSize2)
    | Sub (size1, size2) ->
      let newSize1 = resolveAux withinStruct record sizeArgMap size1
      let newSize2 = resolveAux withinStruct record sizeArgMap size2
      Sub (newSize1, newSize2)
    | Mult (size1, size2) ->
      let newSize1 = resolveAux withinStruct record sizeArgMap size1
      let newSize2 = resolveAux withinStruct record sizeArgMap size2
      Mult (newSize1, newSize2)
    | Div (size1, size2) ->
      let newSize1 = resolveAux withinStruct record sizeArgMap size1
      let newSize2 = resolveAux withinStruct record sizeArgMap size2
      Div (newSize1, newSize2)

  // Resolve an ArgSize with the given context.
  let resolve withinStruct record sizeArgMap size =
    try resolveAux withinStruct record sizeArgMap size with
    | SizeResolveException -> UnknownSize
    | ConvertException -> UnknownSize
