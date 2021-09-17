namespace DLLAnalysis.TypeAnalysis.SelectMajority

open WinHeader
open DLLAnalysis
open DLLAnalysis.Utils

module SelectSize =

  // Auxiliary types for majority decision.
  type SizeKind = Fix | Arg | Adj | A | S | M

  let private rankSizes sizes =
    let sizeRank =
      [ Fix, List.filter Size.isFixed sizes;
        Arg, List.filter Size.isArgField sizes;
        Adj, List.filter Size.isAdjField sizes;
        A, List.filter Size.isAdd sizes;
        S, List.filter Size.isSub sizes;
        M, List.filter Size.isMul sizes;
      ] |> List.sortByDescending (List.length << snd) // Sort by frequency.
    List.head sizeRank // Choose majority.

  let private selectSize sizes =
    let tryGetFixedSize = function
      | Fixed n -> Some n
      | _ -> None
    List.choose tryGetFixedSize sizes |> List.max

  let private selectArgField sizes =
    let tryGetArgInfo = function
      | ArgField (idx, offsets) -> Some (idx, offsets)
      | _ -> None
    List.choose tryGetArgInfo sizes |> List.countBy id |> List.maxBy snd |> fst

  let private selectAdjField sizes =
    let tryGetAdjInfo = function
      | AdjacentField (offsets) -> Some (offsets)
      | _ -> None
    List.choose tryGetAdjInfo sizes |> List.countBy id |> List.maxBy snd |> fst

  let run (sizes: Size list) =
    let unknownCnt = List.filter (fun s -> s = UnknownSize) sizes |> List.length
    let majorSizeKind, chosens = rankSizes sizes // Choose majority.
    if List.length sizes = unknownCnt then UnknownSize // No definitive size.
    else match majorSizeKind with
         | Fix -> Fixed (selectSize chosens)
         | Arg -> ArgField (selectArgField chosens)
         | Adj -> AdjacentField (selectAdjField chosens)
         // For the following compositional sizes, give up majority selection.
         | A | S | M -> if List.length (List.distinct chosens) = 1
                        then List.head chosens
                        else UnknownSize

module SelectType =

  let private DEBUG_FLAG = false

  // Auxiliary types for majority decision.
  type TypKind = H | DH | Sc | Struc | Arr | Str | P | F

  let private rankTypes typs =
    let typeRank =
      [ H, List.filter ArgType.isHandle typs;
        DH, List.filter ArgType.isD3Handle typs;
        Sc, List.filter ArgType.isScalar typs;
        Struc, List.filter ArgType.isStruct typs;
        Arr, List.filter ArgType.isArrLike typs;
        Str, List.filter ArgType.isStrLike typs;
        P, List.filter ArgType.isPtr typs
        F, List.filter ArgType.isFuncPtr typs;
      ] |> List.sortByDescending (List.length << snd) // Sort by frequency.
    List.head typeRank // Choose majority.

  // Repair types inappropriate for their offset.
  let private considerOffset curOffset fieldTyp =
    let wordSize = NUInt.toUInt64 NUInt.WORD_SIZE
    let aligneWidth = match curOffset % wordSize with // x86
                      | 0UL -> wordSize
                      | 2UL -> 2UL
                      | 1UL | 3UL -> 1UL
                      | _ -> failwith "Unreachable"
    match fieldTyp with
    | Array _ | Struct _ | String -> fieldTyp // Give up aggeregate types.
    | Scalar width -> Scalar (min aligneWidth width)
    | D3Handle -> if aligneWidth < 4UL then Scalar aligneWidth else D3Handle
    | Handle -> if aligneWidth < wordSize then Scalar aligneWidth else Handle
    | UnknownType | Ptr _ | VoidPtr _ | FuncPtr ->
      if aligneWidth < wordSize then Scalar aligneWidth else fieldTyp


  let private repairOverlapField offset tailOffsets typ =
    match typ, tailOffsets with
    // For the following types, filter out trailing offset that overlaps.
    | Scalar _, _ | Handle, _ | D3Handle, _
    | VoidPtr _, _ | Ptr _, _ | FuncPtr, _ ->
      let size = ArgType.sizeOf typ
      (typ, List.filter (fun o -> o >= offset + size) tailOffsets)
    // We are not fixing the overlap of the rest of the types.
    | Array _, _ | Struct _, _ | String, _ -> (typ, tailOffsets)
    // Leave UnknownType, which will be handled by repairUnknown() during the
    // finalization after the whold type decision process.
    | UnknownType _, _ -> (typ, tailOffsets)

  let rec private repairOverlapsAux origMap accMap offsets =
    match offsets with
    | [] -> accMap
    | off :: tailOffsets ->
      let fldTyp = Map.find off origMap
      let repairedTyp, tailOffsets = repairOverlapField off tailOffsets fldTyp
      let accMap = Map.add off repairedTyp accMap
      repairOverlapsAux origMap accMap tailOffsets

  // Fix struct field types by considering offsets.
  let private repairOverlaps fieldMap =
    let offsets = Map.keys fieldMap |> List.sort // Caution: sorting needed.
    repairOverlapsAux fieldMap Map.empty offsets

  // Absorb character fields that follow string field.
  let rec private repairStringAux isPrevFieldStr accMap = function
    | [] -> accMap
    | (offset, t) :: tailEntries ->
      let canBeChar = ArgType.isStr t || ArgType.isChar t || ArgType.isUnknown t
      if canBeChar && isPrevFieldStr
      then repairStringAux isPrevFieldStr accMap tailEntries // Pass field
      else let accMap = Map.add offset t accMap
           repairStringAux (t = String) accMap tailEntries

  let private repairString fieldMap =
    repairStringAux false Map.empty (Map.toList fieldMap)

  let private selectScalar scalarTyps =
    let getWidth = function
      | Scalar w -> Some w
      | _ -> None
    List.choose getWidth scalarTyps |> List.countBy id |> List.maxBy snd |> fst

  let private selectPtrInOut ptrTyps =
    let getInOut = function
      | Ptr (_, inout) -> Some inout
      | VoidPtr inout -> Some inout
      | _ -> None
    List.choose getInOut ptrTyps |> List.fold InOut.merge IOUnknown

  let rec private selectStruct typs =
    let tryGetSize = function
      | Struct (size, _) -> Some size
      | Array (ByteSize, size, _) -> Some size
      | _ -> None
    let tryGetFieldMap = function
      | Struct (_, fMap) -> Some fMap
      | Array (_, _, elemTyp) -> Some (Map.add 0UL elemTyp Map.empty)
      | t -> Some (Map.add 0UL t Map.empty)
    let size = List.choose tryGetSize typs |> SelectSize.run
    let fieldMaps = List.choose tryGetFieldMap typs
    let getOffsets accSet map = Map.keys map |> Set.ofList |> Set.union accSet
    let offsets = List.fold getOffsets Set.empty fieldMaps
    let fieldMap = Set.fold (selectField fieldMaps) Map.empty offsets
                   |> repairOverlaps |> repairString
    let arrLikeSize = Size.isArgField size || Size.isAdjField size
    let arrLikeFields = ArgType.isArrLikeFields fieldMap
    if arrLikeSize && arrLikeFields then Array (ByteSize, size, Scalar 4UL)
    else Struct (size, fieldMap)

  and private selectField fieldMaps accMap offset =
    let mapper fm = Map.tryFind offset fm |> Option.map (considerOffset offset)
    let fldTypOpts = List.map mapper fieldMaps
    match selectMajorityField fldTypOpts with
    | None -> accMap
    | Some selectedFldTyp -> Map.add offset selectedFldTyp accMap

  and private selectArray typs =
    let tryGetByteSize = function
      | Array (ByteSize, size, _) -> Some size
      | Struct (ArgField (idx, offsets), _) -> Some (ArgField (idx, offsets))
      | Struct (AdjacentField offsets, _) -> Some (AdjacentField offsets)
      | _ -> None
    let tryGetCountSize = function
      | Array (CountSize, size, _) -> Some size
      | _ -> None
    let tryGetElem = function
      | Array (_, _, elemTyp) -> Some elemTyp
      | Struct (_, fieldMap) -> Map.tryFind 0UL fieldMap
      | _ -> None
    let byteSizes = List.choose tryGetByteSize typs
    let cntSizes = List.choose tryGetCountSize typs
    let newElem = List.choose tryGetElem typs |> selectMajorityAux
    if List.length byteSizes > List.length cntSizes
    then Array (ByteSize, SelectSize.run byteSizes, newElem)
    else Array (CountSize, SelectSize.run cntSizes, newElem)

  and private selectPtrElem ptrTyps =
    let getElem = function
      | Ptr (t, _) -> Some t
      | VoidPtr _ -> Some UnknownType
      | _ -> None
    selectMajorityAux (List.choose getElem ptrTyps)

  and private selectMajorityAux typs =
    let typsCount = List.length typs
    let unknownCnt = List.filter ArgType.isUnknown typs |> List.length
    let majorTypeKind, chosenTyps = rankTypes typs // Choose majority.
    if typsCount = unknownCnt then UnknownType // No definitive type.
    else match majorTypeKind with
         | H -> Handle
         | DH -> D3Handle
         | Sc -> Scalar (selectScalar chosenTyps)
         // Not 'chosenTyps' for Struct and Array here, since we will examine
         // the singleton types and consider them as a singleton struct/array.
         | Struc -> selectStruct typs
         | Arr -> selectArray typs
         | Str -> String
         | P -> Ptr (selectPtrElem chosenTyps, selectPtrInOut chosenTyps)
         | F -> FuncPtr

  and private selectMajorityField typOpts =
    let noneCount = List.filter Option.isNone typOpts |> List.length
    let typs = List.choose id typOpts
    let typsCount = List.length typs
    let unknownCnt  = List.filter ArgType.isUnknown typs |> List.length
    let majorTypeKind, chosenTyps = rankTypes typs  // Choose majority.
    if typsCount < noneCount then None // No field at this offset.
    elif typsCount = unknownCnt then Some UnknownType // No definitive type.
    else match majorTypeKind with
         | H -> Some Handle
         | DH -> Some D3Handle
         | Sc -> Some (Scalar (selectScalar chosenTyps))
         // Not 'chosenTyps' for Struct and Array here, since we will examine
         // the singleton types and consider them as a singleton struct/array.
         | Arr -> Some (selectArray typs)
         | Struc -> Some (selectStruct typs)
         | Str -> Some String
         | P -> Some (Ptr (selectPtrElem chosenTyps, selectPtrInOut chosenTyps))
         | F -> Some FuncPtr

  let run typs callegMsg =
    if DEBUG_FLAG then
      logInfo "SelectType.run(Types from %s)" callegMsg
    let selectedTyp = selectMajorityAux (List.map snd typs)
    if DEBUG_FLAG then
      let selectedTypStr = ArgType.toJson true "" selectedTyp
      logInfo "Selected type from %s: %s" callegMsg selectedTypStr
    selectedTyp
