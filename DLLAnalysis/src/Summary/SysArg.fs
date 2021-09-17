namespace DLLAnalysis.Summary

open WinHeader
open DLLAnalysis
open DLLAnalysis.Const
open DLLAnalysis.Utils
open DLLAnalysis.AbstractDomain
open DLLAnalysis.DataFlow

type SymbolicStruct = {
  StructSize : SymbolicSize
  FieldMap : Map<Offset,SymbolicArg>
}

and SymbolicArg = {
  Value : AbsVal
  Structs : Map<AbsLoc,SymbolicStruct>
  UnresolvedLocs : AbsLocSet
}

module SymbolicArg =

  let bot =
    { Value = AbsVal.bot; Structs = Map.empty; UnresolvedLocs = AbsLocSet.bot }

  let rec toString indent symArg =
    let valStr = AbsVal.toString symArg.Value
    let unresolvedStr = AbsLocSet.toString symArg.UnresolvedLocs
    let strucStr = Map.toList symArg.Structs
                   |> List.map (structToString (indent + "  "))
                   |> String.concat "\n"
    if Map.isEmpty symArg.Structs
    then sprintf "AbsVal: %s, Unresolved: %s" valStr unresolvedStr
    else sprintf "\n%sAbsVal: %s" indent valStr +
         sprintf "\n%sUnresolved: %s" indent unresolvedStr +
         sprintf "\n%sStructs: %s" indent strucStr

  and private structToString indent (baseLoc, struc) =
    let baseStr = AbsLoc.toString baseLoc
    let sizeStr = SymbolicSize.toString struc.StructSize
    let mapper (fldOffset, fldArg) =
      let indent = indent + "  " // Indent once more.
      sprintf "%s%d -> %s" indent fldOffset (toString indent fldArg)
    let mapStr = Map.toList struc.FieldMap
                 |> List.map mapper
                 |> String.concat "\n"
    sprintf "\n%sBase: %s" indent baseStr +
    sprintf "\n%sSize: %s" indent sizeStr +
    sprintf "\n%sMap:\n%s" indent mapStr

  (* Functions related to resolvement under API type information. *)

  let rec private resolveSizeAux withinStruct record sizeArgMap argTyp =
    match argTyp with
    | UnknownType | Handle | D3Handle | Scalar _ | String
    | FuncPtr | VoidPtr _ -> argTyp
    // This case is encountered both in array pointer and array within struct.
    | Array (sizeKind, size, elemTyp) ->
      let newSize = SymbolicSize.resolve withinStruct record sizeArgMap size
      let newElemTyp = resolveSizeAux withinStruct record sizeArgMap elemTyp
      Array (sizeKind, newSize, newElemTyp)
    // This case is encountered only when struct is nested within struct.
    | Struct (size, fieldMap) ->
      let newSize = SymbolicSize.resolve true record sizeArgMap size
      let folder accMap offset fldTyp =
        let newFldTyp = resolveSizeAux true record sizeArgMap fldTyp
        Map.add offset newFldTyp accMap
      let newFieldMap = Map.fold folder Map.empty fieldMap
      Struct (newSize, newFieldMap)
    | Ptr (typ, inOut) ->
      Ptr (resolveSizeAux withinStruct record sizeArgMap typ, inOut)

  /// Resolve size annotation of an ArgType. Here, resolvement means converting
  /// argument index within API function declaration into argument index at the
  /// context (moment) of system call invocation.
  let private resolveSize record sizeArgMap argTyp =
    resolveSizeAux false record sizeArgMap argTyp

  let private resolveSymbolAux apiTyp record sizeArgMap typSym =
    let (argIdx, offsets) = Convert.symbolToArgOffsets typSym record
    match FuncType.findArgType argIdx offsets apiTyp with
    | None -> ArgTypeDom.ofSymbol typSym
    | Some argTyp -> ArgTypeDom.ofArgType (resolveSize record sizeArgMap argTyp)

  /// Resolve a type constraint symbol into ArgType, using API type information.
  let resolveSymbol apiTypOpt record sizeArgMap typSym =
    try
      match apiTypOpt with
      | None -> ArgTypeDom.ofSymbol typSym
      | Some apiTyp ->
        resolveSymbolAux apiTyp record sizeArgMap typSym
    with ConvertException -> ArgTypeDom.ofSymbol typSym

  (* Fuctions related to inference. *)

  let private inferStackFieldsWithHint footPrint hintSize stackLoc =
    let hintSize = NUInt.toNInt hintSize
    FootPrint.getAllLocs footPrint
    |> Set.filter (AbsLoc.isSameBase stackLoc) // To make sure.
    |> Set.filter (fun l -> AbsLoc.findDistance l stackLoc > Offset.ZERO)
    |> Set.filter (fun l -> AbsLoc.findDistance l stackLoc < hintSize)
    |> Set.toList |> List.sortBy AbsLoc.getOffset // To make sure again.

  let rec private chooseAdjacents unusedDefs undefedUses lastLoc = function
    | [] -> []
    | loc :: tailLocs ->
      // Set the threshold with 2 * word_size, to consider alignment issue.
      let threshold = Offset.WORD_SIZE * Offset.TWO
      if AbsLoc.findDistance loc lastLoc >= threshold then []
      elif Set.contains loc unusedDefs || Set.contains loc undefedUses then
        loc :: chooseAdjacents unusedDefs undefedUses loc tailLocs
      else []

  let private inferStackFieldsHeuristic ppoint footPrint dataFlow stackLoc =
    // Note that 'fwState' is before the current stmt, while 'bwState' is after.
    let fwState, bwState = DataFlow.query ppoint dataFlow
    let available, used = fwState.AvailLocs, fwState.UsedLocs
    let curUse = FootPrint.getLoads ppoint footPrint
    let curDef = FootPrint.getStores ppoint footPrint
    let used = Set.union used (FootPrint.getLoads ppoint footPrint)
    let live, willUse = bwState.LiveLocs, bwState.WillUseLocs
    // We should consider the def/use set of current statement, too.
    let unusedDefs = Set.union used willUse
                     |> Set.union curUse
                     |> Set.difference available
    let undefedUses = Set.union available curDef |> Set.difference live
    FootPrint.getAllLocs footPrint
    |> Set.filter (AbsLoc.isSameBase stackLoc) // To make sure.
    |> Set.filter (fun l -> AbsLoc.findDistance l stackLoc > Offset.ZERO)
    |> Set.toList |> List.sortBy AbsLoc.getOffset // To make sure again.
    |> chooseAdjacents unusedDefs undefedUses stackLoc

  let private inferStackFields localInfo hintOpt ppoint stackLoc =
    let locStr = AbsLoc.toString stackLoc
    let ppStr = PPoint.toString ppoint
    let footPrint = LocalInfo.getFootPrint localInfo
    let dataFlow = LocalInfo.getDataFlow localInfo
    match hintOpt with
    | Some hintSize -> inferStackFieldsWithHint footPrint hintSize stackLoc
    | None -> inferStackFieldsHeuristic ppoint footPrint dataFlow stackLoc

  let inferHeapFields hintOpt heapLoc mem =
    AbsMem.findSameBase heapLoc mem
    |> List.filter (fun l -> AbsLoc.findDistance l heapLoc > Offset.ZERO)
    |> (match hintOpt with
       | None -> id
       | Some n ->
        let n = NUInt.toNInt n
        List.filter (fun l -> AbsLoc.findDistance l heapLoc < n))

  // Auxiliary type taht represents struct inference result.
  type private StructInfer =
    | NoStruct
    | Unresolved
    | StructInferred of Fields: AbsLoc list * SymbolicSize

  /// Try to find size and fields of a struct pointed by 'loc'.
  let private inferStructAux localInfo state sizeMap loc =
    let hintOpt = State.tryGetSizeHint loc state
    let mem = State.getAbsMem state
    match loc with
    | AbsLoc.Stack _ ->
      let ppoint = State.getPC state
      let fields = loc :: inferStackFields localInfo hintOpt ppoint loc
      let size = SymbolicSize.inferFromStack hintOpt sizeMap loc fields
      StructInferred (fields, size)
    | AbsLoc.Global _ -> StructInferred ( [loc], SymbolicSize.unknown)
    | AbsLoc.Heap (site, offset, size) ->
      let fields = loc :: inferHeapFields hintOpt loc mem
      let size = SymbolicSize.inferFromHeap hintOpt sizeMap site size offset
      StructInferred (fields, size)
    | AbsLoc.SLoc _ -> Unresolved

  let private isVisited visitedBases loc =
    if AbsLoc.isStack loc then Set.contains loc visitedBases
    elif AbsLoc.isHeap loc then Set.contains (AbsLoc.getBase loc) visitedBases
    else false

  let private addVisited visitedBases loc =
    if AbsLoc.isStack loc then Set.add loc visitedBases
    elif AbsLoc.isHeap loc then Set.add (AbsLoc.getBase loc) visitedBases
    else visitedBases

  /// Infer a symbolic struct argument from an abstract location.
  let private inferStruct localInfo state sizeMap depth l =
    if depth >= STRUCT_DEPTH_LIMIT then NoStruct
    else inferStructAux localInfo state sizeMap l

  let rec private inferLocsAux localInfo state sizeMap depth visited acc l =
    let (accStructs, accUnresolveds) = acc
    match inferStruct localInfo state sizeMap depth l with
    | NoStruct -> (accStructs, accUnresolveds)
    | Unresolved -> (accStructs, Set.add l accUnresolveds)
    | StructInferred (fieldLocs, size) ->
      // Update parameters for terminate condition.
      let depth = depth + 1
      let folder = inferField localInfo state sizeMap depth l
      let fieldMap, _ = List.fold folder (Map.empty, visited) fieldLocs
      let struc = { StructSize = size; FieldMap = fieldMap }
      // Caution: Should remove heap allocation size before using it as map key.
      let keyLoc = AbsLoc.setSize l AbsInt.bot
      (Map.add keyLoc struc accStructs, accUnresolveds)

  /// Infer the shape (struct type) of a set of abstract locations.
  and private inferLocs localInfo state sizeMap depth visited locs =
    let visited = Set.fold addVisited visited locs
    let folder = inferLocsAux localInfo state sizeMap depth visited
    Set.fold folder (Map.empty, Set.empty) locs

  /// Infer the shape (struct type) of an abstract value.
  and private infer localInfo state sizeMap depth visited v =
    let locs = AbsVal.getLoc v |> AbsLocSet.filter (not << isVisited visited)
    let constr = AbsVal.getConstr v
    let curSubrtn = LocalInfo.getSubrtn localInfo
    let record = LocalInfo.getRecord localInfo
    let apiTypOpt = APIInfo.tryFindAPI curSubrtn
    let argTyps = TypeConstr.getArgTypes constr
    let concTyps = ArgTypeSet.leaveConcrete argTyps
    let typSyms = ArgTypeSet.collectSymbols argTyps
    let resolvedTyps = Set.map (resolveSymbol apiTypOpt record sizeMap) typSyms
    let newArgTyps = ArgTypeSet.join concTyps resolvedTyps
    let newConstr = TypeConstr.setArgTypes newArgTyps constr
    let newVal = AbsVal.setConstr newConstr v
    let structs, remLocs = inferLocs localInfo state sizeMap depth visited locs
    { Value = newVal; Structs = structs; UnresolvedLocs = remLocs }

  /// Infer type type of the given field location, and accumulate it to the map.
  and private inferField localInfo state sizeMap depth bLoc acc fLoc =
    let (accFieldMap, accVisited) = acc
    let mem = State.getAbsMem state
    let fVal = AbsMem.find fLoc mem
    let fieldArg = infer localInfo state sizeMap depth accVisited fVal
    // Suppress trace explosion by updating visited bases with the locations in
    // the current field value.
    let accVisited = AbsVal.getLoc fVal |> Set.fold addVisited accVisited
    let offset = AbsLoc.findDistance fLoc bLoc
    if offset < Offset.ZERO then (accFieldMap, accVisited)
    else (Map.add offset fieldArg accFieldMap, accVisited)

  let make localInfo state sizeMap idx argVal =
    infer localInfo state sizeMap 0 Set.empty argVal

  (* Fuctions related to symbol handling. *)

  let rec private collectSymbolsFromStruct symStruct =
    let sizeSyms = SymbolicSize.collectSymbols symStruct.StructSize
    let folder accSyms _ fldArg = Set.union (collectSymbols fldArg) accSyms
    let fieldSyms = Map.fold folder Set.empty symStruct.FieldMap
    // Caution. Do not collect symbols from base locs. Consider them concrete.
    Set.union sizeSyms fieldSyms

  and collectSymbols symArg =
    let valSyms = AbsVal.collectSymbols symArg.Value
    let folder acc _ struc = Set.union (collectSymbolsFromStruct struc) acc
    let structSymbols = Map.fold folder Set.empty symArg.Structs
    let unresolvedSyms = Set.map AbsLoc.getSymbol symArg.UnresolvedLocs
    valSyms |> Set.union structSymbols |> Set.union unresolvedSyms

  let rec private instantiateStruct localInfo state subst sizeMap symStruct =
    let newSize = SymbolicSize.instantiate subst symStruct.StructSize
    let mapper _ fieldArg = instantiate localInfo state subst sizeMap fieldArg
    let newFieldMap = Map.map mapper symStruct.FieldMap
    // Caution. Do not substitute base locs. Consider them concrete.
    { symStruct with StructSize = newSize; FieldMap = newFieldMap }

  and instantiate localInfo state subst sizeMap symArg =
    let curSubrtn = LocalInfo.getSubrtn localInfo
    let record = LocalInfo.getRecord localInfo
    let apiTypOpt = APIInfo.tryFindAPI curSubrtn
    let substVal = Substitution.apply subst symArg.Value
    let substConstr = AbsVal.getConstr substVal
    let substTyps = TypeConstr.getArgTypes substConstr
    let concTyps = ArgTypeSet.filter ArgTypeDom.isConcrete substTyps
    let resolvedTyps = ArgTypeSet.collectSymbols substTyps
                       |> Set.map (resolveSymbol apiTypOpt record sizeMap)
    let newArgTypes = ArgTypeSet.join concTyps resolvedTyps
    let newConstr = TypeConstr.setArgTypes newArgTypes substConstr
    let newVal = AbsVal.setConstr newConstr substVal
    let unresolved = symArg.UnresolvedLocs
    let intSubst = subst.IntSubst
    let locSubst = subst.LocSubst
    let substLocs = AbsLocSet.substitute unresolved intSubst locSubst
    let spawnedStructs, remainLocs =
      inferLocs localInfo state sizeMap 0 Set.empty substLocs
    let mapper _ struc = instantiateStruct localInfo state subst sizeMap struc
    let substStructs = Map.map mapper symArg.Structs
    let folder accMap baseLoc struc = Map.add baseLoc struc accMap
    let newStructs = Map.fold folder substStructs spawnedStructs
    { Value = newVal
      Structs = newStructs
      UnresolvedLocs = remainLocs }
