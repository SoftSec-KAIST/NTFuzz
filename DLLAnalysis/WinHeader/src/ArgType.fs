namespace WinHeader

open WinHeader.Constants
open WinHeader.Utils
open WinHeader.C
open WinHeader.TypeHelper

type InOut =
  | In
  | Out
  | InOut
  | IOUnknown

module InOut =
  let toString = function
    | In -> "in"
    | Out -> "out"
    | InOut -> "inout"
    | IOUnknown -> "inout"

  let fromString = function
    | "in" -> In
    | "out" -> Out
    | "inout" -> InOut
    | _ -> failwith "Invalid string input"

  let merge inOut1 inOut2 =
    match inOut1, inOut2 with
    | IOUnknown, _ -> inOut2
    | _, IOUnknown -> inOut1
    | In, In -> In
    | Out, Out -> Out
    | Out, In | In, Out | _, InOut | InOut, _ -> InOut

type ArgType =
  | UnknownType
  | Handle
  | D3Handle
  // Scalar of given width size.
  | Scalar of uint64
  // Null terminated string.
  | String
  // Array of the given element count.
  | Array of SizeKind * Size * ArgType
  | Struct of Size * Map<uint64, ArgType>
  | Ptr of ArgType * InOut
  | VoidPtr of InOut
  | FuncPtr

module ArgType =

  exception VariableSizeException
  exception UnknownWidthException
  exception ResolveException

  let isUnknown = function
    | UnknownType -> true
    | _ -> false

  let isHandle = function
    | Handle -> true
    | _ -> false

  let isD3Handle = function
    | D3Handle -> true
    | _ -> false

  let isDWord = function
    | Scalar n -> n = 4UL
    | _ -> false

  let isScalar = function
    | Scalar _ -> true
    | _ -> false

  let isChar = function
    | Scalar width -> width = 1UL || width = 2UL
    | _ -> false

  let isStr = function
    | String -> true
    | _ -> false

  let isStrLike = function
    | String -> true
    | Array (_, UnknownSize, elemType) -> isChar elemType
    | Struct (_, fieldMap) -> // Caution, no isUnknown here.
      Map.forall (fun _ t -> isChar t || isStr t) fieldMap
    | typ -> isChar typ

  let isArr = function
    | Array _ -> true
    | _ -> false

  let isArrLikeFields fieldMap =
    Map.forall (fun _ t -> isScalar t || isStr t || isUnknown t) fieldMap

  let isArrLike = function
    | Array _ -> true
    | Struct (_, fieldMap) -> isArrLikeFields fieldMap
    | _ -> false

  let isStruct = function
    | Struct _ -> true
    | _ -> false

  let isPtr = function
    | Ptr _ | VoidPtr _ -> true
    | _ -> false

  let isFuncPtr = function
    | FuncPtr -> true
    | _ -> false

  // Remove size information that is dependent on context (e.g. ArgIndex).
  let rec removeCtxSize typ =
    match typ with
    | UnknownType | Handle | D3Handle | Scalar _ | String
    | VoidPtr _ | FuncPtr -> typ
    // Retain 'fixed' size only.
    | Array (sizeKind, Fixed ui64, elemTyp) ->
      Array (sizeKind, Fixed ui64, removeCtxSize elemTyp)
    | Array (sizeKind, _, elemTyp) ->
      Array (sizeKind, UnknownSize, removeCtxSize elemTyp)
    | Struct (Fixed ui64, fieldMap) ->
      let folder accMap off fldTyp = Map.add off (removeCtxSize fldTyp) accMap
      Struct (Fixed ui64, Map.fold folder Map.empty fieldMap)
    | Struct (_, fieldMap) ->
      let folder accMap off fldTyp = Map.add off (removeCtxSize fldTyp) accMap
      Struct (UnknownSize, Map.fold folder Map.empty fieldMap)
    | Ptr (elemTyp, inOut) -> Ptr (removeCtxSize elemTyp, inOut)

  // Remove size information that is dependent on context (e.g. ArgIndex).
  let rec removeInOut typ =
    match typ with
    | UnknownType | Handle | D3Handle | Scalar _ | String
    | VoidPtr _ | FuncPtr -> typ
    | Array (sizeKind, size, elemTyp) ->
      Array (sizeKind, size, removeInOut elemTyp)
    | Struct (size, fieldMap) ->
      let folder accMap off fldTyp = Map.add off (removeInOut fldTyp) accMap
      Struct (size, Map.fold folder Map.empty fieldMap)
    | Ptr (elemTyp, _) -> Ptr (removeInOut elemTyp, IOUnknown)

  let rec sizeOf = function
    | UnknownType -> WORDSIZE // Approximate as word.
    | Handle -> WORDSIZE
    | D3Handle -> 4UL
    | Scalar width -> width
    | String -> raise VariableSizeException
    | Array (_, Fixed size, elemTyp) -> size * sizeOf elemTyp
    | Array _ -> raise VariableSizeException
    | Struct (Fixed size, _) -> size
    | Struct (_, _) ->  raise VariableSizeException
    | Ptr _ | VoidPtr _ | FuncPtr -> WORDSIZE

  let rec widthOf = function
    | UnknownType -> WORDSIZE // Approximate as word.
    | Handle -> WORDSIZE
    | D3Handle -> 4UL
    | Scalar width -> width
    | String -> 2UL
    | Array (_, _, elemTyp) -> sizeOf elemTyp
    | Struct _ -> raise UnknownWidthException
    | Ptr _ | VoidPtr _ | FuncPtr -> WORDSIZE

  let rec toJson debug indent = function
    | UnknownType ->
      if debug then "{\"type\":\"unknown\"}" // For debugging message.
      else "{\"type\":\"scalar\", \"width\":4}" // Defaults to 4-byte integer.
    | Handle -> sprintf "{\"type\":\"handle\", \"width\":%d}" WORDSIZE
    | D3Handle -> "{\"type\":\"handle\", \"width\":4}"
    | Scalar width -> sprintf "{\"type\":\"scalar\", \"width\":%d}" width
    | String -> "{\"type\":\"stringw\"}"
    | Array (sizeKind, size, elemTyp) ->
      let sizeStr = Size.toJson size
      let kindStr = if sizeKind = ByteSize then "\"byte\"" else "\"elem\""
      let elemSizeStr =
        try sprintf "%d" (sizeOf elemTyp) with
        | VariableSizeException -> "-1"
      let elemTypStr = toJson debug (indent + "  ") elemTyp
      let elemTypStrIndnt = "\n" + indent + "  " + elemTypStr + "\n" + indent
      sprintf "{\"type\":\"array\", \"countkind\":%s, " kindStr +
      sprintf "\"size\":%s, " sizeStr +
      sprintf "\"width\":%s, " elemSizeStr +
      sprintf "\"content\":%s}" elemTypStrIndnt
    | Struct (size, fieldMap) ->
      let sizeStr = Size.toJson size
      let fldCnt = Map.count fieldMap
      let fldStr = Map.toList fieldMap
                   |> List.map (fieldToJson debug (indent + "    "))
                   |> String.concat ",\n"
      let fldStrIndnt = if fldStr = "" then "[]"
                        else "[\n" + fldStr + "\n" + indent + "  ]\n" + indent
      sprintf "{\"type\":\"struct\", \"fieldcount\":%d, " fldCnt +
      (if debug then sprintf "\"size\":%s " sizeStr else "") + // DEBUG
      sprintf "\"fields\":%s}" fldStrIndnt
    | Ptr (contentTyp, inOut) ->
      let contentJson = toJson debug (indent + "  ") contentTyp
      let contentStr = "\n" + indent + "  " + contentJson + "\n" + indent
      let inOutStr = sprintf "\"inout\":\"%s\", " (InOut.toString inOut)
      sprintf "{\"type\":\"ptr\", %s\"content\":%s}" inOutStr contentStr
    | VoidPtr inOut -> toJson debug indent (Ptr (UnknownType, inOut))
    | FuncPtr -> "{\"type\":\"funcptr\"}"

  and private fieldToJson debug indent (offset, arg) =
    let argStr = toJson debug indent arg
    sprintf "%s{\"offset\":%d, \"content\":%s}" indent offset argStr

  let private repairUnknown offset tailOffsets typ =
    match typ, tailOffsets with
    // Consider unknown type as scalar, and decide its width with offset info.
    | UnknownType, [] ->
      if offset % WORDSIZE = 0UL then (UnknownType, []) // x86
      elif offset % 2UL = 0UL then (Scalar 2UL, [])
      else (Scalar 1UL, [])
    | UnknownType, nextOffset :: _ ->
      let delta = nextOffset - offset
      let checkDeltaAndOffset n = delta >= n && offset % n = 0UL
      if checkDeltaAndOffset WORDSIZE then (UnknownType, tailOffsets)
      elif checkDeltaAndOffset 2UL then (Scalar 2UL, tailOffsets)
      else (Scalar 1UL, tailOffsets)
    | _ -> (typ, tailOffsets)

  let rec private finalizeFields fieldMap accMap offsets =
    match offsets with
    | [] -> accMap
    | offset :: tailOffsets ->
      let fldTyp = Map.find offset fieldMap
      // Align field type with and drop overlapping offsets.
      let repairedTyp, tailOffsets = repairUnknown offset tailOffsets fldTyp
      let accMap = Map.add offset (finalize repairedTyp) accMap
      finalizeFields fieldMap accMap tailOffsets

  // Finalize type by fixing UnknownType in struct fields and etc.
  and finalize typ =
    match typ with
    | UnknownType | Handle | D3Handle | Scalar _ | String -> typ
    | Array (sizeKind, size, t) -> Array (sizeKind, size, finalize t)
    | Ptr (t, inOut) -> Ptr (finalize t, inOut)
    | Struct (sizeOpt, fieldMap) ->
      let offsets = Map.keys fieldMap |> List.sort // Caution: Sorting needed.
      let newFieldMap = finalizeFields fieldMap Map.empty offsets
      Struct (sizeOpt, newFieldMap)
    | VoidPtr _ | FuncPtr -> typ

  let private addByteBufferSize typ size =
    match typ with
    | VoidPtr inOut -> Ptr (Array (ByteSize, size, UnknownType), inOut)
    // String pointer. Since NULL termination is more precise condition (e.g.
    // buffer size annotation may mean the maximum size allowed), keep it as a
    // string.
    | Ptr (String, _) -> typ
    // Scalar pointer. Sometimes, non-byte scalars are annotated as byte buffer.
    | Ptr (Scalar width, inOut) ->
      Ptr (Array (ByteSize, size, Scalar width), inOut)
    // Handle pointer. Similar case to scalar pointer.
    | Ptr (Handle, inOut) -> Ptr (Array (ByteSize, size, Handle), inOut)
    // Handle pointer. Similar case to scalar pointer.
    | Ptr (D3Handle, inOut) -> Ptr (Array (ByteSize, size, D3Handle), inOut)
    // Void pointer's pointer. Similar case to scalar pointer.
    | Ptr (VoidPtr inOut', inOut)->
      Ptr (Array (ByteSize, size, VoidPtr inOut'), inOut)
    // Struct pointer.
    | Ptr (Struct (_, f), inOut) ->
      // After manual investigation, it turned out that in this case the length
      // specifies the size of the struct itself, rather than a struct array.
      Ptr (Struct (size, f), inOut)
    | _ -> failwithf "Not byte buffer type : %A" typ

  let private addByteBufferPtrSize typ size =
    match typ with
    | Ptr (elemType, inOut) -> Ptr (addByteBufferSize elemType size, inOut)
    | _ -> failwithf "Not byte buffer pointer type : %A" typ

  let private addCountBufferSize typ size =
    match typ with
    | VoidPtr inOut ->
      // It doesn't make sense, but sometimes void pointers are annotated with
      // count size (e.g. WriteConsole()). In this case, consider it byte len.
      //We can't know exact elem, so will be treated as WORD in the end.
      Ptr (Array (ByteSize, size, UnknownType), inOut)
    // String pointers. Since NULL termination is more precise condition (e.g.
    // buffer size annotation may mean the maximum size allowed), keep it as a
    // string.
    | Ptr (String, _) -> typ
    // Any type of array may come. Occurs in struct field declaration.
    | Array (_, _, t) -> Array (CountSize, size, t)
    // Pointer that actually points to array. Occurs in argument delcaration.
    | Ptr (elemType, inOut) -> Ptr (Array (CountSize, size, elemType), inOut)
    | _ -> failwithf "Not count buffer type : %A" typ

  let private addCountBufferPtrSize typ size =
    match typ with
    | Ptr (elemType, inOut) -> Ptr (addCountBufferSize elemType size, inOut)
    | _ -> failwithf "Not count buffer pointer type : %A" typ

  let private reflectSizeAnnot ctags env annot typ =
    match annot with
    | NoSizeAnnot -> typ
    | ByteBuffer (SinglePtr, sizeExp) ->
      let size = Size.ofSizeExp ctags env sizeExp
      addByteBufferSize typ size
    | ByteBuffer (DoublePtr, sizeExp) ->
      let size = Size.ofSizeExp ctags env sizeExp
      addByteBufferPtrSize typ size
    | CountBuffer (SinglePtr, sizeExp) ->
      let size = Size.ofSizeExp ctags env sizeExp
      addCountBufferSize typ size
    | CountBuffer (DoublePtr, sizeExp) ->
      let size = Size.ofSizeExp ctags env sizeExp
      addCountBufferPtrSize typ size

  type NameRule = {
    ArrPrefix : string
    SizeSuffix : string
  }

  let private nameRules =
    [
      { ArrPrefix = ""; SizeSuffix = "size" };
      { ArrPrefix = "p"; SizeSuffix = "size" };
      { ArrPrefix = ""; SizeSuffix = "length" };
      { ArrPrefix = "p"; SizeSuffix = "length" };
    ]

  let private hasMatchingName env (name: string) rule =
    if name.StartsWith(rule.ArrPrefix) then
      let sizeName = name.[rule.ArrPrefix.Length .. ] + rule.SizeSuffix
      Env.contains sizeName env
    else false

  let private applyNameRule env (name: string) rule typ =
    let sizeName = name.[rule.ArrPrefix.Length .. ] + rule.SizeSuffix
    let var, varType = Env.find sizeName env
    if CType.isPtr varType then typ
    else match var with
         | ArgVar idx -> addByteBufferSize typ (ArgField (idx, []))
         | FieldVar offsets -> addByteBufferSize typ (AdjacentField offsets)
         | ArgFieldVar _ -> failwith "'arg.field' name can't be found"

  // XXX. Heuristic inference with field name (disabled by default)
  let private inferSizeFromName env nameOpt mainAnnot auxAnnot typ =
    match nameOpt, mainAnnot, auxAnnot, typ with
    | Some name, NoSizeAnnot, NoSizeAnnot, Ptr (UnknownType, _)
    | Some name, NoSizeAnnot, NoSizeAnnot, VoidPtr _ ->
      (match List.tryFind (hasMatchingName env name) nameRules with
      | None -> typ
      | Some rule -> applyNameRule env name rule typ)
    | _ -> typ

  let private addInOutProperty typ inOut =
    match typ with
    | Ptr (elemType, _) -> Ptr (elemType, inOut)
    | _ -> typ

  let reflectIOAnnotation annot typ =
    match annot with
    | NoIOAnnot -> typ
    | InArg -> addInOutProperty typ In
    | OutArg -> addInOutProperty typ Out
    | InOutArg -> addInOutProperty typ InOut

  let rec ofCType ctags env cType =
    match cType with
    | TypeName _ -> failwith "TypeName should be resolved before conversion."
    | Void -> failwith "Void should be accompanied with Ptr."
    | CFunc -> failwith "CFunc should be accompanied with Ptr."
    | CType.Handle -> ArgType.Handle
    | CType.D3Handle -> ArgType.D3Handle
    | Char | Byte -> Scalar 1UL
    | WChar -> Scalar 2UL
    | Word -> Scalar 2UL
    | DWord -> Scalar 4UL
    | QWord -> Scalar 8UL
    | IntPtr -> Scalar WORDSIZE // Assume casted pointers as simple integers.
    | CStrPtrA -> Ptr (String, IOUnknown)
    | CStrPtrW -> Ptr (String, IOUnknown)
    | CPtr Void -> VoidPtr IOUnknown
    | CPtr CFunc -> FuncPtr
    // Size information will be elaborated later, if annotation is provided.
    | CPtr t -> Ptr (ofCType ctags env t, IOUnknown)
    | CStruct fields when Field.isBitFields fields ->
      let fieldSize = CType.sizeOf (List.head fields).FieldType
      Scalar fieldSize
    | CStruct fields ->
      // First, update variable environment with fields.
      let env = Env.cleanUpFieldVarEntries env |> Env.addFieldEntries fields
      let fieldMap = makeFieldMap ctags env Map.empty 0UL fields
      let entries = Map.toList fieldMap
      let findMapSize (lastOff, lastFld) = Fixed (lastOff + sizeOf lastFld)
      let size = try List.last entries |> findMapSize with _ -> UnknownSize
      Struct (size, fieldMap)
    | CUnion fields ->
      let maxField = List.maxBy CType.sizeOfField fields
      let maxSize = CType.sizeOfField maxField
      let ptrFields = List.filter (fun fld -> CType.isPtr fld.FieldType) fields
      // If there's a pointer field, prefer it (to have more mutation chance).
      if maxSize = WORDSIZE && not (List.isEmpty ptrFields)
      then ofCField ctags env (List.head ptrFields)
      else ofCField ctags env maxField
    | CEnum -> Scalar 4UL

  and makeFieldMap ctags env newMap curOffset fields =
    match fields with
    | [] -> newMap
    | field :: tailFields ->
      let fieldType = ofCField ctags env field
      let curOffset = alignUp curOffset (Field.alignOf field)
      let newMap = Map.add curOffset fieldType newMap
      try // Stop recursion if variable size field is encountered.
        let nextOffset = curOffset + Field.sizeOf field
        makeFieldMap ctags env newMap nextOffset tailFields
      with VariableArraySizeException -> newMap

  and private ofCField (ctags: CtagsMap) env (field: Field) =
    let mainAnnot = field.MainSizeAnnot
    let auxAnnot = field.AuxSizeAnnot
    CType.appendPtrNtimes field.AstrCount field.FieldType
    |> CType.promotePtrToStr mainAnnot auxAnnot
    |> ofCTypeWithArr ctags env field.ArrSize
    |> reflectSizeAnnot ctags env mainAnnot
    |> reflectSizeAnnot ctags env auxAnnot
    // XXX. Heuristic inference with field name. Disabled for the paper, but can
    // be uncommented for more practical use.
    //|> inferSizeFromName env (Some field.FieldName) mainAnnot auxAnnot

  and private ofCTypeWithArr ctags env arrSizeOpt cType =
    match arrSizeOpt, cType with
    | None, _ -> ofCType ctags env cType
    // Assume character array as string unless the size is given as constant.
    | Some exp, WChar | Some exp, Char when not (SizeExp.isConst exp) -> String
    // For other cases, just append array.
    | Some exp, _ ->
      let size = Size.ofSizeExp ctags env exp
      let elemType = ofCType ctags env cType
      Array (CountSize, size, elemType)

  // Promote if a struct if it's actually just a scalar.
  let private promoteStructArg argTyp =
    match argTyp with
    | Struct (_, fieldMap) ->
      let checkScalarStruct o = function
      | Scalar w -> w + o <= WORDSIZE
      | _ -> false
      if Map.forall checkScalarStruct fieldMap then Scalar WORDSIZE
      else failwith "Failed to promote struct argument into a scalar type."
    | _ -> argTyp

  let ofCArg ctags env cArg =
    let mainAnnot = cArg.MainSizeAnnot
    let auxAnnot = cArg.AuxSizeAnnot
    FindType.run ctags cArg.TypeName
    |> CType.appendPtrNtimes cArg.AstrCount
    |> CType.promotePtrToStr mainAnnot auxAnnot
    |> ofCType ctags env
    |> reflectSizeAnnot ctags env mainAnnot
    |> reflectSizeAnnot ctags env auxAnnot
    // XXX. Heuristic inference with argument name. Disabled for the paper,
    // but can be uncommented for more practical use.
    //|> inferSizeFromName env cArg.ArgName mainAnnot auxAnnot
    |> reflectIOAnnotation cArg.IOAnnot
    |> promoteStructArg

  // Helper function for dereferencing array pointer.
  let private isAligned elemTyp offset =
    try offset % sizeOf elemTyp = 0UL with
    | VariableSizeException -> false

  // Dereference an ArgType with given offset, and obtain a new ArgType.
  let rec deref offset typ =
    match typ with
    | UnknownType | Handle | D3Handle | Scalar _ | String ->
      raise ResolveException
    | VoidPtr _ | FuncPtr -> raise ResolveException
    // This is equivalent to dereferencing the first field of the struct.
    | Struct (_, fieldMap) ->
      if Map.containsKey 0UL fieldMap then deref offset (Map.find 0UL fieldMap)
      else raise ResolveException
    // This is equivalent to dereferncing the first element of the array.
    | Array (_, _, elemTyp) -> deref offset elemTyp
    // If it's a pointer to struct, try to dereference a field.
    | Ptr (Struct (_, fieldMap), _) ->
      if Map.containsKey offset fieldMap then Map.find offset fieldMap
      else raise ResolveException
    // If it's a pointer to array, try to dereference an element.
    | Ptr (Array (_, _, elemTyp), _) ->
      if isAligned elemTyp offset then elemTyp
      else raise ResolveException
    // Simple pointer to singleton element.
    | Ptr (elemType, _) when offset = 0UL -> elemType
    | Ptr _ -> raise ResolveException

  let private doubleFieldMap fieldMap =
    // If the last field has variable size, give up inflating.
    let lastOff, lastFld = Map.toList fieldMap |> List.last
    let delta = try lastOff + sizeOf lastFld with VariableSizeException -> 0UL
    let innerFolder accMap o f = Map.add (delta + o) f accMap
    Map.fold innerFolder fieldMap fieldMap

  let makeImprecise typ =
    match typ with
    | Ptr (Array (sizeKind, size, elemTyp), inOut) ->
      Ptr (Array (sizeKind, Size.double size, elemTyp), inOut)
    | Ptr (Struct (size, fieldMap), inOut) ->
      Ptr (Struct (size, doubleFieldMap fieldMap), inOut)
    | Ptr (t, inOut) ->
      let fieldMap = Map.add 0UL t Map.empty
      Ptr (Struct (UnknownSize, doubleFieldMap fieldMap), inOut)
    | _ -> typ
