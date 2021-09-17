namespace WinHeader

open WinHeader.Utils
open WinHeader.Constants
open WinHeader.C
open WinHeader.TypeHelper

type SizeKind =
  | ByteSize
  | CountSize

type Size =
  | UnknownSize
  | Fixed of uint64
  | ArgField of argIndex: int * offsets: uint64 list
  | AdjacentField of offsets: uint64 list
  | Add of Size * Size
  | Sub of Size * Size
  | Mult of Size * Size
  | Div of Size * Size

module Size =

  let isFixed = function
    | Fixed _ -> true
    | _ -> false

  let isArgField = function
    | ArgField _ -> true
    | _ -> false

  let isAdjField = function
    | AdjacentField _ -> true
    | _ -> false

  let isAdd = function
    | Add _ -> true
    | _ -> false

  let isSub = function
    | Sub _ -> true
    | _ -> false

  let isMul = function
    | Mult _ -> true
    | _ -> false

  let rec toJson = function
    | UnknownSize -> "{\"kind\":\"unknown\"}"
    | Fixed ui64 -> sprintf "{\"kind\":\"fixed\", \"val\":%d}" ui64
    | ArgField (idx, offsets) ->
      let offsetStrs = List.map (sprintf "%d") offsets
      let offsetJson = "[" + (String.concat ", " offsetStrs) + "]"
      sprintf "{\"kind\":\"argfield\", \"idx\":%d, \"offsets\":%s}"
        idx offsetJson
    | AdjacentField offsets ->
      let offsetStrs = List.map (sprintf "%d") offsets
      let offsetJson = "[" + (String.concat ", " offsetStrs) + "]"
      sprintf "{\"kind\":\"adjacentfield\", \"offsets\":%s}" offsetJson
    | Add (sz1, sz2) ->
      let s1, s2 = toJson sz1, toJson sz2
      sprintf "{\"kind\":\"add\", \"val1\":%s, \"val2\":%s}" s1 s2
    | Sub (sz1, sz2) ->
      let s1, s2 = toJson sz1, toJson sz2
      sprintf "{\"kind\":\"sub\", \"val1\":%s, \"val2\":%s}" s1 s2
    | Mult (sz1, sz2) ->
      let s1, s2 = toJson sz1, toJson sz2
      sprintf "{\"kind\":\"mult\", \"val1\":%s, \"val2\":%s}" s1 s2
    | Div (sz1, sz2) ->
      let s1, s2 = toJson sz1, toJson sz2
      sprintf "{\"kind\":\"div\", \"val1\":%s, \"val2\":%s}" s1 s2


  let private ofDirectVar name env =
    match Env.tryFind name env with
    | None -> if List.contains name wildcardMacros then UnknownSize
              elif Map.containsKey name macros then Fixed (Map.find name macros)
              else failwithf "Unresolved direct variable: %s" name
    | Some (ArgVar idx, typ) -> // Direct argument, w/o deref.
      if CType.isPtr typ then failwith "Pointer type for DirectVar"
      else ArgField (idx, [])
    | Some (FieldVar offsets, typ) ->
      if CType.isPtr typ then failwith "Pointer type for DirectVar"
      else AdjacentField offsets
    | Some (ArgFieldVar (idx, offsets), typ) ->
      if CType.isPtr typ then failwith "Pointer type for DirectVar"
      else ArgField (idx, offsets)

  let private ofDerefVar name env =
    match Env.tryFind name env with
    | None -> failwithf "Unresolved deref variable: %s" name
    | Some (ArgVar i, typ) ->
      if CType.isSinglePtr typ then ArgField (i, [0UL]) // Dereference once.
      else failwith "Non pointer type for DerefVar"
    | Some (FieldVar _, _) -> failwith "Unexpected deref with field name"
    | Some (ArgFieldVar _, _) -> failwith "Unexpected deref with arg field"

  let rec ofSizeExp ctags env exp =
    match exp with
    | DontKnow -> UnknownSize
    | Const i -> if i <= 0 then Fixed 1UL else Fixed (uint64 i)
    | SizeOf typName ->
      let typ = FindType.run ctags typName
      Fixed (CType.sizeOf typ)
    | DirectVar name -> ofDirectVar name env
    | DerefVar name -> ofDerefVar name env
    | Plus (e1, e2) -> Add (ofSizeExp ctags env e1, ofSizeExp ctags env e2)
    | Minus (e1, e2) -> Sub (ofSizeExp ctags env e1, ofSizeExp ctags env e2)
    | Times (e1, e2) -> Mult (ofSizeExp ctags env e1, ofSizeExp ctags env e2)

  let double size =
    match size with
    | UnknownSize -> UnknownSize
    | Fixed ui64 -> Fixed (ui64 * 2UL)
    | ArgField _ | AdjacentField _ | Add _ | Sub _ | Mult _ | Div _ ->
      Mult (size, Fixed (uint64 2UL))