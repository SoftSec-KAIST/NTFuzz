module WinHeader.TypeHelper

open WinHeader.Utils
open WinHeader.C

type Var =
  | ArgVar of argIndex: int
  | FieldVar of offsets: uint64 list
  | ArgFieldVar of argIndex: int * offsets: uint64 list

// Auxiliary type to help constructing an 'Env'.
type Ctx =
  | NoCtx
  | ArgCtx of argName: string * argIndex : int
  | FieldCtx of names: string list * offsets: uint64 list
  | ArgFieldCtx of names: string list * argIndex: int * offsets: uint64 list

module Ctx =
  let updateCtx ctx fieldName curOffset =
    match ctx with
    | NoCtx -> FieldCtx ([fieldName], [curOffset])
    | ArgCtx (argName, idx) ->
      ArgFieldCtx([argName; fieldName], idx, [curOffset])
    | FieldCtx (names, offsets) ->
      FieldCtx (names @ [fieldName], offsets @ [curOffset])
    | ArgFieldCtx (names, idx, offsets) ->
      ArgFieldCtx (names @ [fieldName], idx, offsets @ [curOffset])

  let fetchNameAndVar ctx fieldName curOffset =
    match ctx with
    | NoCtx -> (fieldName, FieldVar [curOffset])
    | ArgCtx (argName, idx) ->
      argName + "." + fieldName, ArgFieldVar (idx, [curOffset])
    | FieldCtx (names, offsets) ->
      let name = String.concat "." (names @ [fieldName])
      let var = FieldVar (offsets @ [curOffset])
      (name, var)
    | ArgFieldCtx (names, idx, offsets) ->
      let name = String.concat "." (names @ [fieldName])
      let var = ArgFieldVar (idx, offsets @ [curOffset])
      (name, var)

// Auxiliary type to help constructing an 'ArgType'.
type Env = Map<string,Var * CType>

module Env =
  let empty: Env = Map.empty

  let private add (name: string) var (env: Env) =
    Map.add (name.ToLower()) var env

  let contains (name: string) (env: Env) =
    Map.containsKey (name.ToLower())  env

  let find (name: string) (env: Env) =
    Map.find (name.ToLower()) env

  let tryFind (name: string) (env: Env) =
    Map.tryFind (name.ToLower()) env

  let rec private addFieldEntriesAux fields curOffset ctx env =
    match fields with
    | [] -> env
    | field :: tailFields ->
      // First, align the field offset.
      let curOffset = alignUp curOffset (Field.alignOf field)
      // Then, add the current field to the map.
      let varName, var = Ctx.fetchNameAndVar ctx field.FieldName curOffset
      let fieldType = CType.appendPtrNtimes field.AstrCount field.FieldType
      let env = add varName (var, fieldType) env
      // Now, if this field is a struct, add entries recursively.
      let env = match field.FieldType with
                | CStruct innerFields ->
                  let innerCtx = Ctx.updateCtx ctx field.FieldName curOffset
                  addFieldEntriesAux innerFields 0UL innerCtx env
                | _ -> env
      try // Stop recursion if variable size field is encountered.
        let nextOffset = curOffset + Field.sizeOf field
        addFieldEntriesAux tailFields nextOffset ctx env
      with VariableArraySizeException -> env

  let addFieldEntries fields env =
    addFieldEntriesAux fields 0UL NoCtx env

  let private addArgEntriesAux ctags accEnv i cArg =
    let argTyp = FindType.run ctags cArg.TypeName
                 |> CType.appendPtrNtimes cArg.AstrCount
    let accEnv = match cArg.ArgName with
                 | None -> accEnv
                 | Some name -> add name (ArgVar i, argTyp) accEnv
    match argTyp, cArg.ArgName with
    | CStruct fields, Some argName ->
      addFieldEntriesAux fields 0UL (ArgCtx (argName, i)) accEnv
    | _ -> accEnv

  let addArgEntries ctags cArgs env =
    List.foldi (addArgEntriesAux ctags) env cArgs

  let cleanUpFieldVarEntries (env: Env) =
    let isFieldVar = function
      | ArgVar _, _ -> false
      | FieldVar _, _ -> true
      | ArgFieldVar _, _ -> true
    Map.filter (fun _ v -> not (isFieldVar v)) env
