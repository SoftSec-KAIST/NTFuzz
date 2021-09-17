module WinHeader.FindType

open FSharp.Text.Lexing
open WinHeader.C
open System.Collections.Generic
open WinHeader.ExtractType

let isCallbackType (typName: string) =
  typName.EndsWith("CALLBACK")

let isPType (typName: string) ctagsMap =
  typName.StartsWith("P") && CtagsMap.containsType typName.[1 .. ] ctagsMap

let isLPType (typName: string) ctagsMap =
  typName.StartsWith("LP") && CtagsMap.containsType typName.[2 .. ] ctagsMap

let isRefType (typName: string) ctagsMap =
  typName.StartsWith("REF") && CtagsMap.containsType typName.[3 .. ] ctagsMap

let rec private extractAndParseTypeAux ctagsMap ctx typName =
  let typeDeclStr = ExtractType.run typName ctagsMap
  let lexBuf = LexBuffer<char>.FromString typeDeclStr
  let typeDecl = CParser.typedecl CLexer.tokenize lexBuf
  let realType = Typedef.retrieveMatchingType typName typeDecl
  resolveType ctagsMap ctx realType

and private extractAndParseType ctagsMap ctx typName =
  if Map.containsKey typName Primitives.types then
    Map.find typName Primitives.types
  else
    try extractAndParseTypeAux ctagsMap ctx typName with
    | TypeNotFound -> printfn "Type %s not found" typName; CPtr Void
    | C.InterfaceException -> printfn "Type %s is interface" typName; CPtr Void

and private runInternal ctagsMap ctx (typName: string) =
  let ctx = typName :: ctx
  if isCallbackType typName then
    CPtr CFunc
  elif isPType typName ctagsMap then
    CPtr (extractAndParseType ctagsMap ctx typName.[1 .. ])
  elif isLPType typName ctagsMap then
    CPtr (extractAndParseType ctagsMap ctx typName.[2 .. ])
  elif isRefType typName ctagsMap then
    CPtr (extractAndParseType ctagsMap ctx typName.[3 .. ])
  else extractAndParseType ctagsMap ctx typName

and private resolveType ctagsMap ctx typ =
  match typ with
  // Caution: Should avoid infinite loop in circular types like linked list.
  | TypeName typName -> if List.contains typName ctx then CPtr Void
                        else runInternal ctagsMap ctx typName
  | Void | CFunc | Handle | D3Handle
  | Char | WChar | Byte | Word | DWord | QWord
  | IntPtr | CStrPtrA | CStrPtrW -> typ
  | CPtr t -> CPtr (resolveType ctagsMap ctx t)
  | CStruct fields -> CStruct (resolveFields ctagsMap ctx fields)
  | CUnion fields -> CUnion (resolveFields ctagsMap ctx fields)
  | CEnum _  -> typ

and private resolveField ctagsMap ctx fld =
  { fld with FieldType = resolveType ctagsMap ctx fld.FieldType }

and private resolveFields ctagsMap ctx fields =
  List.map (resolveField ctagsMap ctx) fields

// Memoization.
let cache = Dictionary<string,CType> ()

let isMemoized typName =
  cache.ContainsKey(typName)

let recall typName =
  cache.[typName]

let memoize typName typ =
  ignore (cache.Add(typName, typ))

// Find the given type.
let run ctagsMap (typName: string) =
  let typName = if typName.EndsWith("%") then typName.[ .. typName.Length - 2]
                else typName
  if isMemoized typName then recall typName
  else let typ = runInternal ctagsMap [] typName
       memoize typName typ
       typ
