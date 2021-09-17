namespace WinHeader

open System

type Location = {
  Filepath : string
  LineNum : int
}

type CtagsMap = {
  FuncMap : Map<string,Location list>
  StructMap : Map<string,Location list>
  UnionMap : Map<string,Location list>
  EnumMap : Map<string,Location list>
  TypedefMap : Map<string,Location list>
}

module CtagsMap =

  let empty = {
    FuncMap = Map.empty
    StructMap = Map.empty
    UnionMap = Map.empty
    EnumMap = Map.empty
    TypedefMap = Map.empty
  }

  let addFunc func loc ctagsMap =
    let funcMap = ctagsMap.FuncMap
    let newFuncMap = match Map.tryFind func funcMap with
                     | None -> Map.add func [loc] funcMap
                     | Some locs -> Map.add func (loc :: locs) funcMap
    { ctagsMap with FuncMap = newFuncMap }

  let addStruct struc loc ctagsMap =
    let structMap = ctagsMap.StructMap
    let newStructMap = match Map.tryFind struc structMap with
                       | None -> Map.add struc [loc] structMap
                       | Some locs -> Map.add struc (loc :: locs) structMap
    { ctagsMap with StructMap = newStructMap }

  let addUnion union loc ctagsMap =
    let unionMap = ctagsMap.UnionMap
    let newUnionMap = match Map.tryFind union unionMap with
                      | None -> Map.add union [loc] unionMap
                      | Some locs -> Map.add union (loc :: locs) unionMap
    { ctagsMap with UnionMap = newUnionMap }

  let addEnum enum loc ctagsMap =
    let enumMap = ctagsMap.EnumMap
    let newEnumMap = match Map.tryFind enum enumMap with
                     | None -> Map.add enum [loc] enumMap
                     | Some locs -> Map.add enum (loc :: locs) enumMap
    { ctagsMap with EnumMap = newEnumMap }

  let addTypedef typdef loc ctagsMap =
    let typedefMap = ctagsMap.TypedefMap
    let newTypedefMap = match Map.tryFind typdef typedefMap with
                        | None -> Map.add typdef [loc] typedefMap
                        | Some locs -> Map.add typdef (loc :: locs) typedefMap
    { ctagsMap with TypedefMap = newTypedefMap }

  let private initializeAux headerDir accMap (line: string) =
    let emptyChars : char array = [| |] // Ugliness in C#.
    let tokens = line.Split(emptyChars, StringSplitOptions.RemoveEmptyEntries)
    let name = tokens.[0]
    let entryType = tokens.[1]
    if name = "operator" then accMap
    else let filePath = System.IO.Path.Combine(headerDir, tokens.[3])
         let lineNum = try int(tokens.[2]) with
                       :? FormatException -> failwithf "Invalid format: %s" line
         let loc = { Filepath = filePath; LineNum = lineNum }
         if entryType = "prototype" then addFunc name loc accMap
         elif entryType = "struct" then addStruct name loc accMap
         elif entryType = "union" then addUnion name loc accMap
         elif entryType = "enum" then addEnum name loc accMap
         elif entryType = "typedef" then addTypedef name loc accMap
         else failwithf "Invalid ctags entry: %s" line

  let initialize headerDir ctagsFile =
    let lines = System.IO.File.ReadAllText(ctagsFile).Trim().Split([|'\n'|])
    Array.fold (initializeAux headerDir) empty lines

  let lookupFunc funcName ctagsMap =
    match Map.tryFind funcName ctagsMap.FuncMap with
    | None -> []
    | Some locs -> locs

  let lookupStruct strucName ctagsMap =
    match Map.tryFind strucName ctagsMap.StructMap with
    | None -> []
    | Some locs -> locs

  let lookupUnion unionName ctagsMap =
    match Map.tryFind unionName ctagsMap.UnionMap with
    | None -> []
    | Some locs -> locs

  let lookupEnum enumName ctagsMap =
    match Map.tryFind enumName ctagsMap.EnumMap with
    | None -> []
    | Some locs -> locs

  let lookupTypdef typName ctagsMap =
    match Map.tryFind typName ctagsMap.TypedefMap with
    | None -> []
    | Some locs -> locs

  let containsStruct structName ctagsMap =
    Map.containsKey structName ctagsMap.StructMap

  let containsUnion unionName ctagsMap =
    Map.containsKey unionName ctagsMap.UnionMap

  let containsEnum enumName ctagsMap =
    Map.containsKey enumName ctagsMap.EnumMap

  let containsTypedef typName ctagsMap =
    Map.containsKey typName ctagsMap.TypedefMap

  let containsType typName ctagsMap =
    containsStruct typName ctagsMap || containsUnion typName ctagsMap ||
    containsEnum typName ctagsMap || containsTypedef typName ctagsMap