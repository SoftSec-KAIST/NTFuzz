module WinHeader.ExtractType

open System.Text
open System.Text.RegularExpressions
open WinHeader.Constants

exception TypeNotFound

let rec private typeExtractLoop (str: string) idx openCnt (acc: StringBuilder) =
  if str.Length <= idx then None else
    let ch = str.[idx]
    match ch with
    | '{' -> typeExtractLoop str (idx + 1) (openCnt + 1) (acc.Append(ch))
    | '}' -> typeExtractLoop str (idx + 1) (openCnt - 1) (acc.Append(ch))
    | ';' when openCnt = 0 -> Some (acc.Append(ch).ToString())
    | '_' when isSkipClause idx str ->
      let newIdx = skipClause str idx
      typeExtractLoop str newIdx openCnt acc
    | '_' when isSkipArgClause idx str ->
      let keyword, newIdx = skipArgClause str idx
      typeExtractLoop str newIdx openCnt (acc.Append(keyword))
    | _ -> typeExtractLoop str (idx + 1) openCnt (acc.Append(ch))

let private tryFindTypeStr (content: string) (m: Match) =
  let content = content.[m.Index .. ]
  typeExtractLoop content 0 0 (StringBuilder ())

let extractTypedef typ locInfo =
  let filepath = locInfo.Filepath
  let lineNum = locInfo.LineNum
  printfn "Extracting typedef %s from %s" typ filepath
  let lines = System.IO.File.ReadAllLines(filepath)
  // Caution : line nubmer starts from 1, while array index starts from 0.
  let origContent = String.concat "\n" lines
  let content = String.concat "\n" lines.[ .. (lineNum - 1) ]
  let regEx = Regex ("(\n|\s)+typedef\s+" , RegexOptions.RightToLeft)
  let reMatch = regEx.Match(content)
  // Should provide original content for typedef, to handle some tricky cases.
  if not (reMatch.Success) then None
  else (tryFindTypeStr origContent reMatch)

let extractStruct typ locInfo =
  let filepath = locInfo.Filepath
  let lineNum = locInfo.LineNum
  printfn "Extracting struct %s from %s" typ filepath
  let lines = System.IO.File.ReadAllLines(filepath)
  // Caution : line nubmer starts from 1, while array index starts from 0.
  let content = String.concat "\n" lines.[ (lineNum - 1) .. ]
  let regEx = Regex ("struct\s+")
  let reMatch = regEx.Match(content)
  if not (reMatch.Success) then None
  else (tryFindTypeStr content reMatch)

let extractUnion typ locInfo =
  let filepath = locInfo.Filepath
  let lineNum = locInfo.LineNum
  printfn "Extracting union %s from %s" typ filepath
  let lines = System.IO.File.ReadAllLines(filepath)
  // Caution : line nubmer starts from 1, while array index starts from 0.
  let content = String.concat "\n" lines.[ (lineNum - 1) .. ]
  let regEx = Regex ("union\s+")
  let reMatch = regEx.Match(content)
  if not (reMatch.Success) then None
  else (tryFindTypeStr content reMatch)

let extractEnum typ locInfo =
  let filepath = locInfo.Filepath
  let lineNum = locInfo.LineNum
  printfn "Extracting enum %s from %s" typ filepath
  let lines = System.IO.File.ReadAllLines(filepath)
  // Caution : line nubmer starts from 1, while array index starts from 0.
  let content = String.concat "\n" lines.[ (lineNum - 1) .. ]
  let regEx = Regex ("enum\s+")
  let reMatch = regEx.Match(content)
  if not (reMatch.Success) then None
  else (tryFindTypeStr content reMatch)

let runInternal (typ: string) ctagsMap =
  // Try lookup on 'typedef' entry first.
  let locs = CtagsMap.lookupTypdef typ ctagsMap
  let decls = List.choose (extractTypedef typ) locs
  if not (List.isEmpty decls) then // If unfound, try 'struct' or 'enum'.
    decls
  elif CtagsMap.containsStruct typ ctagsMap then
    let locs = CtagsMap.lookupStruct typ ctagsMap
    List.choose (extractStruct typ) locs
  elif CtagsMap.containsUnion typ ctagsMap then
    let locs = CtagsMap.lookupUnion typ ctagsMap
    List.choose (extractUnion typ) locs
  elif CtagsMap.containsEnum typ ctagsMap then
    let locs = CtagsMap.lookupEnum typ ctagsMap
    List.choose (extractEnum typ) locs
  else []

let run typ ctagsMap =
  match runInternal typ ctagsMap with
  | [] -> printfn "Type not found from header: %s (assume void*)" typ
          raise TypeNotFound
  | typeDeclStr :: _ -> typeDeclStr
