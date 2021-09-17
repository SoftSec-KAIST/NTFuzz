module WinHeader.ExtractFunction

open System.Text
open System.Text.RegularExpressions
open WinHeader.Utils
open WinHeader.Constants

let rec private argExtractLoop (str: string) idx openCnt (acc: StringBuilder) =
  if str.Length <= idx then None else
    let ch = str.[idx]
    match ch with
    | '(' -> argExtractLoop str (idx + 1) (openCnt + 1) (acc.Append(ch))
    | ')' when openCnt <= 1 -> Some (acc.Append(ch).ToString())
    | ')' -> argExtractLoop str (idx + 1) (openCnt - 1) (acc.Append(ch))
    | '_' when isSkipClause idx str ->
      let newIdx = skipClause str idx
      argExtractLoop str newIdx openCnt acc
    | '_' when isSkipArgClause idx str ->
      let keyword, newIdx = skipArgClause str idx
      argExtractLoop str newIdx openCnt (acc.Append(keyword))
    | _ -> argExtractLoop str (idx + 1) openCnt (acc.Append(ch))

let private trimParenth (str: string) =
  if not (str.StartsWith('(')) then failwith "First char is not left-paren"
  elif not (str.EndsWith(')')) then failwith "Last char is not right-paren"
  else str.[1 .. (str.Length - 2)]

let private tryFindArgStr (content: string) (m: Match) =
  let content = content.[m.Index + m.Length - 1 .. ]
  if content.[0] <> '(' then failwith "string does not start with left-paren"
  argExtractLoop content 0 0 (StringBuilder ()) |> Option.map trimParenth

let private extractFuncInternal func filepath lineNum =
  printfn "Extracting function %s from %s" func filepath
  let lines = System.IO.File.ReadAllLines(filepath)
  // Caution : line nubmer starts from 1, while array index starts from 0.
  let content = String.concat "\n" lines.[ (lineNum - 1) .. ]
  let regEx = Regex (sprintf "\s*%s\s*\(" func)
  let reMatch = regEx.Match(content)
  if not (reMatch.Success) then None
  else (tryFindArgStr content reMatch)

let private isBlackListedFile (file: string) =
  List.exists (fun (item: string) -> file.Contains(item)) Constants.blacklist

let private extractFunc func locInfo =
  let filepath = locInfo.Filepath
  let lineNum = locInfo.LineNum
  if isBlackListedFile filepath then None
  else extractFuncInternal func filepath lineNum

let runInternal func ctagsMap =
  if not (isValidFuncName func) then []
  else let locs = CtagsMap.lookupFunc func ctagsMap
       List.choose (extractFunc func) locs

let run func ctagsMap =
  match runInternal func ctagsMap with
  | [] -> None
  | funcDeclStr :: _ -> Some funcDeclStr