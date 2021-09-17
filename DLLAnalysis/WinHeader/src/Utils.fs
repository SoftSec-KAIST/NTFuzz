module WinHeader.Utils

open System.Text.RegularExpressions

/// Align x with y, upward.
let alignUp x y =
  if not (List.contains y [1UL; 2UL; 4UL; 8UL; 16UL; 32UL; 64UL; 128UL]) then
    failwithf "[Invalid] Aligning by %d" y
  if x % y = 0UL then x
  else x - (x &&& (y - 1UL)) + y

let println (s: string) = System.Console.WriteLine(s)

let checkFileExists file =
  if not (System.IO.File.Exists(file)) then
    printfn "Target file ('%s') does not exist" file
    exit 1

let checkDirExists dir =
  if not (System.IO.Directory.Exists(dir)) then
    printfn "Target directory ('%s') does not exist" dir
    exit 1

let isValidFuncName (str: string) =
  let regEx = Regex "^[a-zA-Z0-9_]*$"
  regEx.IsMatch str

let rec private findParenCloseIdxAux (str: string) idx openCnt =
  if str.Length <= idx then -1 else
    let ch = str.[idx]
    match ch with
    | '(' -> findParenCloseIdxAux str (idx + 1) (openCnt + 1)
    | ')' when openCnt <= 1 -> idx
    | ')' -> findParenCloseIdxAux str (idx + 1) (openCnt - 1)
    | _ -> findParenCloseIdxAux str (idx + 1) openCnt

let rec findParenCloseIdx (str: string) =
  if str.[0] = ' ' then 1 + findParenCloseIdx  str.[1 .. ]
  elif str.[0] <> '(' then failwith "string does not start with left-paren"
  else findParenCloseIdxAux str 0 0

// Module extensions.

module List =
  let foldi f acc list =
    let folder (accN, accRes) elem = (accN + 1, f accRes accN elem)
    List.fold folder (0, acc) list |> snd

module Map =

  let foldi f acc map =
    let folder (accN, accRes) k v = (accN + 1, f accRes accN k v)
    Map.fold folder (0, acc) map |> snd

  let keys map =
    Map.fold (fun acc k _ -> k :: acc) [] map
