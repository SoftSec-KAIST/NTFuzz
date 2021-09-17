module DLLAnalysis.Utils

open System
open System.Text.RegularExpressions

let private random = System.Random()

// Auxiliary function for randSubset().
let private randomSubsetAux accumSet i =
  let t = random.Next(i + 1) // 't' will be in the range 0 ~ i.
  if Set.contains t accumSet then Set.add i accumSet else Set.add t accumSet

/// Choose random k integers from { 0 .. (n - 1) }, with Floyd's algorithm.
let randomSubset n k =
  if n > k
  then List.fold randomSubsetAux Set.empty (List.ofSeq { (n - k) .. (n - 1) })
  else Set.ofSeq { 0 .. (n - 1) }

let thd (_, _, z) = z

let print (s: string) = Console.Write(s)

let println (s: string) = Console.WriteLine(s)

let private logInternal logType fmt =
  let headerStr = sprintf "[System:%s] " logType
  Printf.kprintf (fun str -> println <| headerStr + str) fmt

let logInfo fmt =
  logInternal "Progress" fmt

let logWarning fmt =
  logInternal "Warning" fmt

let logError fmt =
  logInternal "Error" fmt

let readInput () = System.Console.ReadLine()

let checkFileExist file =
  if not (System.IO.File.Exists(file)) then
    printfn "Target file ('%s') does not exist" file
    exit 1

let checkDirExist dir =
  if not (System.IO.Directory.Exists(dir)) then
    printfn "Target directory ('%s') does not exist" dir
    exit 1

let makeDirectory dir =
  try ignore (System.IO.Directory.CreateDirectory(dir)) with
  | _ -> ()

let private BINARY_EXTS = [".dll"; ".exe"]

let truncateBinExt (file: string) =
  match List.tryFind (fun (s: string) -> file.EndsWith(s)) BINARY_EXTS with
  | None -> failwithf "Binary file with unknown extension : %s" file
  | Some s -> file.[ .. (file.Length - s.Length - 1) ]

let isWinAPIName (name: string) =
  // Should start with an uppercase, and should be alphanumeric.
  let regEx = Regex "^[A-Z][a-zA-Z0-9_]*$"
  regEx.IsMatch name
