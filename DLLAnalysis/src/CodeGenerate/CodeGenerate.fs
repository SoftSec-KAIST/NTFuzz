module DLLAnalysis.CodeGenerate

open Argu
open DLLAnalysis.Utils
open DLLAnalysis.FrontEnd
open DLLAnalysis.CodeGen

type CLIArg =
  | [<AltCommandLine("-d")>] DLLs of string list
  | [<AltCommandLine("-n")>] Ntoskrnl of string list
  | [<AltCommandLine("-h")>] HookerFile of string
  | [<AltCommandLine("-c")>] ConstFile of string

with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | DLLs _ -> "System binary DLL files (ntdll.dll, win32u.dll)"
      | Ntoskrnl _ -> "ntoskrnl binary and PDB (ntoskrnl.exe, ntkrpamp.pdb)"
      | HookerFile _ -> "Output file to store syscall hooker code"
      | ConstFile _ -> "Output file to store version-specific constants"

let private classifyFiles = function
  | [x: string; y: string] ->
    if x.EndsWith(".exe") && y.EndsWith(".pdb") then (x, y)
    elif x.EndsWith(".pdb") && y.EndsWith(".exe") then (y, x)
    else failwith "Invalid file extensions"
  | _ -> failwith "Invalid number of files provided"

let private parseArg (args: string array) =
  let parser = ArgumentParser.Create<CLIArg> (programName = "DLLAnalysis.dll")
  let r = try parser.Parse(args) with
          | :? Argu.ArguParseException -> println(parser.PrintUsage()); exit 1
  let dlls = r.GetResult (<@ DLLs @>)
  let ntosBin, ntosPDB = classifyFiles(r.GetResult (<@ Ntoskrnl @>))
  let hookerFile = r.GetResult (<@ HookerFile @>)
  let constFile = r.GetResult (<@ ConstFile @>)
  dlls, ntosBin, ntosPDB, hookerFile, constFile

let run argv =
  let dlls, ntosBin, ntosPDB, hookerFile, constFile = parseArg argv
  logInfo "Parsing target binaries to construct CFGs..."
  let cfgs = Parser.run dlls |> fst
  logInfo "Identifying system call stubs..."
  StubAnalysis.run cfgs
  logInfo "Generate hooker code for syscalls..."
  HookerCode.generate hookerFile
  logInfo "Generate version-specific constants definition..."
  ConstDef.generate ntosBin ntosPDB constFile
