open Argu
open WinHeader
open WinHeader.Constants
open WinHeader.Utils

type CLIArg =
  | [<AltCommandLine("-h")>] Header of string
  | [<AltCommandLine("-f")>] Functions of string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Header _ -> "Directory containing the header data project"
      | Functions _ -> "Input file that contains function list to analyze"

let parseArgs (args: string array) =
  let parser = ArgumentParser.Create<CLIArg> (programName = "Parser.dll")
  let r = try parser.Parse(args) with
          | :? Argu.ArguParseException -> println (parser.PrintUsage()); exit 1
  let headerDir = r.GetResult (<@ Header @>)
  let funcFile = r.GetResult (<@ Functions @>)
  (headerDir, funcFile)


[<EntryPoint>]
let main args =
  let headerDir, funcFile = parseArgs args
  let ctagsPath = System.IO.Path.Combine(headerDir, CTAGS_FILENAME)
  let ctagsMap = CtagsMap.initialize headerDir ctagsPath
  ignore (APIMap.makeWithFile ctagsMap funcFile)
  0
