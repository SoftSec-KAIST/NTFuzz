module DLLAnalysis.TypeInference

open Argu
open DLLAnalysis
open DLLAnalysis.Utils
open DLLAnalysis.FrontEnd
open DLLAnalysis.AbstractSemantics
open DLLAnalysis.Analysis
open DLLAnalysis.TypeAnalysis

type CLIArg =
  | [<AltCommandLine("-b")>] Binary of string list
  | [<AltCommandLine("-o")>] Output of string
  | [<AltCommandLine("-d")>] Dump of string
  | Verbose of int
  | Mode of string
  | Debug
  | Distort of int
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Binary _ -> "Binary files to analyze"
      | Output _ -> "Output file to store type analysis results"
      | Dump _ -> "Output directory to dump type analysis result per syscall"
      | Verbose _ -> "Level of verbosity"
      | Mode _ -> "Specifies analysis mode <dummy|unit|eval|all>"
      | Debug -> "Enter interactive debug mode after analysis"
      | Distort _ -> "Add artificial inaccuracy to type analysis result"

// Intermediate type to store parsed configuration arguments.
type Config = {
  Verbosity : int
  Analysis : AnalysisKind
  Debug : bool
  DistortIterN : int
}

let private parseArg (args: string array) =
  let parser = ArgumentParser.Create<CLIArg> (programName = "DLLAnalysis.dll")
  let r = try parser.Parse(args) with
          | :? Argu.ArguParseException -> println(parser.PrintUsage()); exit 1
  let bins = r.GetResult (<@ Binary @>)
  let outFile = r.GetResult (<@ Output @>)
  let dumpDir = r.GetResult (<@ Dump @>, defaultValue = "")
  let config = {
    Verbosity = r.GetResult(<@ Verbose @>, defaultValue = 0)
    Analysis = AnalysisKind.ofString (r.GetResult (<@ Mode @>))
    Debug = r.Contains (<@ Debug @>)
    DistortIterN = r.GetResult (<@ Distort @>, defaultValue = 0)
  }
  bins, outFile, dumpDir, config

let private isExported binInfos subrtn =
  let bin = Subroutine.binOf subrtn
  let rawAddr = Subroutine.rawAddrOf subrtn
  let binInfo = Map.find bin binInfos
  BinInfo.isExportEntry binInfo rawAddr

let private findExports callGraph binInfos =
  CallGraph.getAllSubrtns callGraph
  |> Set.filter (isExported binInfos)

let private getAPINames subrtns =
  Set.toList subrtns
  |> List.collect Subroutine.toStringAll
  |> List.collect (fun s -> s :: RedirectMap.findAliases s)
  |> List.map Subroutine.fetchName
  |> List.filter isWinAPIName

let private printUncalledStubs callGraph stubs =
  let uncalled = Set.filter (not << (CallGraph.hasCaller callGraph)) stubs
  logInfo "Total %d stubs are uncalled." (Set.count uncalled)
  Set.iter (fun s -> printfn "(Uncalled) %s" (Stub.toString s)) uncalled

let private findModularTargets callGraph stubs targStubs =
  logInfo "Calculating API set..."
  let subrtns = CallGraph.getAllSubrtns callGraph
  let apis = Set.filter APIInfo.isAPI subrtns
  let userAPIs = Set.diff apis stubs
  logInfo "Finding subroutines reachable to targets, until API is met..."
  let reachables = CallGraph.findReachablesToUntil callGraph targStubs userAPIs
  logInfo "%d subroutines reachable to targets!" (Set.count reachables)
  logInfo "Finding callee subroutines (for side effect)..."
  let callees = CallGraph.findReachablesFrom callGraph reachables
  logInfo "%d subroutines called by targets!" (Set.count callees)
  let stubCallees = CallGraph.findReachablesFrom callGraph stubs
  let excludes = Set.union stubs stubCallees
  logInfo "Calculating analysis target set..."
  let targets = Set.diff (Set.union reachables callees) excludes
  logInfo "%d subroutines obtained as target!" (Set.count targets)
  // Caution: We should analyze terminal functions first, so reverse the list.
  List.rev (CallGraph.topoSort callGraph targets)

let private findTargets callGraph stubs analysis =
  match analysis with
  | Dummy -> failwith "Unreachable"
  | Eval -> Set.map Stub.ofString Const.EVALSET_SYSCALLS
            |> findModularTargets callGraph stubs
  | All -> Set.filter (not << APIInfo.isAPI) stubs
           |> findModularTargets callGraph stubs

let private analyzeType cfgs binInfos outFile outDir conf =
  let stubs = StubInfo.getStubs()
  let verbosity = conf.Verbosity
  let analysisKind = conf.Analysis
  let isEval = AnalysisKind.isEval analysisKind
  logInfo "Constructing call graph..."
  let encoded = Encoding.getEncoded cfgs
  let callGraph = CallGraph.build cfgs binInfos encoded stubs
  logInfo "Start extracting API function types from header files..."
  let exports = findExports callGraph binInfos
  // In evaluation mode, assume as if the evaluation targets are undocumented.
  let evalSet = Set.map Stub.ofString Const.EVALSET_SYSCALLS
  let candidates = if isEval then Set.diff exports evalSet else exports
  let candidateNames = getAPINames candidates
  APIInfo.initialize candidateNames
  logInfo "Total %d syscall stubs identified" (Set.count stubs)
  printUncalledStubs callGraph stubs
  logInfo "Deciding analysis targets and their order..."
  let targs = findTargets callGraph stubs analysisKind
  logInfo "Target subroutine list (in analysis order)"
  List.iter (fun s -> printfn "(Target) %s" (Subroutine.toString s)) targs
  logInfo "Running binary scale statistics..."
  Statistics.run cfgs stubs (Set.ofList targs)
  logInfo "Start running modular analysis..."
  let summarize = Summarize.run
  let init = TypeMap.empty
  let globInfo = GlobalInfo.make verbosity isEval conf.Debug callGraph
  let typeMap = Modular.run summarize init globInfo cfgs binInfos targs
  logInfo "Deciding system call type..."
  let typeSpec = DecideType.run analysisKind typeMap
  logInfo "Writing analysis output to file..."
  if outDir <> "" then TypeSpec.writeToSeparateFiles typeSpec outDir
  TypeSpec.writeToFile typeSpec outFile
  if conf.Debug then Debug.debugShell ()
  if conf.DistortIterN <> 0 then Distort.run typeSpec outFile conf.DistortIterN

let private emitDummyType outFile =
  let typeSpec = TypeSpec.empty
                 |> TypeSpec.addMissing
                 |> TypeSpec.removeBlacklisted
  TypeSpec.writeToFile typeSpec outFile

let run argv =
  let bins, outFile, outDir, conf = parseArg argv
  logInfo "Parsing target binaries to construct CFGs..."
  let cfgs, binInfos = Parser.run bins
  logInfo "Identifying system call stubs..."
  StubAnalysis.run cfgs
  match conf.Analysis with
  | Dummy -> emitDummyType outFile
  | Eval | All -> analyzeType cfgs binInfos outFile outDir conf
