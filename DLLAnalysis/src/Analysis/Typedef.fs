namespace DLLAnalysis

open DLLAnalysis
open DLLAnalysis.Const
open DLLAnalysis.FrontEnd
open DLLAnalysis.AbstractDomain
open DLLAnalysis.DataFlow

// Fundamentals types and modules for analysis.

exception ExecutionHaltException
exception AnalysisTimeoutException

/// Kind of analysis.
type AnalysisKind =
  | Dummy
  | Eval
  | All

module AnalysisKind =

  let ofString (s: string) =
    match s.ToLower() with
    | "dummy" -> Dummy
    | "eval" -> Eval
    | "all" -> All
    | _ -> failwithf "Unsupported analysis kind: %s" s

  let isEval = function
    | Dummy -> false
    | Eval -> true
    | All -> false

/// Context and configuration applied globally to the whole analysis.
type GlobalInfo = {
  Verbosity : int // Verbosity level
  EvalMode : bool
  DebugMode : bool // Specifies debug mode.
  CallGraph : CallGraph
}

module GlobalInfo =
  let getVerbosity gInfo = gInfo.Verbosity
  let isEvalMode globInfo = globInfo.EvalMode
  let isDebugMode globInfo = globInfo.DebugMode
  let getCallGraph globInfo = globInfo.CallGraph

  let make verbosity isEval isDebug callGraph =
    { Verbosity = verbosity
      EvalMode = isEval
      DebugMode = isDebug
      CallGraph = callGraph }

/// Context and data applied locally to the current subroutine under analysis.
type LocalInfo = {
  BinInfo : BinInfo
  Subrtn : Subroutine
  CFG : CFG
  Record : Record
  FootPrint : FootPrint
  DataFlow : DataFlow
  TimeoutEvent : System.Threading.AutoResetEvent
}

module LocalInfo =

  let getBinInfo localInfo = localInfo.BinInfo
  let getBinName localInfo = localInfo.BinInfo.BinName
  let getSubrtn localInfo = localInfo.Subrtn
  let getCFG localInfo = localInfo.CFG
  let getRecord localInfo = localInfo.Record
  let getFootPrint localInfo = localInfo.FootPrint
  let getDataFlow localInfo = localInfo.DataFlow
  let checkTimeout localInfo = localInfo.TimeoutEvent.WaitOne(0)

  let make binInfo subrtn cfg record footPrint =
    let event = new System.Threading.AutoResetEvent(false)
    let timer = new System.Timers.Timer(float 3600_000)
    timer.Elapsed.Add (fun _ -> event.Set() |> ignore )
    timer.Start()
    { BinInfo = binInfo
      Subrtn = subrtn
      CFG = cfg
      Record = record
      FootPrint = footPrint
      DataFlow = DataFlow.empty
      TimeoutEvent = event }
