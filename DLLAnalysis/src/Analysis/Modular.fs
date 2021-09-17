module DLLAnalysis.Analysis.Modular

open B2R2.BinIR.LowUIR
open DLLAnalysis
open DLLAnalysis.Utils
open DLLAnalysis.FrontEnd
open DLLAnalysis.AbstractDomain
open DLLAnalysis.DataFlow
open DLLAnalysis.Summary
open DLLAnalysis.AbstractSemantics.Eval
open DLLAnalysis.AbstractSemantics.Exec
open DLLAnalysis.AbstractSemantics.Call

// Auxiliary type that represented information collected from 'call-spots'.
type CallItem = {
  Target: Subroutine
  CallerBB : BasicBlock
  CallerState : State
}

let private findRets globInfo localInfo summaries stateMap =
  let cfg = LocalInfo.getCFG localInfo
  CFG.foldVertex (fun accStates bb ->
    let binName = LocalInfo.getBinName localInfo
    let ppoint = BasicBlock.getPPoint binName bb
    let inState = StateMap.find ppoint stateMap
    // State can be bot when the execution halted in the predecessor.
    if State.isBot inState then accStates
    else match execBB globInfo localInfo summaries inState bb with
         | ExecHalt -> accStates
         | Ret state -> state :: accStates
         | CondJump _ -> accStates
         | NormalExec _ -> accStates
  ) [] cfg

// Functions to collect abstract states at call instructions.

let rec private findCallFromInterJmp globInfo localInfo bb state targSubrtn =
  let callEdge = (LocalInfo.getSubrtn localInfo, targSubrtn)
  let callGraph = GlobalInfo.getCallGraph globInfo
  if CallGraph.isCycleEdge callEdge callGraph then None
  else Some { Target = targSubrtn; CallerState = state; CallerBB = bb }

let private findCallFromStmt globInfo localInfo bb state stmt =
  let stmtOptimized = Optimize.constFold stmt
  match stmtOptimized with
  | InterJmp (_, dstExp, InterJmpInfo.IsCall) ->
    AbsVal.getFunc (eval localInfo state dstExp)
    |> FunctionSet.toList
    |> List.choose getJmpTarget
    |> List.choose (findCallFromInterJmp globInfo localInfo bb state)
  | InterJmp (_, dstExp, InterJmpInfo.Base) ->
    let callGraph = GlobalInfo.getCallGraph globInfo
    AbsVal.getFunc (eval localInfo state dstExp)
    |> FunctionSet.toList
    |> List.choose getJmpTarget
    |> List.filter (fun t -> CallGraph.isSubrtn t callGraph)
    |> List.choose (findCallFromInterJmp globInfo localInfo bb state)
  | _ -> []

let rec private findCallFromStmts globInfo localInfo bb summaries state =
  function
  | [] -> []
  | stmt :: tailStmts ->
    let calls = findCallFromStmt globInfo localInfo bb state stmt
    if not (List.isEmpty calls) then calls
    else match execStmt globInfo localInfo summaries state bb stmt with
         | ExecHalt -> []
         | Ret _ -> []
         | CondJump _ -> []
         | NormalExec state ->
           let state = State.incrPC state
           findCallFromStmts globInfo localInfo bb summaries state tailStmts

let private findCallFromBB globInfo localInfo summaries state bb =
  let binName = LocalInfo.getBinName localInfo
  let ppoint = BasicBlock.getPPoint binName bb
  let state = State.setPC state ppoint
  let stmts = BasicBlock.getStmts bb
  findCallFromStmts globInfo localInfo bb summaries state stmts

let private findCalls globInfo localInfo summaries stateMap =
  let cfg = LocalInfo.getCFG localInfo
  CFG.foldVertex (fun accCalls bb ->
    let binName = LocalInfo.getBinName localInfo
    let ppoint = BasicBlock.getPPoint binName bb
    let inState = StateMap.find ppoint stateMap
    // State can be bot when the execution halted in the predecessor.
    if State.isBot inState then accCalls
    else findCallFromBB globInfo localInfo summaries inState bb @ accCalls
  ) [] cfg

let private summarizeSideEffect localInfo retStates =
  match retStates with
  | [] -> SideEffect.abort // No-return function.
  | headRet :: tailRets -> // Caution: Should NOT joinWithRecord on State.bot
    let record = LocalInfo.getRecord localInfo
    let retState = List.fold (State.joinWithRecord record) headRet tailRets
    SideEffect.find localInfo retState

let private analyzeAux summarizer accum globInfo cfg binInfo summaries f =
  let record = Record.create()
  let footPrint = FootPrint.create()
  let localInfo = LocalInfo.make binInfo f cfg record footPrint
  let stateMap = AbstractInterpretation.run globInfo localInfo summaries
  let dataFlow = DFA.run binInfo cfg footPrint
  let localInfo = { localInfo with DataFlow = dataFlow }
  let retStates = findRets globInfo localInfo summaries stateMap
  let callItems = findCalls globInfo localInfo summaries stateMap
  if GlobalInfo.isDebugMode globInfo then Debug.dumpRecord f record
  let sideEffect = summarizeSideEffect localInfo retStates
  let accum, syscalls = summarizer accum globInfo localInfo summaries callItems
  (accum, Summary.make sideEffect syscalls record)

let private analyze summarizer accum globInfo cfg binInfo summaries f =
  try analyzeAux summarizer accum globInfo cfg binInfo summaries f with
  | AnalysisTimeoutException -> // Not expected, but just in case.
    logError "Timeout during analysis"
    (accum, Summary.unknown)
  | InvalidStackPtrException ->
    logWarning "Stack pointer error"
    (accum, Summary.unknown)

let rec runAux summarizer accum globInfo cfgs binInfos summaries total n =
  function
  | [] -> accum
  | subrtn :: tailSubrtns ->
    let bin = Addr.getBinary subrtn
    let binInfo = Map.find bin binInfos
    let cfg = Map.find subrtn cfgs
    let subrtnStr = Subroutine.toString subrtn
    printfn "Analyzing %s (%d / %d)" subrtnStr n total
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let result = analyze summarizer accum globInfo cfg binInfo summaries subrtn
    let accum, summary = result
    stopWatch.Stop()
    let elapsed = stopWatch.Elapsed.TotalSeconds
    printfn "Finished analysis of %s (%.3f sec.)" subrtnStr elapsed
    if GlobalInfo.getVerbosity globInfo >= 2 then
      println (Summary.toString summary)
    let summaries = SummaryMap.add subrtn summary summaries
    let n = n + 1
    runAux summarizer accum globInfo cfgs binInfos summaries total n tailSubrtns

let run summarizer init globInfo cfgs binInfos subrtns =
  let initSummaries = SummaryMap.empty
  let totalN = List.length subrtns
  runAux summarizer init globInfo cfgs binInfos initSummaries totalN 1 subrtns
