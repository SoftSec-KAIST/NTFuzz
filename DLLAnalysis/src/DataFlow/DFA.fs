module DLLAnalysis.DataFlow.DFA

open DLLAnalysis
open DLLAnalysis.FrontEnd

let private appendWorkList newWorks works =
  let folder acc bb = if List.contains bb works then acc else bb :: acc
  List.fold folder works newWorks

let rec private runForwardAux binName footPrint fwMap =
  function
  | [] -> fwMap
  | bb :: tailWorks ->
    let entryPP = BasicBlock.getPPoint binName bb
    let inState = ForwardState.findFromMap entryPP fwMap
    let ppoints = BasicBlock.getPPoints binName bb
    let folder (accState, accMap) pp =
      let accMap = Map.add pp accState accMap
      let accState = ForwardState.update pp footPrint accState
      (accState, accMap)
    let outState, fwMap = List.fold folder (inState, fwMap) ppoints
    let succs = BasicBlock.getSuccs bb
    let newWorks, fwMap = ForwardState.propagate binName succs outState fwMap
    let tailWorks = appendWorkList newWorks tailWorks
    runForwardAux binName footPrint fwMap tailWorks

let private runForward binName cfg footPrint =
  let initWorks = CFG.topologicalSort cfg
  runForwardAux binName footPrint Map.empty initWorks

let rec private runBackwardAux binName footPrint bwMap =
  function
  | [] -> bwMap
  | bb :: tailWorks ->
    let exitPP = BasicBlock.getLastPPoint binName bb // Backward
    let inState = BackwardState.findFromMap exitPP bwMap
    let ppoints = BasicBlock.getPPoints binName bb |> List.rev // Backward.
    let folder (accState, accMap) pp =
      let accMap = Map.add pp accState accMap
      let accState = BackwardState.update pp footPrint accState
      (accState, accMap)
    let outState, bwMap = List.fold folder (inState, bwMap) ppoints
    let preds = BasicBlock.getPreds bb
    let newWorks, bwMap = BackwardState.propagate binName preds outState bwMap
    let tailWorks = appendWorkList newWorks tailWorks
    runBackwardAux binName footPrint bwMap tailWorks

let private runBackward binName cfg footPrint =
  let initWorks = CFG.topologicalSort cfg |> List.rev // Backward.
  runBackwardAux binName footPrint Map.empty initWorks

/// Analyze data flow to obtain available locations and live locations at the
/// given program point. Note that ppoint must be at the end of a basic block.
let run binInfo cfg footprint : DataFlow =
  let binName = binInfo.BinName
  let backwardResult = runBackward binName cfg footprint
  let forwardResult = runForward binName cfg footprint
  { Forward = forwardResult; Backward = backwardResult }
