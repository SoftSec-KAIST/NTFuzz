module DLLAnalysis.Analysis.AbstractInterpretation

open DLLAnalysis
open DLLAnalysis.FrontEnd
open DLLAnalysis.AbstractDomain
open DLLAnalysis.AbstractSemantics.Exec

type WorkList = BasicBlock list // Basic blocks that should be analyzed.

let private makePropagteItems cfg bb execResult succs =
  match execResult with
  | ExecHalt -> []
  | Ret _ -> [] // Do not propagate (consider call with 'jmp' instruction).
  | NormalExec newState -> List.map (fun succ -> (succ, newState)) succs
  | CondJump (trueState, falseState) ->
    let trueBranchSuccs = List.filter (CFG.isTrueBranchEdge cfg bb) succs
    let falseBranchSuccs = List.filter (CFG.isFalseBranchEdge cfg bb) succs
    let trueItems = List.map (fun succ -> (succ, trueState)) trueBranchSuccs
    let falseItems = List.map (fun succ -> (succ, falseState)) falseBranchSuccs
    trueItems @ falseItems

let private propagateFolder binName record accStateMap (succBB, newState) =
  let succPPoint = BasicBlock.getPPoint binName succBB
  // If propagate destination does not have abstract state yet, update it w/o
  // joining states, in order to improve the precision of analsis.
  if StateMap.contains succPPoint accStateMap then
    let oldState = StateMap.find succPPoint accStateMap
    let joinedState = State.joinWithRecord record oldState newState
    StateMap.add succPPoint joinedState accStateMap
  else StateMap.add succPPoint newState accStateMap

let private propagate localInfo bb stateMap execResult =
  let cfg = LocalInfo.getCFG localInfo
  let binName = LocalInfo.getBinName localInfo
  let record = LocalInfo.getRecord localInfo
  let ppoint = BasicBlock.getRawPPoint bb
  let isBackEdgeNode n = CFG.isBackEdge (ppoint, BasicBlock.getRawPPoint n) cfg
  let succs = List.filter (not << isBackEdgeNode) (BasicBlock.getSuccs bb)
  let propagateItems = makePropagteItems cfg bb execResult succs
  List.fold (propagateFolder binName record) stateMap propagateItems

// Find a fixpoint for abstract interpretation framework.
let rec private fixpoint globInfo localInfo summaryMap stateMap =
  function
  | [] -> stateMap
  | (work as bb) :: works -> // Our 'work' item is basic block.
    let binName = LocalInfo.getBinName localInfo
    let ppoint = BasicBlock.getPPoint binName bb
    let inState = StateMap.find ppoint stateMap
    // State can be bot when the execution halted in the predecessor.
    if State.isBot inState then
      fixpoint globInfo localInfo summaryMap stateMap works
    else
      let execRes = execBB globInfo localInfo summaryMap inState bb
      let stateMap = propagate localInfo bb stateMap execRes
      fixpoint globInfo localInfo summaryMap stateMap works

let run globInfo localInfo summaryMap =
  let cfg = LocalInfo.getCFG localInfo
  let binInfo = LocalInfo.getBinInfo localInfo
  let curSubrtn = LocalInfo.getSubrtn localInfo
  let initState = State.initialize binInfo curSubrtn
  let entryBB = CFG.getRoot cfg
  let entryPPoint = BasicBlock.getPPoint binInfo.BinName entryBB
  let stateMap = StateMap.add entryPPoint initState StateMap.bot
  let works = CFG.topologicalSort cfg
  fixpoint globInfo localInfo summaryMap stateMap works
