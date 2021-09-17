namespace DLLAnalysis.FrontEnd

open B2R2
open B2R2.MiddleEnd
open DLLAnalysis

type CFG = {
  Root : BinGraph.Vertex<BinGraph.IRBasicBlock>
  Graph : BinGraph.IRCFG
  BackEdges : Set<RawPPoint * RawPPoint>
}

module CFG =

  /// DFS traversal to find backedges.
  let rec private findBackEdgesAux stack (accBackEdges, accVisited) bb =
    let ppoint = BasicBlock.getRawPPoint bb
    // Update stack and visited node set.
    let stack = ppoint :: stack
    let accVisited = Set.add ppoint accVisited
    // Now calculate backedges that start from current basic block, 'bb'.
    let succs = BasicBlock.getSuccs bb
    let succPPs = List.map BasicBlock.getRawPPoint succs
    let isOnStack p = List.contains p stack
    let newBackEdges = List.filter isOnStack succPPs
                       |> List.map (fun succPP -> (ppoint, succPP))
    let accBackEdges = newBackEdges @ accBackEdges
    // Finally, call recursively on unvisited successor nodes.
    let isVisited b = Set.contains (BasicBlock.getRawPPoint b) accVisited
    let toVisits = List.filter (not << isVisited) succs
    List.fold (findBackEdgesAux stack) (accBackEdges, accVisited) toVisits

  let private findBackedges entryBB =
    findBackEdgesAux [] ([], Set.empty) entryBB |> fst |> Set.ofList

  let make binEssence entry : CFG =
    let ircfg, root = binEssence.SCFG.GetFunctionCFG entry
    { Root = root; Graph = ircfg; BackEdges = findBackedges root }

  let getRoot (cfg: CFG) : DLLAnalysis.BasicBlock =
    cfg.Root

  let foldVertex f acc (cfg: CFG) =
    cfg.Graph.FoldVertex f acc

  let isEntry bb (cfg: CFG) =
    getRoot cfg = bb

  let isBackEdge edge (cfg: CFG) =
    Set.contains edge cfg.BackEdges

  let isTrueBranchEdge (cfg: CFG) src dst =
    match cfg.Graph.TryFindEdge src dst with
    | Some BinGraph.IntraCJmpTrueEdge -> true
    | Some BinGraph.InterCJmpTrueEdge -> true
    | _ -> false

  let isFalseBranchEdge (cfg: CFG) src dst =
    match cfg.Graph.TryFindEdge src dst with
    | Some BinGraph.IntraCJmpFalseEdge -> true
    | Some BinGraph.InterCJmpFalseEdge -> true
    | _ -> false

  let rec private topoSortAux (accVisited, accList) bb =
    let pp = BasicBlock.getRawPPoint bb
    if Set.contains pp accVisited then (accVisited, accList) // Already visited.
    else // Otherwise, accumulate and perform DFS recursively.
      let folder = topoSortAux
      let accVisited = Set.add pp accVisited
      let succs = BasicBlock.getSuccs bb
      let accVisited, accList = List.fold folder (accVisited, accList) succs
      (accVisited, bb :: accList)

  /// Returns a list of topologically sorted basic blocks.
  let topologicalSort cfg : BasicBlock list =
    let entry = getRoot cfg
    let _, sortedList = topoSortAux (Set.empty, []) entry
    sortedList

  let rec private getBBsAux accVisited bb =
    if Set.contains bb accVisited then accVisited // Already visited.
    else // Otherwise, accumulate and perform DFS recursively.
      let accVisited = Set.add bb accVisited
      let succs = BasicBlock.getSuccs bb
      List.fold getBBsAux accVisited succs

  let getBBs cfg: Set<BasicBlock> =
    let entry = getRoot cfg
    getBBsAux Set.empty entry

  let rec private findReachablesToAux cfg workSet accReachables =
    let folder acc bb = Set.union (Set.ofList (BasicBlock.getPreds bb)) acc
    let reachables = Set.fold folder Set.empty workSet
    let isNew bb = not (Set.contains bb accReachables)
    let newWorkSet = Set.filter isNew reachables
    let accReachables = Set.union reachables accReachables
    if Set.isEmpty newWorkSet then accReachables
    else findReachablesToAux cfg newWorkSet accReachables

  /// Find basic blocks that are reachable to 'dstBB' transitively.
  let findReachablesTo cfg dstBB =
    findReachablesToAux cfg (Set.singleton dstBB) Set.empty

  let rec private findReachablesFromAux cfg workSet accReachables =
    let folder acc bb = Set.union (Set.ofList (BasicBlock.getSuccs bb)) acc
    let reachables = Set.fold folder Set.empty workSet
    let isNew bb = not (Set.contains bb accReachables)
    let newWorkSet = Set.filter isNew reachables
    let accReachables = Set.union reachables accReachables
    if Set.isEmpty newWorkSet then accReachables
    else findReachablesFromAux cfg newWorkSet accReachables

  /// Find basic blocks that are reachable from 'srcBB' transitively.
  let findReachablesFrom cfg srcBB =
    findReachablesFromAux cfg (Set.singleton srcBB) Set.empty
