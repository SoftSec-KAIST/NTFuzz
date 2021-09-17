namespace DLLAnalysis.FrontEnd

open B2R2
open B2R2.FrontEnd
open B2R2.BinCorpus
open B2R2.BinIR.LowUIR
open B2R2.BinGraph
open DLLAnalysis
open DLLAnalysis.FrontEnd

/// Basic block type for a call graph (CallCFG).
type SubrtnVertexData (addr) =
  inherit B2R2.BinGraph.BasicBlock ()

  member __.Subrtn = addr

  override __.PPoint = ProgramPoint (0UL, 0)

  override __.Range = AddrRange (0UL, 0UL)

  override __.IsFakeBlock () = false

  override __.ToVisualBlock () = failwith "Not allowed"

type SubrtnVertex = Vertex<SubrtnVertexData>

module SubrtnVertex =

  let getSubrtn (v: SubrtnVertex) =
    v.VData.Subrtn

  let getPreds (v: SubrtnVertex) =
    v.Preds

  let getSuccs (v: SubrtnVertex) =
    v.Succs

type CallType =
  | Static
  | Dynamic

type CallGraph = {
  Graph : ControlFlowGraph<SubrtnVertexData, CallType>
  Subrtns : Set<Subroutine>
  VertexMap : Map<Subroutine,SubrtnVertex>
  CycleEdges : Set<Subroutine * Subroutine>
}

module CallGraph =

  let isCycleEdge callEdge cg =
    Set.contains callEdge cg.CycleEdges

  let isSubrtn addr cg =
    Map.containsKey addr cg.VertexMap

  let getAllSubrtns cg =
    cg.Subrtns

  let getVertex subrtn cg =
    Map.find subrtn cg.VertexMap

  let getCallers cg subrtn =
    let v = getVertex subrtn cg
    let preds = SubrtnVertex.getPreds v
    Set.ofList (List.map SubrtnVertex.getSubrtn preds)

  let hasCaller cg subrtn =
    let v = getVertex subrtn cg
    not (List.isEmpty (SubrtnVertex.getPreds v))

  let getCallersOfSet cg subrtns =
    let folder accSet s = Set.add (getCallers cg s) accSet
    Set.fold folder Set.empty subrtns

  let getCallees cg subrtn =
    let v = getVertex subrtn cg
    let succs = SubrtnVertex.getSuccs v
    Set.ofList (List.map SubrtnVertex.getSubrtn succs)

  let hasCallee cg subrtn =
    let v = getVertex subrtn cg
    not (List.isEmpty (SubrtnVertex.getSuccs v))

  let getCalleesOfSet cg subrtns =
    let folder accSet s = Set.add (getCallees cg s) accSet
    Set.fold folder Set.empty subrtns

  let isSelfRecursive subrtn cg =
    let v = getVertex subrtn cg
    List.contains v (SubrtnVertex.getPreds v)

  let private addVertex accGraph subrtn =
    let vData = SubrtnVertexData(subrtn)
    let vertex = accGraph.Graph.AddVertex vData
    { accGraph with VertexMap = Map.add subrtn vertex accGraph.VertexMap }

  let private addVertices cg subrtns =
    Set.fold addVertex cg subrtns

  let private addEdge cg edge =
    // 'vtxMap' maps a subroutine to its corresponding vertex (a broken design).
    let src, dst = edge
    let vSrcOpt = Map.tryFind src cg.VertexMap
    let vDstOpt = Map.tryFind dst cg.VertexMap
    match vSrcOpt, vDstOpt with
    | Some vSrc, Some vDst -> cg.Graph.AddEdge vSrc vDst Static
    | _ -> ()

  let private addEdges cg edges =
    Set.iter (addEdge cg) edges

  let private removeEdge cg edge =
    let src, dst = edge
    let vSrcOpt = Map.tryFind src cg.VertexMap
    let vDstOpt = Map.tryFind dst cg.VertexMap
    match vSrcOpt, vDstOpt with
    | Some vSrc, Some vDst -> cg.Graph.RemoveEdge vSrc vDst
    | _ -> ()

  let private removeEdges cg edges =
    Set.iter (removeEdge cg) edges

  /// Remove edges that call 'subrtn'.
  let removeCallsTo cg subrtn =
    let callers = getCallers cg subrtn
    let callEdges = Set.map (fun caller -> (caller, subrtn)) callers
    removeEdges cg callEdges

  /// Remove edges that are called by 'subrtn'.
  let removeCallsFrom cg subrtn =
    let callees = getCallees cg subrtn
    let callEdges = Set.map (fun callee -> (subrtn, callee)) callees
    removeEdges cg callEdges

  /// DFS traversal to find backedges.
  let rec private findCycleEdgesAux cg stack (accBackEdges, accVisited) subrtn =
    // Update stack and visited node set.
    let stack = subrtn :: stack
    let accVisited = Set.add subrtn accVisited
    // Now calculate backedges that start from current basic block, 'bb'.
    let succs = getCallees cg subrtn
    let isOnStack subrtn = List.contains subrtn stack
    let newBackEdges = Set.filter isOnStack succs
                       |> Set.map (fun succ -> (subrtn, succ))
    let accBackEdges = Set.union newBackEdges accBackEdges
    // Finally, call recursively on unvisited successor nodes.
    let toVisits = Set.filter (fun s -> not (Set.contains s accVisited)) succs
                   |> Set.toList
                   |> List.sortBy Subroutine.toString // For consistency.
    List.fold (findCycleEdgesAux cg stack) (accBackEdges, accVisited) toVisits

  let findCycleEdges (cg: CallGraph) =
    let initStack = []
    let folder = findCycleEdgesAux cg initStack
    let initAcc = Set.empty, Set.empty
    let subrtns = Set.toList cg.Subrtns
                  |> List.sortBy Subroutine.toString // For consistency.
    List.fold folder initAcc subrtns |> fst

  let build cfgs binInfos encoded stubs =
    let subrtns = Set.ofList (Map.keys cfgs)
    let cg = { Graph = ControlFlowGraph<SubrtnVertexData, CallType>()
               Subrtns = subrtns
               VertexMap = Map.empty
               CycleEdges = Set.empty }
    let cg = addVertices cg subrtns
    let edges = CallGraphAnalysis.run cfgs binInfos subrtns
    addEdges cg edges
    // Refine call graph by removing edges to encoded subroutines. This allows
    // us to obtain a smaller set of SCC (or cycle edges). Note that edges to
    // encoded stubs should be retained, since stub call-site analysis require
    // this. Retaining this edges to not affect cycle detection.
    Set.iter (removeCallsTo cg) (Set.difference encoded stubs)
    let cycleEdges = findCycleEdges cg
    { cg with CycleEdges = cycleEdges }

  let rec topoSortAux targets cg (accVisited, accList) subrtn =
    if not (Set.contains subrtn targets) || Set.contains subrtn accVisited then
      (accVisited, accList) // Skip if not our interest or already visited.
    else // Otherwise, accumulate and perform DFS recursively.
      let succs = getCallees cg subrtn
      let isCycleEdge succ = Set.contains (subrtn, succ) cg.CycleEdges
      let succs = Set.filter (not << isCycleEdge) succs
      // Exclude subroutines that comprise call cycle edges.
      let accVisited = Set.add subrtn accVisited
      let folder = topoSortAux targets cg
      let accVisited, accList = Set.fold folder (accVisited, accList) succs
      (accVisited, subrtn :: accList)

  /// Takes in a set of target subroutines, and return a list of topologically
  /// sorted subroutines.
  let topoSort (cg: CallGraph) targets =
    let initAcc = (Set.empty, [])
    let _, sortedList = Set.fold (topoSortAux targets cg) initAcc targets
    sortedList

  let rec private findReachablesToAux cg workSet accReachables =
    let folder acc subrtn = Set.union (getCallers cg subrtn) acc
    let reachables = Set.fold folder Set.empty workSet
    let isNew subrtn = not (Set.contains subrtn accReachables)
    let newWorkSet = Set.filter isNew reachables
    let accReachables = Set.union reachables accReachables
    if Set.isEmpty newWorkSet then accReachables
    else findReachablesToAux cg newWorkSet accReachables

  /// Find subroutines that are reachable to 'dstSet' transitively.
  let findReachablesTo cg dstSet =
    findReachablesToAux cg dstSet Set.empty

  let rec private findReachablesToUntilAux cg untilSet workSet accReachables =
    let folder acc subrtn = Set.union (getCallers cg subrtn) acc
    let reachables = Set.fold folder Set.empty workSet
    let isNew subrtn = not (Set.contains subrtn accReachables)
    let canContinue subrtn = not (Set.contains subrtn untilSet)
    let newWorkSet = Set.filter (fun s -> isNew s && canContinue s) reachables
    let accReachables = Set.union reachables accReachables
    if Set.isEmpty newWorkSet then accReachables
    else findReachablesToUntilAux cg untilSet newWorkSet accReachables

  /// Find subroutines that are reachable to 'dstSet' transitively, but stop if
  /// subroutines in 'untilSet' are encountered.
  let findReachablesToUntil cg dstSet untilSet =
    findReachablesToUntilAux cg untilSet dstSet Set.empty

  let rec private findReachablesFromAux cg workSet accReachables =
    let folder acc subrtn = Set.union (getCallees cg subrtn) acc
    let reachables = Set.fold folder Set.empty workSet
    let isNew subrtn = not (Set.contains subrtn accReachables)
    let newWorkSet = Set.filter isNew reachables
    let accReachables = Set.union reachables accReachables
    if Set.isEmpty newWorkSet then accReachables
    else findReachablesFromAux cg newWorkSet accReachables

  /// Find subroutines that are reachable from 'srcSet' transitively.
  let findReachablesFrom cg srcSet =
    findReachablesFromAux cg srcSet Set.empty

  let rec private findReachablesFromUntilAux cg untilSet workSet accReachables =
    let folder acc subrtn = Set.union (getCallees cg subrtn) acc
    let reachables = Set.fold folder Set.empty workSet
    let isNew subrtn = not (Set.contains subrtn accReachables)
    let canContinue subrtn = not (Set.contains subrtn untilSet)
    let newWorkSet = Set.filter (fun s -> isNew s && canContinue s) reachables
    let accReachables = Set.union reachables accReachables
    if Set.isEmpty newWorkSet then accReachables
    else findReachablesFromUntilAux cg untilSet newWorkSet accReachables

  /// Find subroutines that are reachable from 'srcSet' transitively.
  let findReachablesFromUntil cg srcSet untilSet =
    findReachablesFromUntilAux cg untilSet srcSet Set.empty
