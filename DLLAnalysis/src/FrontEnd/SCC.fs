namespace DLLAnalysis.FrontEnd

open DLLAnalysis
open DLLAnalysis.FrontEnd

type SCC = Set<Subroutine>

type private SCCContext = {
  /// A counter to assign unique ID to each node.
  Counter : int
  /// Maps each node to its unique ID.
  IDMap : Map<Subroutine,int>
  /// Maps each node to its lowest ID of the reachable nodes.
  LowLinkMap : Map<Subroutine,int>
  /// Maps each node to its node stack, at the point of traversing.
  Stack : Stack<Subroutine>
  /// Set of found SCC.
  SCCs : Set<SCC>
}

module private SCCContext =

  let empty = {
    Counter = 0
    IDMap = Map.empty
    LowLinkMap = Map.empty
    Stack = Stack.empty
    SCCs = Set.empty
  }

  let isVisited subrtn ctx =
    Map.containsKey subrtn ctx.IDMap

  let isOnStack subrtn ctx =
    Stack.contains subrtn ctx.Stack

  let isSCCRoot subrtn ctx =
    let id = Map.find subrtn ctx.IDMap
    let lowLink = Map.find subrtn ctx.LowLinkMap
    id = lowLink

  let assignID subrtn ctx =
    let newIDMap = Map.add subrtn ctx.Counter ctx.IDMap
    let newCounter = ctx.Counter + 1
    { ctx with IDMap = newIDMap; Counter = newCounter }

  let initLowLink subrtn ctx =
    let id = Map.find subrtn ctx.IDMap
    let newLowLinkMap = Map.add subrtn id ctx.LowLinkMap
    { ctx with LowLinkMap = newLowLinkMap }

  let push subrtn ctx =
    { ctx with Stack = Stack.push subrtn ctx.Stack }

  let pop ctx =
    let subrtn, newStack = Stack.pop ctx.Stack
    let newCtx = { ctx with Stack = newStack }
    (subrtn, newCtx)

  let updateLowLinkForUnvisited subrtn succ ctx =
    let oldLowLink = Map.find subrtn ctx.LowLinkMap
    let succLowLink = Map.find succ ctx.LowLinkMap
    let newLowLink = min oldLowLink succLowLink
    let newLowLinkMap = Map.add subrtn newLowLink ctx.LowLinkMap
    { ctx with LowLinkMap = newLowLinkMap }

  let updateLowLinkForOnStack subrtn succ ctx =
    let oldLowLink = Map.find subrtn ctx.LowLinkMap
    let succID = Map.find succ ctx.IDMap
    let newLowLink = min oldLowLink succID
    let newLowLinkMap = Map.add subrtn newLowLink ctx.LowLinkMap
    { ctx with LowLinkMap = newLowLinkMap }

  let private isInterestingSCC scc cg =
    Set.count scc > 1 ||
    Set.exists (fun s -> CallGraph.isSelfRecursive s cg) scc

  let rec private updateSCCInfoAux accSCC rootSubrtn cg ctx =
    let poppedSubrtn, ctx = pop ctx
    let accSCC = Set.add poppedSubrtn accSCC
    if poppedSubrtn = rootSubrtn then
      let newSCCs = if isInterestingSCC accSCC cg
                    then Set.add accSCC ctx.SCCs
                    else ctx.SCCs
      { ctx with SCCs = newSCCs }
    else updateSCCInfoAux accSCC rootSubrtn cg ctx

  let updateSCCInfo rootSubrtn cg ctx =
    updateSCCInfoAux Set.empty rootSubrtn cg ctx

module SCCDetection =

  let rec private runAux cg accCtx subrtn  =
    let accCtx = SCCContext.assignID subrtn accCtx
    let accCtx = SCCContext.initLowLink subrtn accCtx
    let accCtx = SCCContext.push subrtn accCtx
    let callees = CallGraph.getCallees cg subrtn
    let folder accCtx succSubrtn =
      if not (SCCContext.isVisited succSubrtn accCtx) then
        runAux cg accCtx succSubrtn
        |> SCCContext.updateLowLinkForUnvisited subrtn succSubrtn
      elif SCCContext.isOnStack succSubrtn accCtx then
        SCCContext.updateLowLinkForOnStack subrtn succSubrtn accCtx
      else accCtx
    let accCtx = Set.fold folder accCtx callees
    if SCCContext.isSCCRoot subrtn accCtx then
      SCCContext.updateSCCInfo subrtn cg accCtx
    else accCtx

  let run (cg: CallGraph) : Set<SCC> =
    let subrtns = cg.Subrtns
    let folder accCtx subrtn =
      if SCCContext.isVisited subrtn accCtx then accCtx
      else runAux cg accCtx subrtn
    let sccContext = Set.fold folder SCCContext.empty subrtns
    sccContext.SCCs
