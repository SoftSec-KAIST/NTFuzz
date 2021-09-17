namespace DLLAnalysis.DataFlow

open DLLAnalysis
open DLLAnalysis.AbsDom.AbsLoc

type ForwardState = {
  AvailLocs : Set<AbsLoc>
  UsedLocs : Set<AbsLoc>
}

module ForwardState =

  let empty = { AvailLocs = Set.empty; UsedLocs = Set.empty }

  let leq s1 s2 =
    Set.isSubset s1.AvailLocs s2.AvailLocs &&
    Set.isSubset s1.UsedLocs s2.UsedLocs

  let join s1 s2 =
    { AvailLocs = Set.union s1.AvailLocs s2.AvailLocs
      UsedLocs = Set.union s1.UsedLocs s2.UsedLocs }

  let findFromMap (pp: PPoint) map : ForwardState =
    match Map.tryFind pp map with
    | None -> empty
    | Some fwState -> fwState

  let update pp footPrint fwState =
    let stores = FootPrint.getStores pp footPrint
    let loads = FootPrint.getLoads pp footPrint
    //printUpdate pp stores loads
    let availLocs = Set.union stores fwState.AvailLocs
    let usedLocs = Set.union loads fwState.UsedLocs
    { AvailLocs = availLocs; UsedLocs = usedLocs }

  let propagate binName succBBs newState map =
    let folder (accWorks, accMap) succ =
      let succPP = BasicBlock.getPPoint binName succ
      let oldState = findFromMap succPP accMap
      if leq newState oldState then (accWorks, accMap)
      else (succ :: accWorks, Map.add succPP (join oldState newState) accMap)
    List.fold folder ([], map) succBBs
