namespace DLLAnalysis.DataFlow

open DLLAnalysis
open DLLAnalysis.AbsDom.AbsLoc

type BackwardState = {
  LiveLocs : Set<AbsLoc>
  WillUseLocs: Set<AbsLoc>
}

module BackwardState =

  let empty = {
    LiveLocs = Set.empty
    WillUseLocs = Set.empty
  }

  let leq s1 s2 =
    Set.isSubset s1.LiveLocs s2.LiveLocs &&
    Set.isSubset s1.WillUseLocs s2.WillUseLocs

  let join s1 s2 =
    { LiveLocs = Set.union s1.LiveLocs s2.LiveLocs
      WillUseLocs = Set.union s1.WillUseLocs s2.WillUseLocs }

  let findFromMap (pp: PPoint) map : BackwardState =
    match Map.tryFind pp map with
    | None -> empty
    | Some bwState -> bwState

  let update pp footPrint bwState =
    let stores = FootPrint.getStores pp footPrint
    let loads = FootPrint.getLoads pp footPrint
    //printUpdate pp stores loads
    let liveLocs = Set.diff (Set.union loads bwState.LiveLocs) stores
    let willUseLocs = Set.union loads bwState.WillUseLocs
    { LiveLocs = liveLocs
      WillUseLocs = willUseLocs }

  let propagate binName preds newState map =
    let folder (accWorks, accMap) pred =
      let predPP = BasicBlock.getLastPPoint binName pred // Backward.
      let oldState = findFromMap predPP accMap
      if leq newState oldState then (accWorks, accMap)
      else (pred :: accWorks, Map.add predPP (join oldState newState) accMap)
    List.fold folder ([], map) preds
