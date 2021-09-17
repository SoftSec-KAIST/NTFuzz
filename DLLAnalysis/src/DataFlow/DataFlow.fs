namespace DLLAnalysis.DataFlow

open DLLAnalysis

type DataFlow = {
  Forward : Map<PPoint,ForwardState>
  Backward : Map<PPoint,BackwardState>
}

module DataFlow =

  let empty = { Forward = Map.empty; Backward = Map.empty }

  let query ppoint (res: DataFlow) =
    match Map.tryFind ppoint res.Forward, Map.tryFind ppoint res.Backward with
    | Some fwState, Some bwState -> (fwState, bwState)
    | _ -> failwithf "DFA result for %s not found" (PPoint.toString ppoint)
