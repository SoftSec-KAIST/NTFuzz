module DLLAnalysis.AbsDom.PPoint

open DLLAnalysis
open DLLAnalysis.AbsDom.Signature

type PPointModule () =
  inherit Elem<PPoint>()

  override __.toString pp =
    PPoint.toString pp

  member __.make addr idx: PPoint =
    PPoint.make addr idx

  member __.getAddr (pp: PPoint) : Addr =
    PPoint.getAddr pp

  member __.incr (pp: PPoint) : PPoint =
    let addr = PPoint.getAddr pp
    let idx = PPoint.getIdx pp
    PPoint.make addr (idx + 1)

let PPoint = PPointModule () // Now we can use 'PPoint' like a module.
