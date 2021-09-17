namespace DLLAnalysis

open B2R2.FrontEnd
open B2R2.FrontEnd.Intel
open B2R2.BinIR.LowUIR
open B2R2.BinCorpus

/// CFG node that contains the starting PPoint and stmt list.
type BasicBlock = B2R2.BinGraph.Vertex<B2R2.BinGraph.IRBasicBlock>

module BasicBlock =

  let private isFake (bb: BasicBlock) =
    bb.VData.IsFakeBlock()

  let getSuccs (bb: BasicBlock) : BasicBlock list =
    List.filter (not << isFake) bb.Succs

  let getPreds (bb: BasicBlock) : BasicBlock list =
    List.filter (not << isFake) bb.Preds

  let getStmts (bb: BasicBlock) =
    bb.VData.GetIRStatements() |> Array.concat |> Array.toList

  let getInstrs (bb: BasicBlock) =
    bb.VData.GetInstructions ()
    |> Array.map (fun (ins: Instruction) -> ins :?> IntelInstruction)
    |> Array.toList

  // Program point getters.

  let private insInfoToPPoints binName (insInfo: InstructionInfo) =
    let b2r2Addr = insInfo.Instruction.Address
    let addr = Addr.makeWithUI64 binName b2r2Addr
    let irLen = insInfo.Stmts.Length
    Array.init irLen (fun i -> PPoint.make addr i)

  let getPPoints binName (bb: BasicBlock): PPoint list =
    let insInfos = bb.VData.GetInsInfos()
    Array.collect (insInfoToPPoints binName) insInfos |> Array.toList

  let getPPoint binName (bb: BasicBlock): PPoint =
    let b2r2PPoint = bb.VData.PPoint
    let addr = Addr.makeWithUI64 binName b2r2PPoint.Address
    let idx = b2r2PPoint.Position
    PPoint.make addr idx

  let getLastPPoint binName (bb: BasicBlock) =
    let b2r2PPoint = bb.VData.LastPPoint
    let addr = Addr.makeWithUI64 binName b2r2PPoint.Address
    let idx = b2r2PPoint.Position
    PPoint.make addr idx

  let getRawPPoint (bb: BasicBlock) : RawPPoint =
    let b2r2PPoint = bb.VData.PPoint
    let rawAddr = RawAddr.ofUInt64 b2r2PPoint.Address
    let idx = b2r2PPoint.Position
    RawPPoint.make rawAddr idx

  // Address getters.

  let getRawAddrs (bb: BasicBlock) : RawAddr list =
    let insInfos = bb.VData.GetInsInfos()
    let mapper (insInfo: InstructionInfo) = insInfo.Instruction.Address
    Array.map mapper insInfos
    |> Array.map RawAddr.ofUInt64
    |> Array.toList

  // Semantics query.

  let private callsCookieCheck bin bb =
    match List.last (getStmts bb) with
    | InterJmp (_, Num bitv, InterJmpInfo.IsCall) ->
       let addr = (bin, RawAddr.ofBitVector bitv)
       try (Subroutine.toString addr).Contains("__security_check_cookie") with
       | _ -> false
    | _ -> false

  let nextBBClearsStack bin (bb: BasicBlock) =
    match getSuccs bb with
    | [nextBB] ->
      let nextInstrs = getInstrs nextBB
      let clearStack (i: IntelInstruction) = i.IsPop() || i.IsAddStackPtr()
      let incrStack = List.exists clearStack nextInstrs
      let nextLastIns = List.last nextInstrs
      let isRet = nextLastIns.IsRET()
      let isCookieCall = callsCookieCheck bin nextBB
      incrStack && not isRet && not isCookieCall
    | _ -> false
