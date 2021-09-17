module DLLAnalysis.FrontEnd.CallGraphAnalysis

open B2R2.BinIR.LowUIR
open DLLAnalysis
open DLLAnalysis.FrontEnd

type Value = {
  Addrs : Set<RawAddr>
  Subrtns : Set<Subroutine>
}

module Value =
  let bottom = { Addrs = Set.empty; Subrtns = Set.empty }
  let ofSubrtn subrtn = { Addrs = Set.empty; Subrtns = Set.singleton subrtn }
  let ofSubrtns subrtns = { Addrs = Set.empty; Subrtns = subrtns }
  let ofGlobAddr addr = { Addrs = Set.singleton addr; Subrtns = Set.empty }

type RegMap = Map<Register,Value>

type State = Map<RawPPoint,RegMap>

module State =
  let bottom: State = Map.empty

let private loadAux binInfo acc addr =
  let importMap = BinInfo.getImportMap binInfo
  match Map.tryFind addr importMap with
  | None -> acc
  | Some subrtn -> Set.add subrtn acc

let private load binInfo v =
  let subrtns = Set.fold (loadAux binInfo) Set.empty v.Addrs
  Value.ofSubrtns subrtns

let private assign reg v (regMap: RegMap) =
  Map.add reg v regMap

let private lookup reg (regMap: RegMap) =
  if Map.containsKey reg regMap then Map.find reg regMap
  else Value.bottom

let rec private eval binInfo regMap exp =
  match exp with
  | Num bitv ->
    let rawAddr = RawAddr.ofBitVector bitv
    let addr = Addr.makeWithRawAddr binInfo.BinName rawAddr
    if BinInfo.isSubrtnEntry binInfo rawAddr then Value.ofSubrtn addr
    elif BinInfo.isGlobalAddr binInfo rawAddr then Value.ofGlobAddr rawAddr
    else Value.bottom
  | Var (_, _, reg, _) -> lookup reg regMap
  | TempVar (_, regNum) -> lookup (Register.ofTempVar regNum) regMap
  | Load (_, _, e, _, _) -> load binInfo (eval binInfo regMap e)
  | _ -> Value.bottom

let private addTargets jmpInfo accTargs dstVal =
  let folder acc s = Set.add (s, jmpInfo) acc
  Set.fold folder accTargs dstVal.Subrtns

let private analyzeStmt binInfo (regMap, targs) stmt =
  let stmtOptimized = Optimize.constFold stmt
  match stmtOptimized with
  | Put (Var (_, _, reg, _), exp) ->
    let regMap = assign reg (eval binInfo regMap exp) regMap
    (regMap, targs)
  | Put (TempVar (_, regNum), exp) ->
    let reg = Register.ofTempVar regNum
    let regMap = assign reg (eval binInfo regMap exp) regMap
    (regMap, targs)
  | InterJmp (_, exp, InterJmpInfo.IsCall) ->
    let dst = eval binInfo regMap exp
    let targs = addTargets InterJmpInfo.IsCall targs dst
    let regMap = assign Register.ret Value.bottom regMap
    (regMap, targs)
  | InterJmp (_, exp, InterJmpInfo.Base) ->
    let dst = eval binInfo regMap exp
    let targs = addTargets InterJmpInfo.Base targs dst
    (regMap, targs)
  | _ -> (regMap, targs)

let private analyzeBB binInfo regMap bb =
  let stmts = BasicBlock.getStmts bb
  List.fold (analyzeStmt binInfo) (regMap, Set.empty) stmts

let rec private findCallTargets binInfo accState accTargs = function
  | [] -> accTargs
  | bb :: tailBBs ->
    let ppoint = BasicBlock.getRawPPoint bb
    let regMap = match Map.tryFind ppoint accState with
                 | Some map -> map
                 | None -> Map.empty
    let regMap, callTargets = analyzeBB binInfo regMap bb
    let succs = BasicBlock.getSuccs bb |> List.map BasicBlock.getRawPPoint
    let propagate acc succ = Map.add succ regMap acc
    let accState = List.fold propagate accState succs
    let accTargs = Set.union callTargets accTargs
    findCallTargets binInfo accState accTargs tailBBs

let private checkValidDst cfgs call =
  let addr, _ = call
  if not (Map.containsKey addr cfgs) then
    failwithf "Missing call target %s" (Addr.toString addr)

let private findCallEdges cfgs binInfos accEdges (subrtn: Subroutine) =
  let (cfg: CFG) = Map.find subrtn cfgs
  let binInfo = Map.find (Addr.getBinary subrtn) binInfos
  let bbs = CFG.topologicalSort cfg
  let calls = findCallTargets binInfo State.bottom Set.empty bbs
  Set.iter (checkValidDst cfgs) calls
  let newEdges = Set.map (fun (targAddr, _) -> (subrtn, targAddr)) calls
  Set.union newEdges accEdges

/// Analyze function calls and calculate call edges.
let run cfgs binInfos subrtns =
  Set.fold (findCallEdges cfgs binInfos) Set.empty subrtns
