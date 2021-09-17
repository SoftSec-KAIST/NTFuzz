module DLLAnalysis.AbstractSemantics.Exec

open B2R2.BinIR
open B2R2.BinIR.LowUIR
open DLLAnalysis
open DLLAnalysis.Utils
open DLLAnalysis.FrontEnd
open DLLAnalysis.AbstractDomain
open DLLAnalysis.AbstractSemantics.Eval
open DLLAnalysis.AbstractSemantics.Assign
open DLLAnalysis.AbstractSemantics.Call
open DLLAnalysis.AbstractSemantics.CondJump

/// Result of applying abstract semantics.
type ExecResult =
  | ExecHalt
  | Ret of State
  | NormalExec of State
  | CondJump of State * State

let private optimizedMemcpy localInfo state oprndSize =
  let dstVal = State.read Register.strDst state
  let dstBases = AbsVal.getLoc dstVal
  let srcVal = State.read Register.strSrc state
  let srcBases = AbsVal.getLoc srcVal
  let countVal = State.read Register.strCnt state
  let countInt = AbsVal.getInt countVal
  let sizeInt = AbsInt.mul countInt (AbsInt.ofNUInt oprndSize)
  MemFunc.memcpy localInfo dstBases srcBases sizeInt state

let private optimizedMemset localInfo state oprndSize =
  let dstVal = State.read Register.strDst state
  let dstBases = AbsVal.getLoc dstVal
  let setVal = State.read Register.strVal state
  let setInt = AbsVal.getInt setVal
  let countVal = State.read Register.strCnt state
  let countInt = AbsVal.getInt countVal
  let sizeInt = AbsInt.mul countInt (AbsInt.ofNUInt oprndSize)
  MemFunc.memset localInfo dstBases setInt sizeInt state

let private runOptimizedMemFunc (label: string) localInfo state =
  let idx = label.LastIndexOf('_')
  let suffix = label.[idx + 1 ..]
  let oprndSize = System.UInt64.Parse suffix |> NUInt.ofUInt64
  if label.StartsWith("memcpy") then optimizedMemcpy localInfo state oprndSize
  elif label.StartsWith("memset") then optimizedMemset localInfo state oprndSize
  else failwith "Unreachable"

let private isOptimizedMemFunc (label: string) =
  label.StartsWith("memcpy") || label.StartsWith("memset")

let private store addrExp valExp localInfo state =
  let record = LocalInfo.getRecord localInfo
  let footPrint = LocalInfo.getFootPrint localInfo
  let srcVar = decideSrcVar localInfo state valExp
  let dstLocs = AbsVal.getLoc (eval localInfo state addrExp)
  let value = eval localInfo state valExp
  State.store (Some footPrint) record srcVar dstLocs value state

let private call globInfo localInfo summaryMap state bb targs =
  let chooser = runSubrtn globInfo localInfo summaryMap state bb
  let states = Set.choose chooser targs |> Set.toList
  if Set.isEmpty targs then NormalExec (runUnknown localInfo bb state)
  elif List.isEmpty states then ExecHalt
  else let record = LocalInfo.getRecord localInfo
       NormalExec (State.multipleJoinWithRecord record states)

let private callAndRet globInfo localInfo summaryMap state bb targs =
  let chooser = runSubrtn globInfo localInfo summaryMap state bb
  let states = Set.choose chooser targs |> Set.toList
  if Set.isEmpty targs then Ret (runUnknown localInfo bb state)
  elif List.isEmpty states then ExecHalt
  else let record = LocalInfo.getRecord localInfo
       Ret (State.multipleJoinWithRecord record states)

let execStmt globInfo localInfo summaryMap state bb stmt =
  if LocalInfo.checkTimeout localInfo then raise AnalysisTimeoutException
  let verbosity = GlobalInfo.getVerbosity globInfo
  let stmtOptimized = Optimize.constFold stmt // XXX : Move to B2R2
  if verbosity >= 4 then
    logInfo "Executing %s" (Pp.stmtToString stmt)
  if verbosity >= 3 && stmt <> stmtOptimized then
    logInfo "(Optimized)"
    logInfo "Before : %s" (Pp.stmtToString stmt)
    logInfo "After : %s" (Pp.stmtToString stmtOptimized)
  match stmtOptimized with
  | ISMark (ui64, _) ->
    let binName = LocalInfo.getBinName localInfo
    let addr = Addr.makeWithUI64 binName ui64
    let ppoint = PPoint.make addr 0
    if GlobalInfo.isDebugMode globInfo then Debug.dumpState addr state
    NormalExec (State.setPC state ppoint)
  | IEMark _ -> NormalExec state
  | LMark (label, _) ->
    if isOptimizedMemFunc label
    then NormalExec (runOptimizedMemFunc label localInfo state)
    else NormalExec state
  | Put (regExp, valExp) ->
    assignCondition localInfo regExp valExp state
    |> assignValue localInfo regExp valExp |> NormalExec
  | Store (_, addrExp, valExp) ->
    NormalExec (store addrExp valExp localInfo state)
  | InterJmp (_, dstExp, InterJmpInfo.IsCall) ->
    // InterJmp from Call instr.
    let funcs = AbsVal.getFunc (eval localInfo state dstExp)
    let targs = Set.choose getJmpTarget funcs
    call globInfo localInfo summaryMap state bb targs
  | InterJmp (_, dstExp, InterJmpInfo.Base) ->
    // InterJmp to another subroutine should be handled as a call + return.
    let funcs = AbsVal.getFunc (eval localInfo state dstExp)
    let targs = Set.choose getJmpTarget funcs
    let callGraph = GlobalInfo.getCallGraph globInfo
    let callTargs = Set.filter (fun t -> CallGraph.isSubrtn t callGraph) targs
    if Set.isEmpty callTargs then NormalExec state // Intra-procedural switch.
    else callAndRet globInfo localInfo summaryMap state bb callTargs
  | InterJmp (_, _, InterJmpInfo.IsRet) -> Ret state
  | CJmp (condExp, _, _) | InterCJmp (condExp, _, _, _) ->
    CondJump (condJump condExp state)
  | Jmp _ | InterJmp _ -> NormalExec state
  | SideEffect Halt -> ExecHalt
  | SideEffect SysCall -> failwith "Unfiltered syscall stub found"
  | SideEffect (Interrupt n) when n = 0x29 -> ExecHalt
  | SideEffect _ -> NormalExec state // Ignore other side effects

let rec execStmts globInfo localInfo summaryMap bb stmts state =
  match stmts with
  | [] -> NormalExec state
  | stmt :: tailStmts ->
    match execStmt globInfo localInfo summaryMap state bb stmt with
    | ExecHalt -> ExecHalt
    | Ret state -> Ret state
    | CondJump (tState, fState) ->
      if List.isEmpty tailStmts then CondJump (tState, fState)
      else // Malformed JMP (B2R2 div lifting issue)
           let record = LocalInfo.getRecord localInfo
           NormalExec (State.joinWithRecord record tState fState)
    | NormalExec state ->
      let state = State.incrPC state
      execStmts globInfo localInfo summaryMap bb tailStmts state

let checkStackPtr state ppoint =
  if not (State.isStackPtrValid state) then
    let stackPtrs = State.getStackPtrLoc state
    let ppStr = PPoint.toString ppoint
    let spStr = AbsLocSet.toString stackPtrs
    logWarning "Invalid stack pointer @ execBlock(%s): %s" ppStr spStr
    raise InvalidStackPtrException

// Backup stack pointer at the conditional branch, to use in restoration later.
let private backupStackPtr (bb: BasicBlock) result =
  if List.length (BasicBlock.getSuccs bb) < 2 then result
  else match result with
       | ExecHalt -> ExecHalt
       | Ret state -> Ret state
       | CondJump (tState, fState) ->
         CondJump (State.backupStackPtr tState, State.backupStackPtr fState)
       | NormalExec state -> NormalExec (State.backupStackPtr state)

let execBB globInfo localInfo summaryMap state bb =
  let binName = LocalInfo.getBinName localInfo
  let ppoint = BasicBlock.getPPoint binName bb
  if GlobalInfo.getVerbosity globInfo >= 2 then
    logInfo "Executing BB %s" (PPoint.toString ppoint)
  checkStackPtr state ppoint
  let stmts = BasicBlock.getStmts bb
  if GlobalInfo.isDebugMode globInfo then
    Debug.recordNextAddrs binName (BasicBlock.getRawAddrs bb)
  State.setPC state ppoint
  |> execStmts globInfo localInfo summaryMap bb stmts
  |> backupStackPtr bb
