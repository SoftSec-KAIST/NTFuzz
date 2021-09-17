module DLLAnalysis.StubAnalysis

open B2R2.BinIR
open B2R2.BinIR.LowUIR
open DLLAnalysis
open DLLAnalysis.FrontEnd

let private isSyscall stmt =
  match stmt with
  | SideEffect sideEffect -> sideEffect = B2R2.BinIR.SysCall
  | _ -> false

let private hasSyscallStmt cfg =
  CFG.getBBs cfg
  |> Set.toList
  |> List.collect BasicBlock.getStmts
  |> List.exists isSyscall

let private findSysEntry cfgs =
  let folder acc f cfg = if hasSyscallStmt cfg then Set.add f acc else acc
  Map.fold folder Set.empty cfgs

let private belongToSystemDLL subrtn =
  let bin = Subroutine.binOf subrtn
  bin = "ntdll" || bin = "win32u"

let private analyzeStackLift regExp valExp =
  match regExp with
  | Var (_, _, r1, _) when r1 = Register.stackPtr ->
    match valExp with
    | BinOp (BinOpType.ADD, _, Var (_, _, r2, _), Num bv, _, _) ->
      if r2 = Register.stackPtr then BitVector.castToInt bv
      else 0
    | _ -> 0
  | _ -> 0

let private analyzeEax regExp valExp prevEaxOpt =
  match regExp with
  | Var (_, _, r1, _) when r1 = Register.sysNum ->
    match valExp with
    | Num bv -> Some (BitVector.castToInt bv)
    | _ -> prevEaxOpt
  | _ -> prevEaxOpt

let rec private tryAnalyzeStubAux sysEntries bin called stackLift eaxOpt stmts =
  match stmts with
  | [] -> None
  | InterJmp (_, _, InterJmpInfo.IsRet) :: _ ->
    if called && Option.isSome eaxOpt then
      let wordSize = NUInt.toInt NUInt.WORD_SIZE
      let argNum = stackLift / wordSize - 1 // -1 for saved return addr.
      let sysNum = Option.get eaxOpt
      Some (argNum, sysNum)
    else None
  | InterJmp (_, Num bitv, InterJmpInfo.IsCall) :: tailStmts ->
    let ui64 = BitVector.castToUInt64 bitv
    let addr = Addr.makeWithUI64 bin ui64
    if not (Set.contains addr sysEntries) then None
    else tryAnalyzeStubAux sysEntries bin true stackLift eaxOpt tailStmts
  | Put (regExp, valExp) :: tailStmts ->
    let stackLift = stackLift + analyzeStackLift regExp valExp
    let eaxOpt = analyzeEax regExp valExp eaxOpt
    tryAnalyzeStubAux sysEntries bin called stackLift eaxOpt tailStmts
  | _ :: tailStmts ->
    tryAnalyzeStubAux sysEntries bin called stackLift eaxOpt tailStmts

let private tryAnalyzeStub sysEntries subrtn cfg =
  let root = CFG.getRoot cfg
  let succs = BasicBlock.getSuccs root
  if List.length succs = 1 then
    let stmts = BasicBlock.getStmts root @ BasicBlock.getStmts succs.[0]
    let bin = Subroutine.binOf subrtn
    match tryAnalyzeStubAux sysEntries bin false 0 None stmts with
    | None -> ()
    | Some (argNum, sysNum) -> StubInfo.addStub subrtn argNum sysNum

let run cfgs =
  let sysEntries = findSysEntry cfgs
  Map.filter (fun subrtn _ -> belongToSystemDLL subrtn) cfgs
  |> Map.iter (tryAnalyzeStub sysEntries)
