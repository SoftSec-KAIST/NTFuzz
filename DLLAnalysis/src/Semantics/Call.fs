module DLLAnalysis.AbstractSemantics.Call

open System.Collections.Generic
open WinHeader
open DLLAnalysis
open DLLAnalysis.Utils
open DLLAnalysis.FrontEnd
open DLLAnalysis.AbstractDomain
open DLLAnalysis.DataFlow
open DLLAnalysis.Summary

type ConstraintItem = {
  ArgTyps : Set<ArgType>
  ArgVal : AbsVal
}

let private updateSyscallArg localInfo accState arg =
  let footPrint = LocalInfo.getFootPrint localInfo
  // Just mark to footprint, to help stack inference.
  let argLocs = AbsVal.getLoc arg
  let ppoint = State.getPC accState
  FootPrint.addLoad ppoint argLocs footPrint
  FootPrint.addStore ppoint argLocs footPrint
  accState

let private runSyscall localInfo subrtn state =
  let record = LocalInfo.getRecord localInfo
  let footPrint = Some (LocalInfo.getFootPrint localInfo)
  let retVal = AbsVal.unknownInt
  let state = State.setRetVal retVal state
  // Now update pointer arguments of the syscall.
  let argNum = StubInfo.getArgNum subrtn
  let args = State.getArguments footPrint record state argNum
  let state = List.fold (updateSyscallArg localInfo) state args
  let delta = Offset.WORD_SIZE * (Offset.ofInt (argNum + 1)) // x86.
  State.addStack delta state

// Target subroutine to call is unknown.
let runUnknown localInfo (bb: BasicBlock) state =
  let state = State.setRetVal AbsVal.unknownInt state
  // x86.
  let binName = LocalInfo.getBinName localInfo
  if BasicBlock.nextBBClearsStack binName bb
  then State.addStack Offset.WORD_SIZE state // Rely on caller-side clear.
  else State.restoreStackPtr state // Rely on stack pointer backup.

/// Lookup summary map and apply the summary.
let private runSummarized localInfo summaryMap bb subrtn state =
  match SummaryMap.tryFind subrtn summaryMap with
  | None ->
    // Since call graph phase can capture less semantics than main analysis,
    // we may encounter target functions not identified in call graph.
    logWarning "Unfound summary: %s" (Subroutine.toString subrtn)
    runUnknown localInfo bb state
  | Some summary ->
    let sideEffect = Summary.instantiateSideEffect localInfo state summary
    SideEffect.apply localInfo bb sideEffect state

let getJmpTarget = function
  | Function.Static addr -> Some addr
  | Function.SymCode _ -> None
  | Function.PC _ -> failwith "[Invalid] Calling program counter"

let private runSubrtnAux globInfo localInfo summaryMap bb subrtn state =
  let edge = (LocalInfo.getSubrtn localInfo, subrtn)
  let callGraph = GlobalInfo.getCallGraph globInfo
  if Encoding.isEncoded subrtn then Encoding.run localInfo subrtn state
  elif StubInfo.isStub subrtn then runSyscall localInfo subrtn state
  elif CallGraph.isCycleEdge edge callGraph then runUnknown localInfo bb state
  else runSummarized localInfo summaryMap bb subrtn state

let private findConstraints subrtn summaryMap =
  match APIInfo.tryFindAPI subrtn with
  | Some funcTyp -> Constraints.makeFromAPI funcTyp
  | None -> try (Map.find subrtn summaryMap).Constraints with
            | :? KeyNotFoundException -> Constraints.empty

let runSubrtn globInfo localInfo summaryMap state (bb: BasicBlock) subrtn =
  if GlobalInfo.getVerbosity globInfo >= 2 then
    logInfo "Calling subroutine %s" (Subroutine.toString subrtn)
  let constrs = findConstraints subrtn summaryMap
  let snapshot = Constraints.takeSnapshot localInfo state constrs
  try runSubrtnAux globInfo localInfo summaryMap bb subrtn state
      |> Constraints.apply localInfo snapshot constrs |> Some
  with ExecutionHaltException -> None
