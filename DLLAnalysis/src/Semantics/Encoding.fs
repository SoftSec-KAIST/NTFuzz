module DLLAnalysis.AbstractSemantics.Encoding

open DLLAnalysis
open DLLAnalysis.Const
open DLLAnalysis.AbstractDomain

let abort record state =
  raise ExecutionHaltException

let nop record state =
  state

let private runSehProlog localInfo state =
  // Simplified semantics of SEH prologue is:
  // (1) BP := SP + 2 * WORDSIZE (2) SP -= arg + 2 * WORDSIZE (3) Push 5 words.
  let stackPtr = State.getStackPtrVal state
  let ebpLift = NUInt.TWO * NUInt.WORD_SIZE
  let newEBP = AbsVal.addImmediate stackPtr ebpLift
  let state = State.assign Register.basePtr newEBP Variable.NoneVar state
  let record = LocalInfo.getRecord localInfo
  let sizeVal = State.getArgument None record 1 state
  let sizeAbsInt = AbsVal.getInt sizeVal
  if not (AbsInt.isConst sizeAbsInt) then
    failwithf "Unexpected SEH prolog argument: %s" (AbsVal.toString sizeVal)
  let size = AbsInt.getConst sizeAbsInt + (NUInt.ofInt 7) * NUInt.WORD_SIZE
  let delta = - (Offset.ofNUInt size)
  State.addStack delta state

let private runSehEpilog localInfo state =
  // Simplified semantics of SEH epilogue is "SP := BP"
  let ebp = State.read Register.basePtr state
  State.setStackPtr ebp state

let private runRtlAllocateHeap localInfo state =
  let record = LocalInfo.getRecord localInfo
  let footPrint = Some (LocalInfo.getFootPrint localInfo)
  let pcAddr = State.getPCAddr state
  let sizeArgVal = State.getArgument footPrint record 2 state
  let sizeArg = AbsVal.getInt sizeArgVal
  let heapLoc = AbsLoc.makeHeapLoc pcAddr Offset.ZERO sizeArg
  let retVal = AbsVal.ofLoc heapLoc
  // First, update return register with allocated heap pointer.
  let state = State.setRetVal retVal state
  // Next, add allocation tag to registers that are same to the size argument.
  let eqRegs = State.getRegsEqualToArg 2 state
  let regFolder accState reg =
    // No update on def/use set, since it's just tag addition.
    let v = State.read reg accState
    let newVal = AbsVal.addTag v pcAddr
    State.assign reg newVal Variable.NopVar accState
  let state = Set.fold regFolder state eqRegs
  let eqLocs = State.getLocsEqualToArg 2 state
  let locFolder accState l =
    // Avoid updating footPrint.
    let accMem = State.getAbsMem accState
    let v = AbsMem.find l accMem
    let newVal = AbsVal.addTag v pcAddr
    let accMem = AbsMem.add l newVal accMem
    State.setAbsMem accState accMem
  Set.fold locFolder state eqLocs

let private runRtlpAllocateHeap localInfo state =
  runRtlAllocateHeap localInfo state // Can use same semantics.

let private runRtlReAllocateHeap localInfo state =
  let record = LocalInfo.getRecord localInfo
  let footPrint = Some (LocalInfo.getFootPrint localInfo)
  let ptrArgVal = State.getArgument footPrint record 2 state
  State.setRetVal ptrArgVal state

let private runNtAllocateVirtualMemory localInfo state =
  let pcAddr = State.getPCAddr state
  let record = LocalInfo.getRecord localInfo
  let footPrint = Some (LocalInfo.getFootPrint localInfo)
  let sizePtrArgVal = State.getArgument footPrint record 3 state
  let sizePtrLocs = AbsVal.getLoc sizePtrArgVal
  let sizeVal = State.load footPrint record sizePtrLocs state
  let size = AbsVal.getInt sizeVal
  let basePtrArgVal = State.getArgument footPrint record 1 state
  let basePtrLocs = AbsVal.getLoc basePtrArgVal
  let heapLoc = AbsLoc.makeHeapLoc pcAddr Offset.ZERO size
  let heapVal = AbsVal.ofLoc heapLoc
  let newSizeVal = AbsVal.addTag sizeVal pcAddr
  // First, update pointer argument with allocated heap pointer.
  State.store footPrint record Variable.NoneVar basePtrLocs heapVal state
  // Now, update return register.
  |> State.setRetVal AbsVal.unknownInt
  // Finally, update size variable with allocsite tag.
  |> State.store footPrint record Variable.NoneVar sizePtrLocs newSizeVal

// Refer to kernelbase!VirtualAlloc2.
let private runNtAllocateVirtualMemoryEx localInfo state =
  let record = LocalInfo.getRecord localInfo
  let footPrint = Some (LocalInfo.getFootPrint localInfo)
  let pcAddr = State.getPCAddr state
  let sizePtrArgVal = State.getArgument footPrint record 2 state
  let sizePtrLocs = AbsVal.getLoc sizePtrArgVal
  let sizeVal = State.load footPrint record sizePtrLocs state
  let size = AbsVal.getInt sizeVal
  let basePtrArgVal = State.getArgument footPrint record 1 state
  let basePtrLocs = AbsVal.getLoc basePtrArgVal
  let heapLoc = AbsLoc.makeHeapLoc pcAddr Offset.ZERO size
  let heapVal = AbsVal.ofLoc heapLoc
  let newSizeVal = AbsVal.addTag sizeVal pcAddr
  // First, update pointer argument with allocated heap pointer.
  State.store footPrint record Variable.NoneVar basePtrLocs heapVal state
  // Now, update return register.
  |> State.setRetVal AbsVal.unknownInt
  // Finally, update size variable with allocsite tag.
  |> State.store footPrint record Variable.NoneVar sizePtrLocs newSizeVal

let private runNtCreateSection localInfo state =
  let record = LocalInfo.getRecord localInfo
  let footPrint = Some (LocalInfo.getFootPrint localInfo)
  let handleVal = AbsVal.unknownInt
  let pHandleArg = State.getArgument footPrint record 0 state
  let pHandleLocs = AbsVal.getLoc pHandleArg
  State.store footPrint record Variable.NoneVar pHandleLocs handleVal state

let private runNtCreateSectionEx localInfo state =
  runNtCreateSection localInfo state // Can use same semantics.

let private runNtMapViewOfSection localInfo state =
  let record = LocalInfo.getRecord localInfo
  let footPrint = Some (LocalInfo.getFootPrint localInfo)
  let pcAddr = State.getPCAddr state
  let sizePtrArgVal = State.getArgument footPrint record 6 state
  let sizePtrLocs = AbsVal.getLoc sizePtrArgVal
  let sizeVal = State.load footPrint record sizePtrLocs state
  let size = AbsVal.getInt sizeVal
  let basePtrArgVal = State.getArgument footPrint record 2 state
  let basePtrLocs = AbsVal.getLoc basePtrArgVal
  let heapLoc = AbsLoc.makeHeapLoc pcAddr Offset.ZERO size
  let heapVal = AbsVal.ofLoc heapLoc
  let newSizeVal = AbsVal.addTag sizeVal pcAddr
  // First, update pointer argument with allocated heap pointer.
  State.store footPrint record Variable.NoneVar basePtrLocs heapVal state
  // Now, update return register.
  |> State.setRetVal AbsVal.unknownInt
  // Finally, update size variable with allocsite tag.
  |> State.store footPrint record Variable.NoneVar sizePtrLocs newSizeVal

let private runNtMapViewOfSectionEx record state =
  runNtMapViewOfSection record state

let private runMemcpy localInfo state =
  let record = LocalInfo.getRecord localInfo
  let footPrint = LocalInfo.getFootPrint localInfo
  let footPrintOpt = Some footPrint
  let dstVal = State.getArgument footPrintOpt record 0 state
  let dstLocs = AbsVal.getLoc dstVal
  let srcVal = State.getArgument footPrintOpt record 1 state
  let srcLocs = AbsVal.getLoc srcVal
  let sizeVal = State.getArgument footPrintOpt record 2 state
  let sizeInt = AbsVal.getInt sizeVal
  MemFunc.memcpy localInfo dstLocs srcLocs sizeInt state

let private runMemMove = runMemcpy

let private runMemMoveS localInfo state =
  let record = LocalInfo.getRecord localInfo
  let footPrint = LocalInfo.getFootPrint localInfo
  let footPrintOpt = Some footPrint
  let dstVal = State.getArgument footPrintOpt record 0 state
  let dstLocs = AbsVal.getLoc dstVal
  let srcVal = State.getArgument footPrintOpt record 2 state
  let srcLocs = AbsVal.getLoc srcVal
  let sizeVal = State.getArgument footPrintOpt record 3 state
  let sizeInt = AbsVal.getInt sizeVal
  MemFunc.memcpy localInfo dstLocs srcLocs sizeInt state

let private runMemset localInfo state =
  let record = LocalInfo.getRecord localInfo
  let footPrint = LocalInfo.getFootPrint localInfo
  let footPrintOpt = Some footPrint
  let dstVal = State.getArgument footPrintOpt record 0 state
  let dstBases = AbsVal.getLoc dstVal
  let setVal = State.getArgument footPrintOpt record 1 state
  let setInt = AbsVal.getInt setVal
  let sizeVal = State.getArgument footPrintOpt record 2 state
  let sizeInt = AbsVal.getInt sizeVal
  MemFunc.memset localInfo dstBases setInt sizeInt state

let private encodeMap =
  Map.empty
  // Exception and error handling functions.
  |> Map.add "__security_check_cookie" nop
  |> Map.add "_invalid_parameter" nop
  |> Map.add "__SEH_prolog4" runSehProlog
  |> Map.add "__SEH_prolog4_GS" runSehProlog
  |> Map.add "__SEH_epilog4" runSehEpilog
  |> Map.add "__SEH_epilog4_GS" runSehEpilog
  // Abort functions and syscalls.
  |> Map.add "RtlRaiseException" abort
  |> Map.add "RtlRaiseStatus" abort
  |> Map.add "NtTerminateProcess" abort
  |> Map.add "NtTerminateThread" abort
  |> Map.add "NtContinue" abort
  |> Map.add "NtContinueEx" abort
  // Memory management functions and syscalls.
  |> Map.add "RtlCreateHeap" nop
  |> Map.add "RtlDestroyHeap" nop
  |> Map.add "RtlAllocateHeap" runRtlAllocateHeap
  |> Map.add "RtlpAllocateHeap" runRtlpAllocateHeap
  |> Map.add "RtlReAllocateHeap" runRtlReAllocateHeap
  |> Map.add "NtAllocateVirtualMemory" runNtAllocateVirtualMemory
  |> Map.add "NtAllocateVirtualMemoryEx" runNtAllocateVirtualMemoryEx
  |> Map.add "NtFreeVirtualMemory" nop
  |> Map.add "NtCreateSection" runNtCreateSection
  |> Map.add "NtCreateSectionEx" runNtCreateSectionEx
  |> Map.add "NtMapViewOfSection" runNtMapViewOfSection
  |> Map.add "NtMapViewOfSectionEx" runNtMapViewOfSectionEx
  |> Map.add "NtUnmapViewOfSection" nop
  |> Map.add "NtUnmapViewOfSectionEx" nop
  // Data propagation functions.
  |> Map.add "memcpy" runMemcpy
  |> Map.add "_memcpy" runMemcpy
  |> Map.add "memmove" runMemMove
  |> Map.add "_memmove" runMemMove
  |> Map.add "memmove_s" runMemMoveS
  |> Map.add "_memmove_s" runMemMoveS
  |> Map.add "memset" runMemset
  |> Map.add "_memset" runMemset
  |> Map.add "RtlRbRemoveNode" nop
  |> Map.add "RtlRbInsertNodeEx" nop
  |> Map.add "RtlAvlRemoveNode" nop
  |> Map.add "RtlAvlInsertNodeEx" nop

/// Truncate '@' suffix and prefix before looking up the encoding map.
let private noarmalizeName (name: string) =
  let name = if name.StartsWith("@") then name.[1..] else name
  let idx = name.LastIndexOf('@')
  if idx > 0 then name.[.. (idx - 1)] else name

/// Checks if a given subroutine has encoded semantics.
let isEncoded (subrtn: Subroutine) =
  let names = Subroutine.toNameAll subrtn
  let normalized = List.map noarmalizeName names
  let allNames = names @ normalized
  List.exists (fun n -> Map.containsKey n encodeMap) allNames

/// Returns a set of subroutines that have encoded semantics. Note that the PDB
/// symbol table should be initialzed before calling this function.
let getEncoded cfgs =
  Map.keys cfgs
  |> List.filter isEncoded
  |> Set.ofList

let private stackLiftRule subrtn =
  let names = Subroutine.toNameAll subrtn
  let normalized = List.map noarmalizeName names
  let allNames = names @ normalized
  if List.contains "__SEH_prolog4" allNames then Some NInt.ZERO
  elif List.contains "__SEH_prolog4_GS" allNames then Some NInt.ZERO
  elif List.contains "__SEH_epilog4" allNames then Some NInt.ZERO
  elif List.contains "__SEH_epilog4_GS" allNames then Some NInt.ZERO
  elif List.contains "memcpy" allNames then Some NInt.ZERO
  elif List.contains "_memcpy" allNames then Some NInt.ZERO
  elif List.contains "memset" allNames then Some NInt.ZERO
  elif List.contains "_memset" allNames then Some NInt.ZERO
  elif List.contains "memmove" allNames then Some NInt.ZERO
  elif List.contains "_memmove" allNames then Some NInt.ZERO
  elif List.contains "memmove_s" allNames then Some NInt.ZERO
  elif List.contains "_memmove_s" allNames then Some NInt.ZERO
  else Subroutine.tryFindStackLiftWithName subrtn

// x86
let private liftStack subrtn state =
  match stackLiftRule subrtn with
  | Some delta -> State.addStack (delta + NInt.WORD_SIZE) state
  | None -> failwithf "Unhandled case: %s" (Subroutine.toString subrtn)

/// Fetch and apply the encoded semantic function.
let run localInfo subrtn state =
  let names = Subroutine.toNameAll subrtn
  let normalized = List.map noarmalizeName names
  let allNames = names @ normalized
  let matchingName = List.find (fun n -> Map.containsKey n encodeMap) allNames
  let encodedFn = Map.find matchingName encodeMap
  encodedFn localInfo state
  |> liftStack subrtn
