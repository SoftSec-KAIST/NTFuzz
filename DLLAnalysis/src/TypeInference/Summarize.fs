module DLLAnalysis.TypeAnalysis.Summarize

open DLLAnalysis
open DLLAnalysis.Const
open DLLAnalysis.AbstractDomain
open DLLAnalysis.Summary
open DLLAnalysis.AbstractSemantics
open DLLAnalysis.Analysis.Modular

let private collectSyscall globInfo localInfo summaries callItem =
  let callState = callItem.CallerState
  let targ = callItem.Target
  let record = LocalInfo.getRecord localInfo
  let isEvalMode = GlobalInfo.isEvalMode globInfo
  let isEvalTarg s = Set.contains (Stub.toString s) EVALSET_SYSCALLS
  if StubInfo.isStub targ then
    if Encoding.isEncoded targ then Set.empty // Encoded.
    elif not isEvalMode && APIInfo.isAPI targ then Set.empty // Documented.
    elif not isEvalMode && Stub.isBlacklist targ then Set.empty // Blacklisted.
    elif isEvalMode && not (isEvalTarg targ) then Set.empty // Not target.
    else let argNum = StubInfo.getArgNum targ
         let argVals = State.getArguments None record callState argNum
         let syscall = Syscall.make localInfo callState targ argVals
         Set.singleton syscall
  elif SummaryMap.isSummarized targ summaries then
    // For API functions, we know exact types of arguments.
    if APIInfo.isAPI targ then Set.empty
    else // Instantiate the summarized syscall invocations.
         let summary = SummaryMap.find targ summaries
         Summary.instantiateSyscall localInfo callState summary
  else Set.empty

let rec private updateWithArgAux syscall idx pos accMap arg =
  let constr = AbsVal.getConstr arg.Value
  let typs = TypeConstr.getArgTypes constr
  let symTyps = ArgTypeSet.leaveSymbolic typs
  let symConstr = TypeConstr.setArgTypes symTyps constr
  let newVal = AbsVal.setConstr symConstr arg.Value
  // Add information from constraint domain.
  let concTyps = ArgTypeSet.leaveConcrete typs
                 |> ArgTypeSet.toList
                 |> List.map ArgTypeDom.getConcrType
  let accMap = TypeMap.addType syscall idx pos concTyps accMap
  let folder (acc, accStructs) baseLoc struc =
    match updateWithStruct syscall idx pos acc baseLoc struc with
    | (acc, None) -> (acc, accStructs)
    | (acc, Some newStruct) -> (acc, Map.add baseLoc newStruct accStructs)
  let accMap, newStructs = Map.fold folder (accMap, Map.empty) arg.Structs
  let isConcType = ArgTypeSet.isBot symTyps
  let isConcStruc = Map.isEmpty newStructs
  let allLocResolved = AbsLocSet.isBot arg.UnresolvedLocs
  let argOpt = if isConcType && isConcStruc && allLocResolved then None
               else Some { arg with Value = newVal; Structs = newStructs }
  accMap, argOpt

and private updateWithStruct syscall idx pos accMap baseLoc struc =
  let accMap, newSize =
    match struc.StructSize with
    | SymSize i -> (accMap, SymSize i)
    | ConcSize size -> // To prevent repeated update, set to bottom symbol.
      (TypeMap.addSize syscall idx pos baseLoc size accMap, SymSize AbsInt.bot)
  let folder (acc, accFields) (offset: Offset) fldArg =
    let pos = pos @ [(baseLoc, offset)]
    let acc, argOpt = updateWithArgAux syscall idx pos acc fldArg
    match argOpt with
    | None -> (acc, accFields)
    | Some arg -> (acc, Map.add offset arg accFields)
  let accMap, newFieldMap = Map.fold folder (accMap, Map.empty) struc.FieldMap
  let newStruc = { struc with StructSize = newSize; FieldMap = newFieldMap }
  if Map.isEmpty newFieldMap then (accMap, None) else (accMap, Some newStruc)

let private updateWithArg syscall (accMap, accArgs) idx arg =
  let accMap, argOpt = updateWithArgAux syscall idx [] accMap arg
  match argOpt with
  | None -> (accMap, SymbolicArg.bot :: accArgs)
  | Some arg -> (accMap, arg :: accArgs)

let private runAux (accResult, accSyscalls) syscall =
  let args = syscall.SymArgs
  let accResult, args = List.foldi (updateWithArg syscall) (accResult, []) args
  let reducedSyscall = { syscall with SymArgs = List.rev args }
  (accResult, Set.add reducedSyscall accSyscalls)

// Migrate the concretized system call argument information to 'result', and
// leave symbolic arguments only.
let run (result: TypeMap) gInfo lInfo summaries calls =
  let folder acc x = Set.union (collectSyscall gInfo lInfo summaries x) acc
  let syscalls = List.fold folder Set.empty calls
  Set.fold runAux (result, Set.empty) syscalls
