module DLLAnalysis.CodeGen.HookerFunc

open DLLAnalysis

let private makeHeader funcName argNum =
  let declArg i = sprintf "ULONG arg%d" (i + 1)
  let argDecl = if argNum = 0 then "void"
                else Array.init argNum declArg |> String.concat ", "
  sprintf "ULONG\n%s\n(%s) {\n" funcName argDecl

let private makeVars isNtSyscall argNum sysNum =
  let funcType = sprintf "SYSCALL_%dARG" argNum
  let origArr = if isNtSyscall then "origNtSyscalls" else "origW32Syscalls"
  let typArr = if isNtSyscall then "ntSyscallTypes" else "w32SyscallTypes"
  [
    sprintf "ULONG sysIdx = %d & 0xfff;" sysNum
    sprintf "%s origFunc = (%s)%s[sysIdx];" funcType funcType origArr
    sprintf "SyscallType* sysType = %s[sysIdx];" typArr
    "bool mutateTarget = false;"
    "#ifdef LOGGING_FLAG"
    "bool logTarget = false;"
    "#endif"
    "Log* log = NULL;"
    "ULONG ret;"
  ]
  |> List.map (fun (s: string) -> if s.[0] = '#' then s else  "  " + s)
  |> String.concat "\n"

let private makeTargetCheck () =
  [
    "if (IsTarget() && ExGetPreviousMode() == UserMode) {"
    "  execCount++;"
    "  if (mutateRatio && execCount > triggerCount) {"
    "    log = ObtainLog(sysType);"
    "    SetupLog(log, execCount, sysType);"
    "    mutateTarget = true;"
    "#ifdef LOGGING_FLAG"
    "    logTarget = true;"
    "  }"
    "  else if (sysType->kind == HANDLE_LOG) {"
    "    log = ObtainLog(sysType);"
    "    SetupLog(log, execCount, sysType);"
    "    logTarget = true;"
    "#endif"
    "  }"
    "}"
  ]
  |> List.map (fun (s: string) -> if s.[0] = '#' then s else  "  " + s)
  |> String.concat "\n"

let private makeMutations argNum =
  let storeMapper i = sprintf "  StoreArg(%d, arg%d);" i (i + 1)
  let stores = Array.init argNum storeMapper |> Array.toList
  let mutationMapper i =
    let arg = sprintf "arg%d" (i + 1)
    sprintf "    %s = MutateArg(log, sysType, %d, %s);" arg i arg
  let mutations = Array.init argNum mutationMapper |> Array.toList
  let header = [
    "if (mutateTarget) {"
  ]
  let interlude = ["  if (!sysType->noMutate) {"]
  let footer = [
    "  }"
    "}"
  ]
  header @ stores @ interlude @ mutations @ footer
  |> List.map (fun s -> "  " + s)
  |> String.concat "\n"

let private makeLogInput argNum =
  let mapper i = sprintf "  LogInputArg(log, sysType, %d, arg%d);" i (i + 1)
  let logInputs = Array.init argNum mapper |> Array.toList
  let header = [
    "#ifdef LOGGING_FLAG"
    "if (logTarget) {"
  ]
  let footer = [
    "}"
    "#endif"
  ]
  header @ logInputs @ footer
  |> List.map (fun (s: string) -> if s.[0] = '#' then s else  "  " + s)
  |> String.concat "\n"

let private makeCall argNum =
  Array.init argNum (fun i -> sprintf "arg%d" (i + 1))
  |> String.concat ", "
  |> sprintf "  ret = origFunc(%s);"

let private makeLogOutput argNum =
  let mapper i = sprintf "  LogOutputArg(log, sysType, %d, arg%d);" i (i + 1)
  let logOutputs = Array.init argNum mapper |> Array.toList
  let header = [
    "#ifdef LOGGING_FLAG"
    "if (logTarget) {"
    "  LogReturn(log, ret);"
  ]
  let footer = [
    "}"
    "#endif"
  ]
  header @ logOutputs @ footer
  |> List.map (fun (s: string) -> if s.[0] = '#' then s else  "  " + s)
  |> String.concat "\n"

let private defineHooker stub =
  let bin = Subroutine.binOf stub
  let stubName = Subroutine.fetchName (Stub.toString stub)
  let argNum = StubInfo.getArgNum stub
  let sysNum = StubInfo.getSysNum stub
  let funcName = Convention.HOOK_PREFIX + stubName
  let isNtSyscall =
    match bin with
    | "ntdll" -> true
    | "win32u" -> false
    | _ -> failwith "Unexpected binary of syscall stub"
  let header = makeHeader funcName argNum
  let vars = makeVars isNtSyscall argNum sysNum
  let check = makeTargetCheck ()
  let mut = makeMutations argNum
  let logIn = makeLogInput argNum
  let call = makeCall argNum
  let logOut = makeLogOutput argNum
  let body = String.concat "\n\n" [vars; check; mut; logIn; call; logOut]
  let footer = "\n\n  return ret;\n}"
  header + body + footer

let make targStubs =
  targStubs
  |> Set.filter (not << Stub.isBlacklist)
  |> Set.toList
  |> List.sortBy Stub.toString
  |> List.map defineHooker
  |> String.concat "\n\n"
