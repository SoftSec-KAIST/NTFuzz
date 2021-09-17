module DLLAnalysis.CodeGen.RegisterFunc

open DLLAnalysis

let private registerFunc stub =
  let bin = Subroutine.binOf stub
  let isNtSyscall =
    match bin with
    | "ntdll" -> true
    | "win32u" -> false
    | _ -> failwith "Unexpected binary of syscall stub"
  let hookingAPI = if isNtSyscall then "RegisterNtHook" else "RegisterW32Hook"
  let sysNum = StubInfo.getSysNum stub
  let stubName = Subroutine.fetchName (Stub.toString stub)
  let funcName = Convention.HOOK_PREFIX + stubName
  sprintf "  %s(%d, (ULONG_PTR)%s);\n" hookingAPI sysNum funcName

let make targStubs =
  let stubs = Set.filter (not << Stub.isBlacklist) targStubs
              |> Set.toList
              |> List.sortBy Stub.toString
  let header = "void RegisterGeneralHooks(void) {\n"
  let body = List.map registerFunc stubs |> String.concat ""
  let footer = "}"
  header + body + footer
