module DLLAnalysis.CodeGen.HookerCode

open DLLAnalysis

let private incl =
  "#include \"..\inc\GeneralHooker.h\""

let private macro =
  "#define IsTarget() ((ULONG)PsGetCurrentProcessId() == targetPID)"

let generate hookerFile =
  let stubs = StubInfo.getStubs()
  let typedefs = Typedefs.make()
  let hookers = HookerFunc.make stubs
  let register = RegisterFunc.make stubs
  let code = [incl; macro; typedefs; hookers; register] |> String.concat "\n\n"
  System.IO.File.WriteAllText(hookerFile, code)
