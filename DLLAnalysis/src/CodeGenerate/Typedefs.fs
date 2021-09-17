module DLLAnalysis.CodeGen.Typedefs

// Assume that the number of argument does not exceed this number.
let private MAX_ARG_NUM = 20

let private defineType argNum =
  if argNum > 0 then
    Array.init argNum (fun i -> sprintf "  ULONG arg%d" (i + 1))
    |> String.concat ",\n"
    |> sprintf "typedef ULONG (*SYSCALL_%dARG)(\n%s\n);" argNum
  else "typedef ULONG (*SYSCALL_0ARG)(void);"

let make () =
  Array.init MAX_ARG_NUM defineType |> String.concat "\n"
