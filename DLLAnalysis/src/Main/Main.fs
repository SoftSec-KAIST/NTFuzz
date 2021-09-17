open DLLAnalysis
open DLLAnalysis.Utils

let printUsage () =
  println "Usage: 'dotnet DLLAnalysis.dll <type|code> (options...)'"
  println "  type: Analyze type of syscall arguments."
  println "        Use 'dotnet DLLAnalysis.dll type --help' for details."
  println "  code: Generate code for system call hooker project."
  println "        Use 'dotnet DLLAnalysis.dll code --help' for details."

let runMode (mode: string) args =
  match mode.ToLower() with
  | "type" -> TypeInference.run args
  | "code" -> CodeGenerate.run args
  | _ -> printUsage ()

[<EntryPoint>]
let main argv =
  if Array.length argv <= 1
  then printUsage (); 1
  else runMode argv.[0] argv.[1..]; 0
