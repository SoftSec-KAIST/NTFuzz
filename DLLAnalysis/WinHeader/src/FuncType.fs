namespace WinHeader

open WinHeader.Utils
open WinHeader.C
open WinHeader.TypeHelper

type FuncType = ArgType list

module FuncType =

  let private isVoidArgs ctags cArgs =
    if List.length cArgs = 1 then
      let firstCArg = List.head cArgs
      FindType.run ctags firstCArg.TypeName = Void
    else false

  let ofCFunction ctags (cFunc: CFunction) : FuncType =
    if isVoidArgs ctags cFunc.Args then []
    else let env = Env.addArgEntries ctags cFunc.Args Env.empty
         List.map (ArgType.ofCArg ctags env) cFunc.Args

  let rec private findArgTypeAux offsets argTyp =
    match offsets with
    | [] -> argTyp
    | offset :: tailOffsets ->
      findArgTypeAux tailOffsets (ArgType.deref offset argTyp)

  let findArgType argIdx offsets (funcTyp: FuncType) =
    try Some (findArgTypeAux offsets funcTyp.[argIdx]) with
    | ArgType.ResolveException ->
      // For example, this can happen if we try to dereference a handle type
      // argument. Since header information cannot provide better information
      // about underlying type in such cases, give up.
      None
    | :? System.ArgumentException ->
      // This can happen since out analysis can overestimate function arguments,
      // if the function assigns to the lowest byte of an argument resgister.
      None

  let toJson sysNum funcTyp =
    let debugStr = "\"debug\":0"
    let argNum = List.length funcTyp
    let argNumStr = sprintf "\"argnum\":%d" argNum
    let sysNumStr = sprintf "\"sysnum\":%d" sysNum
    let indent = "    "
    let mapper i argtyp =
      sprintf "\"arg%d\":%s" (i + 1) (ArgType.toJson false indent argtyp)
    let typStrs = List.mapi mapper funcTyp
    let entries = debugStr :: argNumStr :: sysNumStr :: typStrs
    let header = "{\n    "
    let body = String.concat ",\n    " entries
    let footer = "\n  }"
    header + body + footer
