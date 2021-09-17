namespace WinHeader.C

open WinHeader.Utils

type CArg = {
  MainSizeAnnot  : SizeAnnot
  AuxSizeAnnot  : SizeAnnot
  IOAnnot : IOAnnot
  TypeName : string // Type name.
  AstrCount : int // Number of asterisk appended to the type.
  ArgName : string option // Argument name is optional.
}

module CArg =
  let toString arg =
    let typStr = arg.TypeName
    let ptrStr = String.replicate arg.AstrCount "*" + " "
    let nameStr = match arg.ArgName with | Some s -> s | None -> ""
    typStr + ptrStr + nameStr

type CFunction = {
  Args : CArg list
}

module CFunction =
  let toString func =
    let argStrs = List.map CArg.toString func.Args
    "(" + String.concat ", " argStrs + ")"
