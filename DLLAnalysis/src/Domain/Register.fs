module DLLAnalysis.AbsDom.Register

open DLLAnalysis
open DLLAnalysis.AbsDom.Signature

type RegisterModule () =

  inherit Elem<Register>()

  override __.toString reg = Register.toString reg

  member __.stackPtr = Register.stackPtr

  member __.ghostStackPtr = Register.ghostStackPtr

  member __.stackImpreciseFlag = Register.stackImpreciseFlag

  member __.basePtr = Register.basePtr

  member __.ret = Register.ret

  member __.strVal = Register.strVal

  member __.strSrc = Register.strSrc

  member __.strDst = Register.strDst

  member __.strCnt = Register.strCnt

  member __.pgmCounter = Register.pgmCounter

  member __.flags = Register.flags

  member __.args = Register.args

  member __.inputs = Register.inputs

  member __.isFlag reg = Register.isFlag reg

  member __.ofTempVar tmpRegNum = Register.ofTempVar tmpRegNum

  member __.decideSymbol reg : Symbol =
    reg // Just use register name itself as symbol.

let Register = RegisterModule()
