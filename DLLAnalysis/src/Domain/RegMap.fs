module DLLAnalysis.AbsDom.RegMap

open DLLAnalysis
open DLLAnalysis.AbsDom.Functor
open DLLAnalysis.AbsDom.Register
open DLLAnalysis.AbsDom.AbsVal

type RegMap = Map<Register,AbsVal>

type RegMapModule() =
  inherit FunDomain<Register,AbsVal>(Register,AbsVal)

  member private __.initializeAux reg regMap =
    let prefixSymbol = Register.decideSymbol reg
    let initVal = AbsVal.ofSymbol prefixSymbol
    __.add reg initVal regMap

  /// Initialize input registers with symbolic values.
  member __.initialize () =
    let regsToInit = Set.ofList Register.inputs
    Set.fold (fun accMap r -> __.initializeAux r accMap) __.bot regsToInit

let RegMap = RegMapModule () // Now we can use 'RegMap' like a module
