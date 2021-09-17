namespace DLLAnalysis.Summary

open DLLAnalysis
open DLLAnalysis.Utils
open DLLAnalysis.AbstractDomain

type Syscall = {
  Stub : Stub
  Callsite : Addr
  SymArgs : SymbolicArg list
  SizeArgMap : SizeArgMap
  // Symbols used in syscall.
  Symbols : Set<Symbol>
}

module Syscall =

  /// Collect symbols used in syscall.
  let private collectSymbols args sizeArgMap =
    let folder accSet arg = Set.union (SymbolicArg.collectSymbols arg) accSet
    let argSyms = List.fold folder Set.empty args
    let folder accSet sym _ = Set.add sym accSet
    let sizeArgMapSyms = Map.fold folder Set.empty sizeArgMap.SymArgMap
    Set.union argSyms sizeArgMapSyms

  /// Generate an entity that represents a system call invocation. Type of each
  /// argument is inferred, using the abstract state at callsite.
  let make localInfo callState stub args =
    let pcAddr = State.getPCAddr callState
    let syscallName = Subroutine.toString stub
    let callsiteStr = Addr.toString pcAddr
    let mem = State.getAbsMem callState
    let sizeArgMap = SizeArgMap.make mem args
    let mapper = SymbolicArg.make localInfo callState sizeArgMap
    let symArgs = List.mapi mapper args
    let symbols = collectSymbols symArgs sizeArgMap
    { Stub = stub
      SymArgs = symArgs
      SizeArgMap = sizeArgMap
      Callsite = pcAddr
      Symbols = symbols }

  let private argValToStr idx symArg =
    sprintf "arg%d = %s" (idx + 1) (AbsVal.toString symArg.Value)

  let private symArgToStr idx symArg =
    sprintf "arg%d : %s" (idx + 1) (SymbolicArg.toString "" symArg)

  let getName syscall =
    Subroutine.toString syscall.Stub

  let isConcrete syscall =
    Set.isEmpty syscall.Symbols

  let toString syscall =
    let name = getName syscall
    let argValStr = List.mapi argValToStr syscall.SymArgs |> String.concat ", "
    let symArgStr = List.mapi symArgToStr syscall.SymArgs |> String.concat "\n"
    let callsiteStr = Addr.toString syscall.Callsite
    let sizeArgStr = SizeArgMap.toString syscall.SizeArgMap
    sprintf "%s(%s)\n%s\n(Callsite)\n%s\n(SizeArgMap)\n%s"
      name argValStr symArgStr callsiteStr sizeArgStr

  let instantiate localInfo state subst syscall =
    let sizeArgMap = syscall.SizeArgMap
    // Note that size argument map should be substituted, too.
    let newSizeArgMap = SizeArgMap.substitute subst sizeArgMap
    let mapper = SymbolicArg.instantiate localInfo state subst newSizeArgMap
    let newSymArgs = List.map mapper syscall.SymArgs
    let newSyms = collectSymbols newSymArgs newSizeArgMap
    { syscall with SymArgs = newSymArgs
                   SizeArgMap = newSizeArgMap
                   Symbols = newSyms }
