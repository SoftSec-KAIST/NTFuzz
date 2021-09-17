namespace DLLAnalysis.Summary

open DLLAnalysis
open DLLAnalysis.AbstractDomain

type Summary = {
  LocSymMap : Map<AbsLoc,Symbol>
  Constraints : Constraints
  SideEffect : SideEffect
  Syscalls : Set<Syscall>
}

module Summary =

  let unknown =
    { LocSymMap = Map.empty
      Constraints = Constraints.empty
      SideEffect = SideEffect.unknown
      Syscalls = Set.empty }

  let toString summary =
    let symStr = LocSymMap.toString summary.LocSymMap
    let sideEffectStr = SideEffect.toString summary.SideEffect
    let syscalls = Set.toList summary.Syscalls
                   |> List.sortBy Syscall.getName
    let delim = "\n-----------------\n\n"
    let syscallStr = String.concat delim (List.map Syscall.toString syscalls)
    let constrStr = Constraints.toString summary.Constraints
    sprintf "<Symbols>\n\n%s\n\n" symStr +
    sprintf "<Constraints>\n\n%s\n\n" constrStr +
    sprintf "<Side Effects>\n\n%s\n\n" sideEffectStr +
    sprintf "<Syscalls>\n\n%s\n" syscallStr

  let make sideEffect syscalls record =
    // Omit symbols that are not used in syscall summary.
    let sideEffectSyms = SideEffect.collectSymbols sideEffect
    let folder accSet syscall = Set.union syscall.Symbols accSet
    let syscallSyms = Set.fold folder Set.empty syscalls
    let usedSyms = Set.union sideEffectSyms syscallSyms
                   |> Set.map Symbol.truncateSuffix
    { LocSymMap = LocSymMap.make record usedSyms
      Constraints = Dictionary.toMap record.ConstrMap
      SideEffect = sideEffect
      Syscalls = syscalls }

  let private instantiateSyscallAux localInfo state subst syscall =
    if Syscall.isConcrete syscall then None
    else Some (Syscall.instantiate localInfo state subst syscall)

  /// Instantiate summarized syscall invocations, under the caller's context
  /// 'state' and 'record'. If syscall is already concrete, do not instantiate,
  /// to prevent explosion of syscalls.
  let instantiateSyscall localInfo state summary =
    let subst = Substitution.make localInfo state summary.LocSymMap
    Set.choose (instantiateSyscallAux localInfo state subst) summary.Syscalls

  /// Instantiate summarized side-effects, under the caller's context 'state'
  /// and 'record'.
  let instantiateSideEffect localInfo state summary =
    Substitution.make localInfo state summary.LocSymMap
    |> SideEffect.instantiate localInfo summary.SideEffect

type SummaryMap = Map<Subroutine, Summary>

module SummaryMap =

  let empty : SummaryMap = Map.empty

  let add subrtn summary (summaryMap : SummaryMap) =
    Map.add subrtn summary summaryMap

  let find subrtn (summaryMap: SummaryMap) =
    Map.find subrtn summaryMap

  let isSummarized subrtn (summaryMap: SummaryMap) =
    Map.containsKey subrtn summaryMap

  let tryFind subrtn (summaryMap: SummaryMap) =
    Map.tryFind subrtn summaryMap
