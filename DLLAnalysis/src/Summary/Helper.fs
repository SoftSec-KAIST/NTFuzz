namespace DLLAnalysis.Summary

open DLLAnalysis
open DLLAnalysis.Const
open DLLAnalysis.AbstractDomain

exception ConvertException

module Convert =

  let rec private offsetToSymbol curSym offsets locSymList =
    match offsets with
    | [] -> curSym
    | offset :: tailOffsets ->
      let curLocSym = Symbol.appendLocSuffix curSym
      let locOffset = Offset.ofUInt64 offset
      let loc = AbsLoc.makeSymLoc curLocSym locOffset
      let isMatching (l, _) = loc = l
      if List.exists isMatching locSymList then
        let (_, newSym) = List.find isMatching locSymList
        offsetToSymbol newSym tailOffsets locSymList
      else raise ConvertException

  let private getArgSymbol idx locSyms =
    if idx < NUMREGARG then // Register argument.
      Register.decideSymbol Register.args.[idx]
    else // Stack argument.
      let isMatching (l, _) = AbsLoc.isNthArgument idx l
      match List.tryFind isMatching locSyms with
      | None -> raise ConvertException
      | Some (_, s) -> s

  let argOffsetsToSymbol argIdx (offsets: uint64 list) (record: Record) =
    let locSymList = Dictionary.toList record.SLocMap
    let startSym = getArgSymbol argIdx locSymList
    offsetToSymbol startSym offsets locSymList

  let private tryGetArgIndex sym locSymMap =
    let isMatchingReg r = (sym = Register.decideSymbol r)
    let isMatchingEntry _ s = (sym = s)
    if List.exists isMatchingReg Register.args then
      Some (List.findIndex isMatchingReg Register.args)
    elif Map.exists isMatchingEntry locSymMap then
      let loc = Map.findKey isMatchingEntry locSymMap
      AbsLoc.tryGetRootArgIdx loc
    else None

  let rec private symbolToArgOffsetsAux sym locSymMap accOffsets =
    match tryGetArgIndex sym locSymMap with
    | Some argIdx -> // Successfully resolved as 'argIdx'-th argument.
      // Negative offset is invalid.
      if List.exists (fun o -> o < Offset.ZERO) accOffsets then
        raise ConvertException
      (argIdx, List.map uint64 accOffsets)
    | None -> // The symbol is not resolved as an argument symbol yet.
      // Find the location where this 'sym' was derived from.
      let isMatchingEntry _ s = (sym = s)
      match Map.tryFindKey isMatchingEntry locSymMap with
      | None -> raise ConvertException // Non-arg register symbol (e.g. ECX)
      | Some srcLoc when not (AbsLoc.isSymbolic srcLoc) ->
        raise ConvertException // Reached a global var loc.
      | Some srcLoc ->
        let srcOffset = AbsLoc.getOffset srcLoc
        // We're traversing backward, so offset should be appended at front.
        let accOffsets = srcOffset :: accOffsets
        let srcSym = Symbol.truncateSuffix (AbsLoc.getSymbol srcLoc)
        symbolToArgOffsetsAux srcSym locSymMap accOffsets

  let symbolToArgOffsets typSym (record: Record) =
    let locSymMap = Dictionary.toMap record.SLocMap
    let sym = Symbol.truncateSuffix typSym
    symbolToArgOffsetsAux sym locSymMap []

  let symbolToArgIdx typSym (record: Record) =
    let locSymMap = Dictionary.toMap record.SLocMap
    let sym = Symbol.truncateSuffix typSym
    match tryGetArgIndex sym locSymMap with
    | None -> raise ConvertException
    | Some idx -> idx
