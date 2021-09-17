namespace DLLAnalysis

open System.Collections.Generic
open B2R2
open B2R2.BinFile
open B2R2.FrontEnd // For struct lookup.
open DLLAnalysis
open DLLAnalysis.Utils

/// An imperative module to store subroutine redirect mapping.
module RedirectMap =

  let private srcToDst: Dictionary<string,string> = Dictionary ()
  let private dstToSrc: Dictionary<string,string> = Dictionary ()

  let private addMapping src dst =
    srcToDst.[src] <- dst
    dstToSrc.[dst] <- src

  // Update map with forward export entries of PE file. This map will be used
  // to construct final 'redirect mapping' afterwards.
  let private addForwardExport binName accMap funcName (fwdStr: string) =
    // Note that there are some non-function forwarding entries.
    if fwdStr.Contains(".") && fwdStr.IndexOf('.') > 0 then
      let srcName = sprintf "%s!%s" binName funcName
      let idx = fwdStr.IndexOf('.')
      let fwdBin = fwdStr.[.. (idx - 1)].ToLower()
      let fwdFunc = fwdStr.[(idx + 1) ..]
      let fwdName = sprintf "%s!%s" fwdBin fwdFunc
      Map.add srcName fwdName accMap
    else accMap

  let private buildForwardMapAux accMap (binName, binHandle) =
    let peInfo = binHandle.FileInfo :?> B2R2.BinFile.PEFileInfo // XXX. Windows.
    let forwardMap = peInfo.GetForwardMap()
    Map.fold (addForwardExport binName) accMap forwardMap

  let private buildForwardMap binList =
    List.fold buildForwardMapAux Map.empty binList

  let rec private updateRedirect forwardMap cnt name =
    if cnt > 5 then failwith "Cyclic API resolvement"
    // First, check if this subroutine is forwarded.
    match Map.tryFind name forwardMap with
    | Some dst ->
      addMapping name dst
      updateRedirect forwardMap (cnt + 1) dst
    | None ->
      let bin = name.Split("!").[0]
      let func = name.Split("!").[1]
      // Also, check if module should be resolved with API schema.
      match APISchema.tryResolveDLL bin with
      | None -> ()
      | Some resolvedBin ->
        let dst = (sprintf "%s!%s" resolvedBin func)
        addMapping name dst
        updateRedirect forwardMap (cnt + 1) dst

  let private importSymbolToName (s: BinFile.Symbol) =
    let binName = (truncateBinExt s.LibraryName).ToLower()
    sprintf "%s!%s" binName s.Name

  let private getTraverseRootsAux acc (binName, binHandle) =
    let peInfo = binHandle.FileInfo :?> B2R2.BinFile.PEFileInfo // XXX. Windows.
    let forwardEntries = peInfo.GetForwardMap()
                         |> Map.keys
                         |> List.map (fun s -> sprintf "%s!%s" binName s)
    let isImportSym (s: BinFile.Symbol) = s.Kind = SymbolKind.ExternFunctionType
    let importEntries = binHandle.FileInfo.GetDynamicSymbols() |> List.ofSeq
                        |> List.filter isImportSym
                        |> List.map importSymbolToName
    forwardEntries @ importEntries @ acc

  let private getTraverseRoots binaries =
    List.fold getTraverseRootsAux [] binaries

  let init binaries =
    let forwardMap = buildForwardMap binaries
    let roots = getTraverseRoots binaries
    List.iter (updateRedirect forwardMap 0) roots

  /// Resolve the given name to obtain the final redirected name.
  let rec resolve name =
    if srcToDst.ContainsKey(name) then
      let redirected = srcToDst.[name]
      if name = redirected then failwith "Self-recursive entry" // DEBUG
      resolve redirected
    else name

  let rec private findSrcAliases acc name =
    if dstToSrc.ContainsKey(name) then
      let alias = dstToSrc.[name]
      let acc = alias :: acc
      findSrcAliases acc alias
    else acc

  let rec private findDstAliases acc name =
    if srcToDst.ContainsKey(name) then
      let alias = srcToDst.[name]
      let acc = alias :: acc
      findDstAliases acc alias
    else acc

  /// Find aliases of a given function.
  let rec findAliases name =
    findSrcAliases [] name @ findDstAliases [] name
