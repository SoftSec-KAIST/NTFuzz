namespace DLLAnalysis.FrontEnd

open B2R2
open B2R2.BinFile
open B2R2.FrontEnd // For struct lookup.
open DLLAnalysis
open DLLAnalysis.Utils

// Map from raw address to the string that represents imported subroutine.
type ImportMap = Map<RawAddr,string>

module ImportMap =

  let rec private tryResolveImport importName =
    let redirectedName = RedirectMap.resolve importName
    try Some (Subroutine.ofString redirectedName) with
    | PESymbol.UnfoundSymbolException -> None

  let resolve (importMap: ImportMap) =
    let folder acc addr importName =
      match tryResolveImport importName with
      | None -> acc
      | Some subrtn -> Map.add addr subrtn acc
    Map.fold folder Map.empty importMap

  let private addImportSymb accMap (s: BinFile.Symbol) =
    // Note that cases should be ignored (e.g. KernelBase vs KERNELBASE).
    let binName = (truncateBinExt s.LibraryName).ToLower()
    let subrtnName = sprintf "%s!%s" binName s.Name
    let rawAddr = RawAddr.ofUInt64 s.Address
    Map.add rawAddr subrtnName accMap

  let make binHandle: ImportMap =
    let dynamicSymbols = binHandle.FileInfo.GetDynamicSymbols() |> List.ofSeq
    let isImportSym (s: BinFile.Symbol) = s.Kind = SymbolKind.ExternFunctionType
    let importSymbols = List.filter isImportSym dynamicSymbols
    // Defer the conversion to address, since binaries are not all loaded yet.
    List.fold addImportSymb Map.empty importSymbols
