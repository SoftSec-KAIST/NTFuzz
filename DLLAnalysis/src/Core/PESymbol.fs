namespace DLLAnalysis

open System.Collections.Generic
open B2R2.FrontEnd // For struct lookup.
open DLLAnalysis.Utils

/// An imperative module to store PE function symbol information.
module PESymbol =
  exception UnfoundSymbolException

  let private addrToName: Dictionary<Addr,string list> = Dictionary ()
  let private nameToAddr: Dictionary<string,Addr> = Dictionary ()
  let private fNameToAddr: Dictionary<string,Addr list> = Dictionary ()

  let private updateAddrToName addr name =
    if not (addrToName.ContainsKey(addr)) then
      addrToName.[addr] <- [name]
    elif addrToName.[addr].Head.Contains("!Zw") && name.Contains("!Nt") then
      addrToName.[addr] <- name :: addrToName.[addr] // Override Zw- prefix.
    elif addrToName.[addr].Head.Contains("!#") then
      addrToName.[addr] <- name :: addrToName.[addr] // Override ordinal name.
    else // O/w, maintain the order, to prefer export names over PDB names.
      addrToName.[addr] <- addrToName.[addr] @ [name]

  let private updateNameToAddr name addr =
    if not (nameToAddr.ContainsKey(name)) then nameToAddr.[name] <- addr

  let private updateFuncNameToAddr fName addr =
    if not (fNameToAddr.ContainsKey(fName)) then fNameToAddr.[fName] <- [addr]
    else fNameToAddr.[fName] <- addr :: fNameToAddr.[fName]

  let private updateSymbolMap binName sym =
    let addr = Addr.makeWithUI64 binName (B2R2Symbol.getAddr sym)
    let name = sprintf "%s!%s" binName sym.Name
    updateAddrToName addr name
    updateNameToAddr name addr
    updateFuncNameToAddr sym.Name addr

  let private initAux (binName, binHandle) =
    let exportSymbols = binHandle.FileInfo.GetDynamicSymbols(true)
                        |> Seq.toList
                        |> List.filter B2R2Symbol.isFunctionSymbol
    let exportNames = List.map B2R2Symbol.getName exportSymbols |> Set.ofList
    let isExportedName sym = Set.contains (B2R2Symbol.getName sym) exportNames
    let pdbSymbols = binHandle.FileInfo.GetStaticSymbols()
                     |> Seq.toList
                     // If we already have symbol in export table, stick to it.
                     |> List.filter (not << isExportedName)
    // Since there are many garbage PDB symbols in kernelbase.dll, prioritize
    // symbols from export table by updating with export symbols first.
    List.iter (updateSymbolMap binName) exportSymbols
    List.iter (updateSymbolMap binName) pdbSymbols

  let init binaries =
    List.iter initAux binaries

  let getNames (addr: Addr) =
    if addrToName.ContainsKey(addr) then addrToName.[addr] else []

  let getAddr (name: string) =
    // Normalize binary name to lowercase.
    if not (name.Contains("!")) then failwithf "Invalid addr string: %s" name
    let idx = name.IndexOf("!")
    let bin = name.[ .. (idx - 1) ]
    let name = bin.ToLower() + name.[idx .. ]
    if nameToAddr.ContainsKey(name) then nameToAddr.[name]
    else raise UnfoundSymbolException

  let rec getAddrs (fName: string) =
    if fNameToAddr.ContainsKey(fName) then fNameToAddr.[fName] else []
