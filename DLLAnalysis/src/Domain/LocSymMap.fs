module DLLAnalysis.AbsDom.LocSymMap

open DLLAnalysis
open DLLAnalysis.AbsDom.AbsLoc
open DLLAnalysis.AbsDom.Record

/// A presistent mapping made from 'Record', which is an imperative dictionary.
type LocSymMap = Map<AbsLoc,Symbol>

// When declare this as module and alias it within Wrapper.fs, F# seems to have
// problem in distinguishing 'LocSymMap' type and 'LocSymMap' module.
type LocSymMapModule () =

  // Recursively extend used symbol set by examining loc-symbol map.
  member private __.extendUsedSyms locSymMap usedSyms =
    let folder accSyms loc sym =
      if Set.contains sym usedSyms && AbsLoc.isSymbolic loc then
        Set.add (Symbol.truncateSuffix (AbsLoc.getSymbol loc)) accSyms
      else accSyms
    let newUsedSyms = Map.fold folder usedSyms locSymMap
    if Set.isSubset newUsedSyms usedSyms then usedSyms
    else __.extendUsedSyms locSymMap newUsedSyms

  member __.make (record: Record) usedSyms =
    let locSymMap = Dictionary.toMap record.SLocMap
    let realUsedSyms = __.extendUsedSyms locSymMap usedSyms
    let folder accMap loc sym =
      if Set.contains sym realUsedSyms then Map.add loc sym accMap else accMap
    Map.fold folder Map.empty locSymMap

  member private __.toStringAux locSymMap indent loc =
    let sym = Map.find loc locSymMap
    let entryStr = sprintf "%s%s --> %s" indent (AbsLoc.toString loc) sym
    let isChild l = // Check if child (i.e. location derived from 'loc').
      AbsLoc.isSymbolic l && (AbsLoc.getSymbol l = Symbol.appendLocSuffix sym)
    let locs = Map.keys locSymMap
    let childLocs = List.filter isChild locs |> List.sortBy AbsLoc.getOrder
    let childIndent = indent + "    "
    let childStrs = List.map (__.toStringAux locSymMap childIndent) childLocs
    String.concat "\n" (entryStr :: childStrs)

  member __.toString locSymMap =
    let locs = Map.keys locSymMap
    let rootLocs = List.filter AbsLoc.isRootArgument locs
    List.sortBy (AbsLoc.getOrder) rootLocs // Sort root locations.
    |> List.map (__.toStringAux locSymMap "") // Stringfy each root location.
    |> String.concat "\n"

// Now we can use 'LocSymMap' like a module.
 let LocSymMap = LocSymMapModule ()