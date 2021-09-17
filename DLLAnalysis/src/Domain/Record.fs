module DLLAnalysis.AbsDom.Record

open WinHeader
open System.Collections.Generic
open DLLAnalysis
open DLLAnalysis.Const
open DLLAnalysis.AbsDom.AbsLoc
open DLLAnalysis.AbsDom.AbsVal

type Record = {
  // To give a unique name to symbols dynamically created during analysis.
  SymbolCounter : int ref
  // Memory that maps a dereferenced loc to its corresponding value.
  SLocMap : Dictionary<AbsLoc,Symbol>
  ConstrMap: Dictionary<int,Set<ArgType>>
}

type RecordModule () =

  member __.create () =
    { SymbolCounter = ref 0
      SLocMap = Dictionary ()
      ConstrMap = Dictionary () }

  // Create a new symbol.
  member __.createSymbol record =
    let counter = !record.SymbolCounter
    record.SymbolCounter := counter + 1
    sprintf "%s%d" SYMBOL_PREFIX counter

  member __.getLocs record =
    Dictionary.keys record.SLocMap

  member __.hasLoc loc record = record.SLocMap.ContainsKey(loc)

  member __.findLoc loc record = record.SLocMap.[loc]

  member __.addLoc loc v record = record.SLocMap.[loc] <- v

  member __.addConstr argIdx typs record =
    if record.ConstrMap.ContainsKey(argIdx)
    then record.ConstrMap.[argIdx] <- Set.union record.ConstrMap.[argIdx] typs
    else record.ConstrMap.[argIdx] <- typs

  member __.spawnSymbolicVal loc record =
    let symbol = __.createSymbol record
    __.addLoc loc symbol record
    AbsVal.ofSymbol symbol

 // Now we can use 'Record' like a module.
 let Record = RecordModule ()