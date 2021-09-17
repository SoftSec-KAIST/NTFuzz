module DLLAnalysis.Debug

// For debugging during the development.

open System
open System.Collections.Generic
open DLLAnalysis
open DLLAnalysis.Utils
open DLLAnalysis.AbstractDomain

type DebugMap = Dictionary<Addr,State>
type RecordMap = Dictionary<Subroutine,Map<AbsLoc,Symbol>>
type NextMap = Dictionary<Addr,Addr>

let mutable curBin: Binary = ""
let mutable curRawAddr: RawAddr = RawAddr.ZERO
let private debugMap: DebugMap = Dictionary ()
let private recordMap: RecordMap = Dictionary ()
let private nextMap: NextMap = Dictionary ()

let dumpRecord subrtn (record: Record) =
  let locSymMap = Dictionary.toMap record.SLocMap
  recordMap.[subrtn] <- locSymMap

let dumpState addr state =
  debugMap.[addr] <- state

let rec recordNextAddrs bin rawAddrs =
  match rawAddrs with
  | [] -> () // unreachable
  | [rawAddr] -> nextMap.[(bin, rawAddr)] <- (bin, RawAddr.ZERO) // Last addr
  | rawAddr1 :: rawAddr2 :: tailRawAddrs ->
    nextMap.[(bin, rawAddr1)] <- (bin, rawAddr2)
    recordNextAddrs bin (rawAddr2 :: tailRawAddrs)

let private handleShowCmd () =
  let state = debugMap.[(curBin, curRawAddr)]
  printfn "State @ address %s" (Addr.toString (curBin, curRawAddr))
  println (State.toString state)

let private updateCurBin bin =
  curBin <- bin
  let addrs = List.ofSeq debugMap.Keys
  if not (List.exists (fun (bin, _) -> bin = curBin) addrs) then
    printfn "Invalid binary"

let private updateCurAddr rawAddr =
  if debugMap.ContainsKey (curBin, rawAddr)
  then printfn "Address = %x" rawAddr; curRawAddr <- rawAddr
  else println "Address not found"

let private handleBinCmd (cmd: string) =
  match cmd.Split([|' '|]) |> Array.toList with
  | _ :: binStr :: [] when binStr.Length > 0 -> updateCurBin binStr
  | _ -> println "'bin' command requires one argument"

let private handleAddrCmd (cmd: string) =
  match cmd.Split([|' '|]) |> Array.toList with
  | _ :: addrStr :: [] when addrStr.Length > 0 ->
    try updateCurAddr (RawAddr.ofString addrStr) with
    | :? FormatException -> printfn "Invalid address format: %s" addrStr
    | :? ArgumentException -> printfn "Invalid address argument: %s" addrStr
  | _ -> println "'addr' command requires one argument"

let private handleRegCmd (cmd: string) =
  match cmd.Split([|' '|]) |> Array.toList with
  | _ :: regStr :: [] when regStr.Length > 0 ->
    let state = debugMap.[(curBin, curRawAddr)]
    let regMap = State.getRegMap state
    Map.iter (fun (reg: string) v ->
      if reg.IndexOf regStr <> -1 then printfn "%s : %s" reg (AbsVal.toString v)
    ) regMap
  | _ -> println "'reg' command requires one argument"

let private handleCondCmd () =
  let state = debugMap.[(curBin, curRawAddr)]
  match State.getPruneMap state with
  | PruningMap.Bot -> ()
  | PruningMap.CondMap pruneMap ->
    Map.iter (fun (reg: string) cond ->
      printfn "%s : %s" reg (PruningCond.toString cond)
    ) pruneMap

let private handleLocCmd (cmd: string) =
  match cmd.Split([|' '|]) |> Array.toList with
  | _ :: locStr :: [] when locStr.Length > 0 ->
    let state = debugMap.[(curBin, curRawAddr)]
    let mem = State.getAbsMem state
    Map.iter (fun loc v ->
      let s = AbsLoc.toString loc
      if s.IndexOf locStr <> -1 then printfn "%s : %s" s (AbsVal.toString v)
    ) mem
  | _ -> println "'loc' command requires one argument"

let rec private locSymMapToStringAux locSymMap indent loc =
  let sym = Map.find loc locSymMap
  let entryStr = sprintf "%s%s --> %s" indent (AbsLoc.toString loc) sym
  let isChild l = // Check if child (i.e. location derived from 'loc').
    AbsLoc.isSymbolic l && (AbsLoc.getSymbol l = Symbol.appendLocSuffix sym)
  let locs = Map.keys locSymMap
  let childLocs = List.filter isChild locs |> List.sortBy AbsLoc.getOrder
  let childIndent = indent + "    "
  let mapper = locSymMapToStringAux locSymMap childIndent
  let childStrs = List.map mapper childLocs
  String.concat "\n" (entryStr :: childStrs)

let private handleRecordCmd (cmd: string) =
  match cmd.Split([|' '|]) |> Array.toList with
  | _ :: subrtnStr :: [] when subrtnStr.Length > 0 ->
    try
      let subrtn = Subroutine.ofString subrtnStr
      if recordMap.ContainsKey(subrtn)
      then printfn "%s" (LocSymMap.toString recordMap.[subrtn])
      else printfn "Subroutine %s not found" subrtnStr
    with _ -> printfn "Invalid subroutine string %s" subrtnStr
  | _ -> println "'record' command requires one argument"

let private handleEqualCmd (cmd: string) =
  match cmd.Split([|' '|]) |> Array.toList with
  | _ :: varStr :: [] when varStr.Length > 0 ->
    let state = debugMap.[(curBin, curRawAddr)]
    let eqRel = State.getEQRel state
    EQRelation.debugPrint varStr eqRel
  | _ -> println "'equal' command requires one argument"

let private handleNextCmd () =
  if curRawAddr = RawAddr.ZERO then
    println "You should run 'address' command first"
  elif snd nextMap.[(curBin, curRawAddr)] = RawAddr.ZERO then
    printfn "%x: BB end" curRawAddr
  else
    updateCurAddr (snd nextMap.[(curBin, curRawAddr)])

let private printHelp () =
  println "Supported commands:"
  println "  help"
  println "  quit"
  println "  exit"
  println "  bin"
  println "  addr"
  println "  show"
  println "  reg"
  println "  cond"
  println "  loc"
  println "  equal"
  println "  next"

let rec debugShell () =
  print "(Debug) $ "
  let cmd = readInput ()
  match cmd.Split([|' '|]).[0].ToLower() with
  | "help" -> printHelp (); debugShell()
  | "quit" | "exit" -> ()
  | "bin" -> handleBinCmd cmd; debugShell ()
  | "addr" | "a" -> handleAddrCmd cmd; debugShell ()
  | "show" | "s" -> handleShowCmd (); debugShell ()
  | "reg" | "r" -> handleRegCmd cmd; debugShell ()
  | "cond" | "c" -> handleCondCmd (); debugShell ()
  | "loc" | "l" -> handleLocCmd cmd; debugShell ()
  | "record" | "rec" -> handleRecordCmd cmd; debugShell ()
  | "equal" | "e" -> handleEqualCmd cmd; debugShell ()
  | "next" | "n" -> handleNextCmd (); debugShell ()
  | _ -> println "Invalid command"; debugShell ()
