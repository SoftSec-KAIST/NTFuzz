namespace DLLAnalysis

open System.Collections.Generic
open DLLAnalysis.Const

/// Index of system call argument.
type ArgIndex = int

/// A system call stub, is also a subroutine.
type Stub = Subroutine

module Stub =
  // Stringfy like a subroutine, but unify to start wiht NT-, for consistency.
  let toString stub =
    let bin = Subroutine.binOf stub
    let names = Subroutine.toNameAll stub
    match List.tryFind (fun (n: string) -> n.StartsWith("Nt")) names with
    | None -> failwithf "[Invalid] Stub without syscall prefix: %A" names
    | Some name -> sprintf "%s!%s" bin name

  let ofString = Subroutine.ofString

  let binOf = Subroutine.binOf

  let isBlacklist stub =
    let str = toString stub
    Set.contains str BLACKLIST_SYSCALLS

/// Record to store information about a syscall stub.
type StubInfo = {
  ArgNum : int
  SysNum : int
}

module StubInfo =
  let private stubs: HashSet<Stub> = HashSet()
  let private dict: Dictionary<Stub,StubInfo> = Dictionary ()

  let getStubs () =
    Set.ofSeq stubs

  let addStub (subrtn: Subroutine) argNum sysNum =
    stubs.Add(subrtn) |> ignore
    dict.[subrtn] <- { ArgNum = argNum; SysNum = sysNum }

  let isStub (subrtn: Subroutine) =
    stubs.Contains(subrtn)

  let getArgNum (stub: Subroutine) =
    dict.[stub].ArgNum

  let getSysNum (stub: Subroutine) =
    dict.[stub].SysNum
