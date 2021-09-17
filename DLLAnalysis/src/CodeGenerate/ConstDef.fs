module DLLAnalysis.CodeGen.ConstDef

open B2R2
open DLLAnalysis

let private pragma = "#pragma once\n"

let private extractShadowTable (symbols: BinFile.Symbol list) =
  let name = "_KeServiceDescriptorTableShadow"
  match List.tryFind (fun (s: BinFile.Symbol) -> s.Name = name) symbols with
  | None -> failwith "Failed to find descriptor table symbol"
  | Some s -> s.Address

let private extractExAlloc (symbols: BinFile.Symbol list) =
  let name = "ExAllocatePoolWithTag"
  match List.tryFind (fun (s: BinFile.Symbol) -> s.Name = name) symbols with
  | None -> failwith "Failed to find allocation function symbol"
  | Some s -> s.Address

let private extractCreateProcSysNum () =
  let stubs = StubInfo.getStubs() |> Set.toList
  let name = "ntdll!NtCreateUserProcess"
  match List.tryFind (fun (s: Stub) -> Stub.toString s = name) stubs with
  | None -> failwith "Failed to find NtCreateUserProcess syscall"
  | Some s -> StubInfo.getSysNum s

let generate ntosBin ntosPDB constFile =
  let binBytes = System.IO.File.ReadAllBytes(ntosBin)
  let pdbBytes = System.IO.File.ReadAllBytes(ntosPDB)
  let peFile = B2R2.BinFile.PEFileInfo(binBytes, ntosBin, pdbBytes)
  let imageBase = peFile.GetImageBase()
  let dynamicSymbols = peFile.GetDynamicSymbols() |> List.ofSeq
  let staticSymbols = peFile.GetStaticSymbols() |> List.ofSeq
  let commentAlloc = "// Offset of nt!ExAllocatePoolWithTag."
  let allocOffset = extractExAlloc dynamicSymbols - imageBase
  let defineAlloc = sprintf "#define EXALLOC_OFFSET 0x%x" allocOffset
  let alloc = commentAlloc + "\n" + defineAlloc
  let commentTable = "// Offset of nt!KeServiceDescriptorTableShadow."
  let tableOffset = extractShadowTable staticSymbols - imageBase
  let defineTable = sprintf "#define SHADOW_TABLE_OFFSET 0x%x" tableOffset
  let table = commentTable + "\n" + defineTable
  let commentCreateProc = "// Syscall number of NtCreateUserProcess()."
  let sysNum = extractCreateProcSysNum ()
  let defineSysNum = sprintf "#define CREATE_USER_PROC_SYSNUM %d" sysNum
  let createProc = commentCreateProc + "\n" + defineSysNum
  let code = [pragma; alloc; table; createProc] |> String.concat "\n"
  System.IO.File.WriteAllText(constFile, code)
