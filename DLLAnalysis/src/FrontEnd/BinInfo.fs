namespace DLLAnalysis.FrontEnd

open B2R2.BinFile
open B2R2.FrontEnd // For struct lookup.
open DLLAnalysis

type BinInfo = {
  BinName : string
  ImageBase : RawAddr
  CodeRanges : (RawAddr * RawAddr) list
  SectionStart : RawAddr
  SectionEnd : RawAddr
  Entries : Set<RawAddr>
  ImportMap : Map<RawAddr,string> // String that represents imported subroutine.
  ExportEntries : Set<RawAddr>
}

module BinInfo =
  let private CODE_SECTIONS = [".text"; "RT"]
  let private UNINTERESTING_SECTIONS = [".rsrc"; ".reloc"]

  let private isCodeSection (sec: Section) =
    sec.Kind = SectionKind.ExecutableSection &&
    List.contains sec.Name CODE_SECTIONS

  let private isInterestingSection (sec: Section) =
    not (List.contains sec.Name UNINTERESTING_SECTIONS)

  let private getCodeRange binName cfgs entries (sec: Section) =
    let start = RawAddr.ofUInt64 sec.Address
    let finish = RawAddr.ofUInt64 (sec.Address + sec.Size)
    let entries = Set.filter (fun e -> start <= e && e <= finish) entries
    let funcStart = Set.minElement entries
    let lastEntry = Set.maxElement entries
    let lastCFG = Map.find (Addr.makeWithRawAddr binName lastEntry) cfgs
    let lastCFGBBs = CFG.getBBs lastCFG |> Set.toList
    let lastCFGAddrs = List.collect BasicBlock.getRawAddrs lastCFGBBs
    let funcEnd = List.max lastCFGAddrs
    let realStart = max start funcStart
    let realFinish = min finish funcEnd
    (realStart, realFinish)

  let private getSectionRange (sec: Section) =
    let start = RawAddr.ofUInt64 sec.Address
    let finish = RawAddr.ofUInt64 (sec.Address + sec.Size)
    (start, finish)

  let makeExportEntries binHandle =
    let peInfo = binHandle.FileInfo :?> PEFileInfo // XXX. Windows.
    peInfo.GetExportAddrs() |> Set.ofList |> Set.map RawAddr.ofUInt64

  let make binName binHandle cfgs entries =
    let entries = Set.map RawAddr.ofUInt64 entries
    let peInfo = binHandle.FileInfo :?> PEFileInfo // XXX. Windows.
    let imageBase = peInfo.GetImageBase() |> RawAddr.ofUInt64
    let sections = Seq.toList (binHandle.FileInfo.GetSections ())
                   |> List.filter isInterestingSection
    let codeSections = List.filter isCodeSection sections
    if List.isEmpty codeSections then failwith "No code section found"
    let codeRanges = List.map (getCodeRange binName cfgs entries) codeSections
    let sectionRanges = List.map getSectionRange sections
    let sectionStart = List.minBy fst sectionRanges |> fst
    let sectionEnd = List.maxBy snd sectionRanges |> snd
    let importMap = ImportMap.make binHandle
    let exportEntries = makeExportEntries binHandle
    { BinName = binName
      ImageBase = imageBase
      CodeRanges = codeRanges
      SectionStart = sectionStart
      SectionEnd = sectionEnd
      Entries = entries
      ImportMap = importMap
      ExportEntries = exportEntries }

  let isCodeAddr binInfo rawAddr =
    let checker (sAddr, eAddr) = sAddr <= rawAddr && rawAddr <= eAddr
    List.exists checker binInfo.CodeRanges

  let isGlobalAddr binInfo rawAddr =
    binInfo.SectionStart <= rawAddr && rawAddr <= binInfo.SectionEnd

  let isSubrtnEntry binInfo rawAddr =
    Set.contains rawAddr binInfo.Entries

  let isExportEntry binInfo rawAddr =
    Set.contains rawAddr binInfo.ExportEntries

  let getImportMap binInfo =
    ImportMap.resolve binInfo.ImportMap
