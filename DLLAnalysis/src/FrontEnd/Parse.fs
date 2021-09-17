module DLLAnalysis.FrontEnd.Parser

open B2R2
open B2R2.FrontEnd
open B2R2.BinCorpus
open B2R2.MiddleEnd
open DLLAnalysis
open DLLAnalysis.Utils

let getBinName (binFile: string) =
  System.IO.Path.GetFileName(binFile) |> truncateBinExt

let makeBinHandle (binFile: string) =
  BinHandler.Init (ISA.DefaultISA, ArchOperationMode.NoMode, true, 0UL, binFile)

let getEntries binEssence =
  Apparatus.getInternalFunctions binEssence.Apparatus
  |> Set.ofSeq
  |> Set.choose (fun (f: BinCorpus.Callee) -> f.Addr)

let private parseBinary (accCFGs, accBinInfos) (binName, binHandle) =
  logInfo "Running B2R2 frontend on DLL '%s'..." binName
  let binEssence = BinEssence.Init binHandle
  let entries = getEntries binEssence
  let folder acc entry =
    let cfg = CFG.make binEssence entry
    let addr = Addr.makeWithUI64 binName entry
    Map.add addr cfg acc
  logInfo "Constructing our own CFG with %d entries..." (Set.count entries)
  let accCFGs = Set.fold folder accCFGs entries
  logInfo "Creating binary information struct..."
  let binInfo = BinInfo.make binName binHandle accCFGs entries
  let accBinInfos = Map.add binName binInfo accBinInfos
  (accCFGs, accBinInfos)

let run binFiles : Map<Subroutine,CFG> * Map<Binary,BinInfo> =
  List.iter checkFileExist binFiles
  let binNames = List.map getBinName binFiles
  let binHandles = List.map makeBinHandle binFiles
  let binaries = List.zip binNames binHandles
  let initAcc = (Map.empty, Map.empty)
  logInfo "Initializing PDB symbol information..."
  PESymbol.init binaries
  logInfo "Initializing redirect map information..."
  RedirectMap.init binaries
  let cfgs, binInfos = List.fold parseBinary initAcc binaries
  (cfgs, binInfos)
