namespace DLLAnalysis.TypeAnalysis

open DLLAnalysis
open DLLAnalysis.FrontEnd

module Statistics =

  type BinStat = {
    TotalFuncs : int
    AnalyzedFuncs : int
    TotalBBs: int
    AnalyzedBBs : int
    TotalInsts : int
    AnalyzedInsts : int

  }

  let emptyCount =
    { TotalFuncs = 0
      AnalyzedFuncs = 0
      TotalBBs = 0
      AnalyzedBBs = 0
      TotalInsts = 0
      AnalyzedInsts = 0 }

  type StatMap = Map<Binary,BinStat>

  let private countInstrs acc bb =
    acc + List.length (BasicBlock.getInstrs bb)

  let private runAux analyzeSet accStatMap subrtn cfg =
    let bin = Subroutine.binOf subrtn
    let bbs = CFG.getBBs cfg
    let bbCnt = Set.count bbs
    let insCnt = Set.fold countInstrs 0 bbs
    let binStat =
      match Map.tryFind bin accStatMap with
      | None -> emptyCount
      | Some count -> count
    let binStat = { binStat with TotalFuncs = binStat.TotalFuncs + 1 }
    let binStat =
      if not (Set.contains subrtn analyzeSet) then binStat
      else { binStat with AnalyzedFuncs = binStat.AnalyzedFuncs + 1 }
    let binStat = { binStat with TotalBBs = binStat.TotalBBs + bbCnt }
    let binStat =
      if not (Set.contains subrtn analyzeSet) then binStat
      else { binStat with AnalyzedBBs = binStat.AnalyzedBBs + bbCnt }
    let binStat = { binStat with TotalInsts = binStat.TotalInsts + insCnt }
    let binStat =
      if not (Set.contains subrtn analyzeSet) then binStat
      else { binStat with AnalyzedInsts = binStat.AnalyzedInsts + insCnt }
    Map.add bin binStat accStatMap

  let private printBinStat bin stat =
    printfn "%s (Total) : #Funcs = %d, #BasicBlks = %d, #Instrs = %d"
      bin stat.TotalFuncs stat.TotalBBs stat.TotalInsts
    printfn "%s (Analyzed): #Funcs = %d, #BasicBlks = %d, #Instrs = %d"
      bin stat.AnalyzedFuncs stat.AnalyzedBBs stat.AnalyzedInsts

  let run cfgs stubs targets =
    let analyzeSet = Set.union stubs targets
    let binStats = Map.fold (runAux analyzeSet) Map.empty cfgs
    Map.iter printBinStat binStats
