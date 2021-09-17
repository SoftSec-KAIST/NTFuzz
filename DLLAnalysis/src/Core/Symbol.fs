namespace DLLAnalysis

open DLLAnalysis.Const

/// Symbols used in modular analysis.
type Symbol = string

module Symbol =
  let toString sym = sym

  let appendIntSuffix prefix =
    prefix + INT_SUFFIX

  let appendLocSuffix prefix =
    prefix + LOC_SUFFIX

  let appendFunctionSuffix prefix =
    prefix + FUNC_SUFFIX

  let appendTypSuffix prefix =
    prefix + TYP_SUFFIX

  let truncateSuffix (sym: string) =
    if sym.EndsWith(INT_SUFFIX) then sym.Remove(sym.IndexOf(INT_SUFFIX))
    elif sym.EndsWith(LOC_SUFFIX) then sym.Remove(sym.IndexOf(LOC_SUFFIX))
    elif sym.EndsWith(FUNC_SUFFIX) then sym.Remove(sym.IndexOf(FUNC_SUFFIX))
    elif sym.EndsWith(TYP_SUFFIX) then sym.Remove(sym.IndexOf(TYP_SUFFIX))
    else failwith "Invalid symbol suffix"
