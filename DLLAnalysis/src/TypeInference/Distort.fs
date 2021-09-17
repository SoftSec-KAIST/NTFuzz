module DLLAnalysis.TypeAnalysis.Distort

open DLLAnalysis.Utils

let private UNSOUND_RATIOS = [0.25; 0.5; 0.75; 1.00]
let private IMPRECISE_RATIOS = [0.25; 0.5; 0.75; 1.00]

let private appendSuffix (outFile: string) suffix  =
  if not (outFile.Contains(".")) then outFile + suffix
  else let idx = outFile.LastIndexOf('.')
       outFile.[..(idx - 1)] + suffix + outFile.[idx..]

let private storeUnsoundType typeSpec outFileOrig ratio iter =
  logInfo "Make types unsound with ratio = %.2f (trial %d)" ratio iter
  let typeSpecDeflated = TypeSpec.makeUnsound ratio typeSpec
  let suffix = sprintf "_unsound_%d_%d" (int (100.0 * ratio)) iter
  let outFile = appendSuffix outFileOrig suffix
  TypeSpec.writeToFile typeSpecDeflated outFile

let private storeUnsoundTypes typeSpec outFileOrig iterN ratio =
  Seq.iter (storeUnsoundType typeSpec outFileOrig ratio) { 1 .. iterN }

let private storeImpreciseType typeSpec outFileOrig ratio iter =
  logInfo "Make types imprecise with ratio = %.2f (trial %d)" ratio iter
  let typeSpecInflated = TypeSpec.makeImprecise ratio typeSpec
  let suffix = sprintf "_imprecise_%d_%d" (int (100.0 * ratio)) iter
  let outFile = appendSuffix outFileOrig suffix
  TypeSpec.writeToFile typeSpecInflated outFile

let private storeImpreciseTypes typeSpec outFileOrig iterN ratio =
  Seq.iter (storeImpreciseType typeSpec outFileOrig ratio) { 1 .. iterN }

// Artificially introduce inaccuracy (unsoundness and imprecision) to type
// specificaion. This is for the ablation experiment in our paper.
let run typeSpec outFile iterN =
  List.iter (storeUnsoundTypes typeSpec outFile iterN) UNSOUND_RATIOS
  List.iter (storeImpreciseTypes typeSpec outFile iterN) IMPRECISE_RATIOS
