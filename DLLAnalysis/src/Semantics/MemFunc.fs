module DLLAnalysis.AbstractSemantics.MemFunc

// Memcpy and memset semantics.

open DLLAnalysis
open DLLAnalysis.Const
open DLLAnalysis.AbstractDomain

let private addMemFuncSizeHint locs sizeArgInt state =
  if not (AbsInt.isConst sizeArgInt) then state
  else State.addSizeHint locs (AbsInt.getConst sizeArgInt) state

let private decideSize absInt =
  if AbsInt.isConst absInt then
    let constSize = AbsInt.getConst absInt
    min (Offset.ofInt MAX_MEMFUNC_SIZE) (NUInt.toNInt constSize)
  else (Offset.ofInt MIN_MEMFUNC_SIZE)

let rec private memcpyAux localInfo dstBases srcBases size offset acc =
  if size <= offset then acc
  else
    let record = LocalInfo.getRecord localInfo
    let footPrint = Some (LocalInfo.getFootPrint localInfo)
    let srcLocs = AbsLocSet.addOffset srcBases offset
    let dstLocs = AbsLocSet.addOffset dstBases offset
    let v = State.load footPrint record srcLocs acc // Value to copy.
    let acc = State.store footPrint record Variable.NoneVar dstLocs v acc
    let nextOffset = Offset.add offset Offset.WORD_SIZE
    memcpyAux localInfo dstBases srcBases size nextOffset acc

let memcpy localInfo dstBases srcBases sizeInt state =
  let copySize = decideSize sizeInt
  memcpyAux localInfo dstBases srcBases copySize Offset.ZERO state
  |> addMemFuncSizeHint dstBases sizeInt
  |> addMemFuncSizeHint srcBases sizeInt

let rec private memsetAux localInfo dstBases v size offset acc =
  if size <= offset then acc
  else
    let record = LocalInfo.getRecord localInfo
    let footPrintOpt = Some (LocalInfo.getFootPrint localInfo)
    let dstLocs = AbsLocSet.addOffset dstBases offset
    let acc = State.store footPrintOpt record Variable.NoneVar dstLocs v acc
    let nextOffset = Offset.add offset Offset.WORD_SIZE
    memsetAux localInfo dstBases v size nextOffset acc

let private decideValueToSet i =
  if AbsInt.isConst i && AbsInt.getConst i = NUInt.ZERO then AbsVal.zero
  else AbsVal.unknownInt

let memset localInfo dstBases setVal sizeInt state =
  let v = decideValueToSet setVal
  let copySize = decideSize sizeInt
  memsetAux localInfo dstBases v copySize Offset.ZERO state
  |> addMemFuncSizeHint dstBases sizeInt
