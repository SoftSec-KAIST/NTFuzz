namespace DLLAnalysis.DataFlow

open System.Collections.Generic
open DLLAnalysis
open DLLAnalysis.AbsDom.AbsLoc

type FootPrint = {
  SizeHints : Dictionary<AbsLoc,int64>
  Loads : Dictionary<PPoint,AbsLocSet>
  Stores : Dictionary<PPoint,AbsLocSet>
}

module FootPrint =

  let create () =
    { SizeHints = Dictionary ()
      Loads = Dictionary ()
      Stores = Dictionary () }

  let private addLocs ppoint locs (locDict: Dictionary<PPoint,AbsLocSet>) =
    // Filter out non-stack locations and argument backup locations.
    let locs = AbsLocSet.filter AbsLoc.isStack locs
               |> AbsLocSet.filter (fun l -> AbsLoc.getOffset l < Offset.ZERO)
    if locDict.ContainsKey(ppoint) then
      locDict.[ppoint] <- AbsLocSet.join locs locDict.[ppoint]
    else locDict.[ppoint] <- locs

  let addLoad ppoint locs footPrint =
    addLocs ppoint locs footPrint.Loads

  let addStore ppoint locs footPrint =
    addLocs ppoint locs footPrint.Stores

  let getLoads ppoint footPrint =
    let loadDict = footPrint.Loads
    if loadDict.ContainsKey(ppoint) then loadDict.[ppoint] else AbsLocSet.bot

  let getStores ppoint footPrint =
    let storeDict = footPrint.Stores
    if storeDict.ContainsKey(ppoint) then storeDict.[ppoint] else AbsLocSet.bot

  let getAllLocs footPrint =
    Dictionary.vals footPrint.Loads @ Dictionary.vals footPrint.Stores
    |> List.fold Set.union Set.empty
