namespace DLLAnalysis.TypeAnalysis

open DLLAnalysis
open DLLAnalysis.Utils
open WinHeader

type TypeSpec = Map<Stub,ArgType list>

module TypeSpec =

  let empty: TypeSpec = Map.empty

  let add stub argTyps (typeSpec: TypeSpec) : TypeSpec =
    Map.add stub argTyps typeSpec

  let private makeDummyType stub =
    let argNum = StubInfo.getArgNum stub
    let mapper _ = UnknownType
    List.map mapper (List.ofSeq { 0 .. (argNum - 1) })

  let addMissing (typeSpec: TypeSpec) =
    let stubs = StubInfo.getStubs()
    // Fill in stub type if not present in type map.
    let folder accMap stub =
      if Map.containsKey stub accMap then accMap
      else Map.add stub (makeDummyType stub) accMap
    Set.fold folder typeSpec stubs

  let addHeaders (typeSpec: TypeSpec) =
    let stubs = StubInfo.getStubs()
    let folder accMap stub =
      match APIInfo.tryFindAPI stub with
      | None -> accMap
      | Some typ -> Map.add stub typ accMap
    Set.fold folder typeSpec stubs

  let addEncoded (typeSpec: TypeSpec) =
    let folder accMap name typ =
      try Map.add (Stub.ofString name) typ accMap with
      | PESymbol.UnfoundSymbolException -> accMap
    Map.fold folder typeSpec Primitives.ENCODED_SYSCALL_TYPES

  let removeBlacklisted (typeSpec: TypeSpec): TypeSpec =
    Map.filter (fun stub _ -> not (Stub.isBlacklist stub)) typeSpec

  let writeToFile (typeSpec: TypeSpec) outFile  =
    let mapper (stub, typ) =
      let stubStr = Stub.toString stub
      let sysNum = StubInfo.getSysNum stub
      let funcStr = FuncType.toJson sysNum typ
      sprintf "\"%s\":%s" stubStr funcStr
    let entries = Map.toList typeSpec |> List.map mapper |> List.sort
    let header = "{\n  "
    let body = String.concat ",\n  " entries
    let footer = "\n}"
    let outStr = header + body + footer
    System.IO.File.WriteAllText(outFile, outStr)

  let private writeSingleType outDir stub funcType =
    let sysNum = StubInfo.getSysNum stub
    let stubStr = Stub.toString stub
    let funcStr = FuncType.toJson sysNum funcType
    let header = "{\n  "
    let body = sprintf "\"%s\":%s" stubStr funcStr
    let footer = "\n}"
    let outStr = header + body + footer
    let idx = stubStr.IndexOf("!")
    let fileName = stubStr.[idx + 1 .. ] + ".json"
    let outFile = System.IO.Path.Combine(outDir, fileName)
    System.IO.File.WriteAllText(outFile, outStr)

  let writeToSeparateFiles (typeSpec: TypeSpec) outDir =
    makeDirectory outDir
    Map.iter (writeSingleType outDir) typeSpec

  let private countArg pred (typeSpec: TypeSpec) =
    let folder accN _ argTyps = accN + List.length (List.filter pred argTyps)
    Map.fold folder 0 typeSpec

  let private isUnsoundTarget t =
    not (ArgType.isDWord t || ArgType.isUnknown t)

  let private makeUnsoundAux chosens (accN, argTyps) =
    let folder (accN, accTyps) t =
      if isUnsoundTarget t then
        let t = if Set.contains accN chosens then ArgType.Scalar 4UL else t
        (accN + 1, t :: accTyps)
      else (accN, t :: accTyps)
    List.fold folder (accN, []) argTyps

  let makeUnsound ratio (typeSpec: TypeSpec): TypeSpec =
    let totalN = countArg isUnsoundTarget typeSpec
    let chooseN = int (float totalN * ratio)
    let chosens = randomSubset totalN chooseN
    let folder (accN, accMap) stub argTyps =
      let accN, argTyps = makeUnsoundAux chosens (accN, argTyps)
      (accN, Map.add stub (List.rev argTyps) accMap)
    Map.fold folder (0, Map.empty) typeSpec |> snd

  let private isImpreciseTarget = function
    | Ptr _ -> true
    | _ -> false

  let private makeImpreciseAux chosens (accN, argTyps) =
    let folder (accN, accTyps) t =
      if isImpreciseTarget t then
        let t = if Set.contains accN chosens then ArgType.makeImprecise t else t
        (accN + 1, t :: accTyps)
      else (accN, t :: accTyps)
    List.fold folder (accN, []) argTyps

  let makeImprecise ratio (typeSpec: TypeSpec): TypeSpec =
    let totalN = countArg isImpreciseTarget typeSpec
    let chooseN = int (float totalN * ratio)
    let chosens = randomSubset totalN chooseN
    let folder (accN, accMap) stub argTyps =
      let accN, argTyps = makeImpreciseAux chosens (accN, argTyps)
      (accN, Map.add stub (List.rev argTyps) accMap)
    Map.fold folder (0, Map.empty) typeSpec |> snd

