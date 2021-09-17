namespace DLLAnalysis

open System.Collections.Generic
open WinHeader
open WinHeader.Constants
open WinHeader.TypeHelper
open DLLAnalysis.Const

module APIInfo =

  let private dict = Dictionary<string,FuncType>()

  let initialize apiNames =
    let ctagsPath = System.IO.Path.Combine(HEADER_DIR, CTAGS_FILENAME)
    let ctagsMap = CtagsMap.initialize HEADER_DIR ctagsPath
    let apis = APIMap.makeWithList ctagsMap apiNames
    let iterator name fType = dict.[name] <- fType
    APIMap.iter iterator apis

  let isAPI subrtn =
    let subrtnNames = Subroutine.toStringAll subrtn
    let aliasNames = List.collect RedirectMap.findAliases subrtnNames
    let lookupNames = subrtnNames @ aliasNames |> List.map Subroutine.fetchName
    List.exists (fun n -> dict.ContainsKey(n)) lookupNames

  let tryFindAPI subrtn =
    let subrtnNames = Subroutine.toStringAll subrtn
    let aliasNames = List.collect RedirectMap.findAliases subrtnNames
    let lookupNames = subrtnNames @ aliasNames |> List.map Subroutine.fetchName
    match List.tryFind (fun n -> dict.ContainsKey(n)) lookupNames with
    | None -> None
    | Some name -> Some dict.[name]

  /// An add-hoc funciton to lookup type definition.
  let findType typName =
    let ctagsPath = System.IO.Path.Combine(HEADER_DIR, CTAGS_FILENAME)
    let ctagsMap = CtagsMap.initialize HEADER_DIR ctagsPath
    FindType.run ctagsMap typName
    |> ArgType.ofCType ctagsMap Env.empty
