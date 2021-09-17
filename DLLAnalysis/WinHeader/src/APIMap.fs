namespace WinHeader

open WinHeader.Constants
open WinHeader.Utils

type APIMap = Map<string,FuncType>

module APIMap =

  let empty: APIMap = Map.empty

  let makeAux ctagsMap accMap f =
    match FindFunction.run ctagsMap f with
    | None -> accMap
    | Some funcDecl -> Map.add f (FuncType.ofCFunction ctagsMap funcDecl) accMap

  let makeWithList ctagsMap funcList =
    let folder accMap f =
      try makeAux ctagsMap accMap f with
      | Failure msg -> accMap
    List.fold folder Map.empty funcList

  let makeWithFile ctagsMap funcFile =
    checkFileExists funcFile
    let funcList = System.IO.File.ReadAllText(funcFile).Trim().Split([|'\n'|])
                   |> Array.toList
    makeWithList ctagsMap funcList

  let contains: string -> APIMap -> bool = Map.containsKey

  let find: string -> APIMap -> FuncType = Map.find

  let tryFind: string -> APIMap -> FuncType option = Map.tryFind

  let iter: (string -> FuncType -> unit) -> APIMap -> unit =
    Map.iter
