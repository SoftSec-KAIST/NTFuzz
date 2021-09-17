namespace DLLAnalysis.TypeAnalysis

open DLLAnalysis
open DLLAnalysis.Utils
open DLLAnalysis.AbstractDomain
open DLLAnalysis.Summary
open WinHeader

type Position = (AbsLoc * Offset) list

module Position =
  let private entryToStr (loc, off) =
    sprintf "%s + %s" (AbsLoc.toString loc) (Offset.toString off)

  let toString position =
    List.map entryToStr position
    |> String.concat ", "
    |> sprintf "[%s]"

type TypeInfo = {
  Constrs : ArgType list
  StructTypes : Map<AbsLoc, StructInfo>
}

and StructInfo = {
  Sizes : Size list
  FieldTypes : Map<Offset, TypeInfo>
}

module TypeInfo =

  let empty = { Constrs = []; StructTypes = Map.empty }

  let rec toString indent (typeInfo: TypeInfo) =
    let typStr = List.map (ArgType.toJson true (indent + "  ")) typeInfo.Constrs
                 |> String.concat ","
    let structStr = Map.toList typeInfo.StructTypes
                    |> List.map (structInfoToString (indent + "  "))
                    |> String.concat "\n"
    if Map.isEmpty typeInfo.StructTypes
    then sprintf "Types: %s" typStr
    else sprintf "Types: %s" typStr +
         sprintf "\n%sStruct: %s" indent structStr

  and structInfoToString indent (baseLoc, struc) =
    let mapper (offset, fldTyp) =
      let indent = indent + "  " // Indent once more.
      sprintf "%s%x -> %s" indent offset (toString indent fldTyp)
    let mapStr = Map.toList struc.FieldTypes
                 |> List.map mapper
                 |> String.concat "\n"
    sprintf "\n%sBase: %s" indent (AbsLoc.toString baseLoc) +
    sprintf "\n%sMap:\n%s" indent mapStr

module StructInfo =

  let empty = { Sizes = []; FieldTypes = Map.empty }

  let toString = TypeInfo.structInfoToString

type CallsiteMap = Map<Addr, TypeInfo>

module CallsiteMap =
  let (empty: CallsiteMap) = Map.empty

  let lookup callsite (callsiteMap: CallsiteMap) =
    match Map.tryFind callsite callsiteMap with
    | None -> TypeInfo.empty
    | Some typ -> typ

  let update = Map.add

type TypeMap = Map<Stub * int, CallsiteMap>

module TypeMap =

  let (empty: TypeMap) = Map.empty

  let getStubKeys (typeMap:TypeMap) =
    Map.keys typeMap |> List.map fst |> Set.ofList

  let lookup stub idx (typeMap: TypeMap) =
    match Map.tryFind (stub, idx) typeMap with
    | None -> Map.empty
    | Some map -> map

  let update stub idx callsiteMap (typeMap: TypeMap) =
    Map.add (stub, idx) callsiteMap typeMap

  let rec private addTypeAux (position: Position) argTypes typeInfo =
    match position with
    | [] -> { typeInfo with Constrs = argTypes @ typeInfo.Constrs }
    | (baseLoc, offset) :: tailPosition ->
      let structInfo =
        match Map.tryFind baseLoc typeInfo.StructTypes with
        | None -> StructInfo.empty
        | Some sInfo -> sInfo
      let fieldInfo =
        match Map.tryFind offset structInfo.FieldTypes with
        | None -> TypeInfo.empty
        | Some tInfo -> tInfo
      let newFieldInfo = addTypeAux tailPosition argTypes fieldInfo
      let newFieldTypes = Map.add offset newFieldInfo structInfo.FieldTypes
      let newStructInfo = { structInfo with FieldTypes = newFieldTypes }
      let newStructTypes = Map.add baseLoc newStructInfo typeInfo.StructTypes
      { typeInfo with StructTypes = newStructTypes }

  let addType syscall idx position argTypes (typeMap: TypeMap) =
    let callsiteMap = lookup syscall.Stub idx typeMap
    let typ = CallsiteMap.lookup syscall.Callsite callsiteMap
    let newTyp = addTypeAux position argTypes typ
    let newCallsiteMap = CallsiteMap.update syscall.Callsite newTyp callsiteMap
    update syscall.Stub idx newCallsiteMap typeMap

  let rec private addSizeAux position lastBase size typeInfo =
    match position with
    | [] ->
      let structInfo =
        match Map.tryFind lastBase typeInfo.StructTypes with
        | None -> StructInfo.empty
        | Some sInfo -> sInfo
      let newSizes = size :: structInfo.Sizes
      let newStructInfo = { structInfo with Sizes = newSizes }
      let newStructTypes = Map.add lastBase newStructInfo typeInfo.StructTypes
      { typeInfo with StructTypes = newStructTypes }
    | (baseLoc, offset) :: tailPosition ->
      let structInfo =
        match Map.tryFind baseLoc typeInfo.StructTypes with
        | None -> StructInfo.empty
        | Some sInfo -> sInfo
      let fieldInfo =
        match Map.tryFind offset structInfo.FieldTypes with
        | None -> TypeInfo.empty
        | Some tInfo -> tInfo
      let newFieldInfo = addSizeAux tailPosition lastBase size fieldInfo
      let newFieldTypes = Map.add offset newFieldInfo structInfo.FieldTypes
      let newStructInfo = { structInfo with FieldTypes = newFieldTypes }
      let newStructTypes = Map.add baseLoc newStructInfo typeInfo.StructTypes
      { typeInfo with StructTypes = newStructTypes }

  let addSize syscall idx position baseLoc size (typeMap: TypeMap) =
    let callsiteMap = lookup syscall.Stub idx typeMap
    let typ = CallsiteMap.lookup syscall.Callsite callsiteMap
    let newTyp = addSizeAux position baseLoc size typ
    let newCallsiteMap = CallsiteMap.update syscall.Callsite newTyp callsiteMap
    update syscall.Stub idx newCallsiteMap typeMap
