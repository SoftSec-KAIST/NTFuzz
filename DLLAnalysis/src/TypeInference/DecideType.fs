module DLLAnalysis.TypeAnalysis.DecideType

open WinHeader
open DLLAnalysis
open DLLAnalysis.Utils
open DLLAnalysis.AbstractDomain
open DLLAnalysis.TypeAnalysis.SelectMajority

// Auxiliary type for debugging message.
type TypeSource = {
  CallSite : Addr
  Position : (AbsLoc * Offset) list
}

module TypeSource =

  let make callsite =
    { CallSite = callsite; Position = [] }

  let append baseLoc offset src =
    { src with Position = src.Position @ [(baseLoc, offset)] }

  let private entryToStr (baseLoc, offset) =
    sprintf "%s + 0x%x" (AbsLoc.toString baseLoc) offset

  let toString src =
    if List.isEmpty src.Position
    then sprintf "callsite %s" (Addr.toString src.CallSite)
    else List.map entryToStr src.Position
         |> String.concat ", "
         |> sprintf "[%s]"

// Add hoc filtering. We can filter out types that can never come at top level.
let private filterForTopLevel isTopLevel typs =
  if not isTopLevel then typs
  else List.filter (not << ArgType.isStr) typs
       |> List.filter (not << ArgType.isStruct)
       |> List.filter (not << ArgType.isArr)

let rec private decideStructType src baseLoc (structInfo: StructInfo) =
  let folder accMap offset fldTypeInfo =
    let src = TypeSource.append baseLoc offset src
    let argTyp = decideArgTypeAux src false fldTypeInfo
    let offsetUI64 = Offset.toUInt64 offset
    Map.add offsetUI64 argTyp accMap
  let size = SelectSize.run structInfo.Sizes
  let fieldMap = Map.fold folder Map.empty structInfo.FieldTypes
  Ptr (Struct (size, fieldMap), IOUnknown)

and private decideArgTypeAux src isTopLevel (typeInfo: TypeInfo) =
  let constrs = typeInfo.Constrs
                |> filterForTopLevel isTopLevel
                |> List.map (fun t -> ("Constraint", t))
  let mapper (baseLoc, structInfo) =
    let msg = TypeSource.toString (TypeSource.append baseLoc Offset.ZERO src)
    (msg, decideStructType src baseLoc structInfo)
  let structTypes = List.map mapper (Map.toList typeInfo.StructTypes)
  SelectType.run (constrs @ structTypes) (TypeSource.toString src)

/// Decide the type of an argument.
let private decideArgType stub n typeMap  =
  let callsiteMap = TypeMap.lookup stub n typeMap
  let mapper (callsite, typeInfo) =
    let src = TypeSource.make callsite
    (TypeSource.toString src, decideArgTypeAux src true typeInfo)
  let argTyps = List.map mapper (Map.toList callsiteMap)
  SelectType.run argTyps "decideArgType() on each callsite"

/// Decide the type of system call.
let decideSyscallType typeMap accSpec stub =
  printfn "==== Deciding system call type of %s ====" (Stub.toString stub)
  let argNum = StubInfo.getArgNum stub
  let mapper n = decideArgType stub n typeMap |> ArgType.finalize
  let argTyps = List.map mapper (List.ofSeq { 0 .. (argNum - 1) })
  TypeSpec.add stub argTyps accSpec

let run analysis typeMap =
  let stubs = TypeMap.getStubKeys typeMap // Caution: not StubInfo.getStubs().
  let typeSpec = Set.fold (decideSyscallType typeMap) TypeSpec.empty stubs
  match analysis with
  | Dummy -> failwith "Unreachable"
  | Eval -> typeSpec
  | All -> // Caution on the order (prioritize by overwriting)
    typeSpec
    |> TypeSpec.addMissing // Fill in unanalyzed/zero-arg syscalls.
    |> TypeSpec.addHeaders // Fill in syscalls documented in Windows header.
    |> TypeSpec.addEncoded // Fill in syscalls whose semantics are encoded.
    |> TypeSpec.removeBlacklisted // Remove blacklisted (non-target) stubs.
