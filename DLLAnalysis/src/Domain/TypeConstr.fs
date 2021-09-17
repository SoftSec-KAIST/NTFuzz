module DLLAnalysis.AbsDom.TypeConstr

open DLLAnalysis
open DLLAnalysis.Utils
open DLLAnalysis.AbsDom.Signature
open DLLAnalysis.AbsDom.Functor
open DLLAnalysis.AbsDom.FlatInt
open WinHeader

type ArgTypeDom =
  | ConcrTyp of ArgType
  | SymTyp of Symbol

type ArgTypeDomModule () =
  inherit Elem<ArgTypeDom>()

  override __.toString typ =
    match typ with
    | ConcrTyp argTyp -> ArgType.toJson true "" argTyp
    | SymTyp sym -> Symbol.toString sym

  member __.ofArgType argTyp : ArgTypeDom =
    ConcrTyp argTyp

  member __.ofSymbol sym : ArgTypeDom =
    SymTyp sym

  member __.isConcrete typ =
    match typ with
    | ConcrTyp _ -> true
    | SymTyp _ -> false

  member __.isSymbolic typ =
    match typ with
    | ConcrTyp _ -> false
    | SymTyp _ -> true

  member __.isPtr typ =
    match typ with
    | ConcrTyp (Ptr _) -> true
    | ConcrTyp (VoidPtr _) -> true
    | ConcrTyp _ -> false
    | SymTyp _ -> false

  member __.canBeInt typ =
    match typ with
    | ConcrTyp (Scalar _) -> true
    | ConcrTyp _ -> false
    | SymTyp _ -> true

  member __.isValidWidth width typ =
    match typ with
    | SymTyp _ -> true
    | ConcrTyp argTyp ->
      try // For width 1~2, should consider the vagueness of character width.
        if width <= 2UL then ArgType.widthOf argTyp <= width
        else ArgType.widthOf argTyp = width
      with ArgType.UnknownWidthException -> true // Conservative.

  member __.getConcrType typ : ArgType =
    match typ with
    | ConcrTyp argTyp -> argTyp
    | SymTyp _ -> failwith "TypeConstr.getConcrType() called with symbolic type"

// Now we can use 'ArgTypeDom' like a module.
let ArgTypeDom = ArgTypeDomModule ()

type ArgTypeSet = Set<ArgTypeDom>

type ArgTypeSetModule () =
  inherit SetDomain<ArgTypeDom>(ArgTypeDom)

  member private __.resolveSymbol typ symMap =
    match typ with
    | ConcrTyp _-> Set.singleton typ
    | SymTyp sym ->
      if not (Map.containsKey sym symMap) then Set.empty
      else Map.find sym symMap

  /// Symbol substitution function.
  member __.substitute typs symMap =
    Set.fold (fun accSet typ ->
      __.join (__.resolveSymbol typ symMap) accSet
    ) Set.empty typs

  /// Collect symbols used in 'typs'.
  member __.collectSymbols typs =
    let folder accSet = function
      | ConcrTyp _ -> accSet
      | SymTyp sym -> Set.add sym accSet
    Set.fold folder Set.empty typs

  member __.funcPtr: ArgTypeSet =
    __.make [ArgTypeDom.ofArgType FuncPtr]

  member __.makeInt width: ArgTypeSet =
    if not (List.contains width NUInt.VALID_WIDTHS) then __.bot
    else let byteSize = width / NUInt.BYTE_WIDTH
         __.make [ArgTypeDom.ofArgType (Scalar (NUInt.toUInt64 byteSize))]

  member __.ofSymbol sym: ArgTypeSet =
    __.make [ArgTypeDom.ofSymbol sym]

  member __.ofConcrTypes argTyps: ArgTypeSet =
    let folder acc argTyp = __.add (ArgTypeDom.ofArgType argTyp) acc
    Set.fold folder __.bot argTyps

  member __.leaveInt (typs: ArgTypeSet): ArgTypeSet =
    __.filter ArgTypeDom.canBeInt typs

  member __.leaveNonPtr (typs: ArgTypeSet): ArgTypeSet =
    __.filter (not << ArgTypeDom.isPtr) typs

  member __.leaveConcrete (typs: ArgTypeSet): ArgTypeSet =
    __.filter ArgTypeDom.isConcrete typs

  member __.leaveSymbolic (typs: ArgTypeSet): ArgTypeSet =
    __.filter ArgTypeDom.isSymbolic typs

  member __.truncate width typs =
    if width = NUInt.WORD_WIDTH then typs
    else __.makeInt width

// Now we can use 'ArgTypeSet' like a module.
let ArgTypeSet = ArgTypeSetModule()

// Type information for struct size.
type SizeHint = FlatInt
let SizeHint = FlatInt

type TypeConstr = ArgTypeSet * SizeHint

type TypeConstrModule () =
  inherit Prod2Domain<ArgTypeSet,SizeHint>(ArgTypeSet,SizeHint)

  override __.toString constr =
    let argTypes = __.getArgTypes constr
    let sizeHint = __.getSizeHint constr
    if SizeHint.isBot sizeHint then ArgTypeSet.toString argTypes
    else let argTypStr = ArgTypeSet.toString argTypes
         let sizeStr = SizeHint.toString sizeHint
         sprintf "%s (size hint = %s)" argTypStr sizeStr

  member __.getArgTypes (constr: TypeConstr): ArgTypeSet =
    fst constr

  member __.getSizeHint (constr: TypeConstr): SizeHint =
    snd constr

  member __.setArgTypes argTypes (constr: TypeConstr): TypeConstr =
    (argTypes, __.getSizeHint constr)

  member __.addSizeHint size constr =
    let sizeHint = __.getSizeHint constr
    let newSizeHint = SizeHint.join sizeHint (SizeHint.ofUInt64 size)
    (__.getArgTypes constr, newSizeHint)

  member __.removeSizeHint constr =
    (__.getArgTypes constr, SizeHint.bot)

  member __.tryGetSizeHint (constr: TypeConstr): NUInt option =
    let sizeHint = __.getSizeHint constr
    if FlatInt.isConst sizeHint
    then Some (FlatInt.getConst sizeHint)
    else None

  /// Symbol substitution function.
  member __.substitute (constr: TypeConstr) symMap: TypeConstr =
    let argTypes = __.getArgTypes constr
    __.setArgTypes (ArgTypeSet.substitute argTypes symMap) constr

  /// Collect symbols used in 'typs'.
  member __.collectSymbols (constr: TypeConstr) =
    ArgTypeSet.collectSymbols (__.getArgTypes constr)

  member __.funcPtr: TypeConstr =
    (ArgTypeSet.funcPtr, SizeHint.bot)

  member __.makeInt width: TypeConstr =
    (ArgTypeSet.makeInt width, SizeHint.bot)

  member __.ofSymbol sym: TypeConstr =
    (ArgTypeSet.ofSymbol sym, SizeHint.bot)

  member __.ofConcrTypes argTyps: TypeConstr =
    (ArgTypeSet.ofConcrTypes argTyps, SizeHint.bot)

  member __.leaveInt (constr: TypeConstr): TypeConstr =
    let argTypes = __.getArgTypes constr
    __.setArgTypes (ArgTypeSet.leaveInt argTypes) constr

  member __.leaveNonPtr (constr: TypeConstr): TypeConstr =
    let argTypes = __.getArgTypes constr
    __.setArgTypes (ArgTypeSet.leaveNonPtr argTypes) constr

  member __.truncate bitWidth (constr: TypeConstr): TypeConstr =
    let argTypes = __.getArgTypes constr
    __.setArgTypes (ArgTypeSet.truncate bitWidth argTypes) constr

// Now we can use 'TypeConstr' like a module.
let TypeConstr = TypeConstrModule()
