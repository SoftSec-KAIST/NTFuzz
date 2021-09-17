module Binatomy.AbsDom.TypeConstr

open Binatomy
open Binatomy.AbsDom.Signature
open Binatomy.AbsDom.Functor
open WinHeader

type TypeConstr =
  | ConcrTyp of ArgType
  | SymTyp of Symbol

type TypeConstrModule () =
  inherit Elem<TypeConstr>()

  override __.toString typ =
    match typ with
    | ConcrTyp argTyp -> ArgType.toJson true "" argTyp
    | SymTyp sym -> Symbol.toString sym

  member __.ofType argTyp : TypeConstr =
    ConcrTyp argTyp

  member __.ofSymbol sym : TypeConstr =
    SymTyp sym

  member __.isConcrete typ =
    match typ with
    | ConcrTyp _ -> true
    | SymTyp _ -> false

  member __.isSymbolic typ =
    match typ with
    | ConcrTyp _ -> false
    | SymTyp _ -> true

  member __.isPtrConstr typ =
    match typ with
    | ConcrTyp (Ptr _) -> true
    | ConcrTyp (VoidPtr _) -> true
    | ConcrTyp _ -> false
    | SymTyp _ -> false

  member __.getSymbol typ : Symbol =
    match typ with
    | ConcrTyp _ -> failwith "TypeConstr.getSymbol() called with concrete type"
    | SymTyp sym -> sym

  member __.getConcrType typ : ArgType =
    match typ with
    | ConcrTyp argTyp -> argTyp
    | SymTyp _ -> failwith "TypeConstr.getConcrType() called with symbolic type"

 // Now we can use 'StringData' like a module.
let TypeConstr = TypeConstrModule ()

type TypeConstrSet = Set<TypeConstr>

type TypeConstrSetModule () =
  inherit SetDomain<TypeConstr>(TypeConstr)

  member __.resolveSymbol typ symMap =
    match typ with
    | ConcrTyp _-> Set.singleton typ
    | SymTyp sym ->
      if not (Map.containsKey sym symMap) then
        printfn "%s is unbound" sym
        raise (UnboundSymbolException sym)
      Map.find sym symMap

  /// Symbol substitution function.
  member __.substitute typs symMap =
    Set.fold (fun accSet typ ->
      __.join (__.resolveSymbol typ symMap) accSet
    ) Set.empty typs

  /// Collect symbols used in 'taintSet'.
  member __.collectSymbols typs =
    let folder accSet = function
      | ConcrTyp _ -> accSet
      | SymTyp sym -> Set.add sym accSet
    Set.fold folder Set.empty typs

// Now we can use 'StringDataSet' like a module.
let TypeConstrSet = TypeConstrSetModule()
