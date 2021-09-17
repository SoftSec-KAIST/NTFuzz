module DLLAnalysis.AbsDom.Function

open DLLAnalysis
open DLLAnalysis.Utils
open DLLAnalysis.AbsDom.Signature
open DLLAnalysis.AbsDom.Functor

type Function =
  | Static of Addr
  | SymCode of Symbol
  | PC of PPoint // Abuse the domain to store current program point in state.

type FunctionModule () =
  inherit Elem<Function>()

  override __.toString f =
    match f with
    | Static addr -> Addr.toString addr
    | SymCode sym -> Symbol.toString sym
    | PC ppoint -> PPoint.toString ppoint

  // Create functions.
  member __.ofAddr addr = Static addr
  member __.ofPC ppoint = PC ppoint
  member __.ofSymbol sym = SymCode sym

  // Checker functions.

  member __.isStatic = function
    | Static _ -> true
    | SymCode _ | PC _ -> false

  member __.isSymbolic = function
    | SymCode _ -> true
    | Static _ | PC _ -> false

  // Binary operations.

  member __.addOffset f i =
    match f with
    | Static addr -> Some (Static (Addr.addOffset addr i))
    | SymCode _ -> None
    | PC _ -> None

  member __.subOffset f i =
    match f with
    | Static addr -> Some (Static (Addr.subOffset addr i))
    | SymCode _ -> None
    | PC _ -> None

let Function = FunctionModule() // Now we can use 'Function' like a module.

type FunctionSet = Set<Function>
type FunctionSetModule() =
  inherit SetDomain<Function>(Function)

  member __.addOffset fset i =
    Set.toList fset
    |> List.choose (fun f -> Function.addOffset f i)
    |> Set.ofList

  member __.subOffset fset i =
    Set.toList fset
    |> List.choose (fun f -> Function.subOffset f i)
    |> Set.ofList

  member __.resolveSymbol func symMap =
    match func with
    | Static _ | PC _ -> Set.singleton func
    | SymCode sym ->
      if not (Map.containsKey sym symMap) then Set.empty
      else Map.find sym symMap

  /// Symbol substitution function.
  member __.substitute funcSet symMap =
    Set.fold (fun accSet func ->
      __.join (__.resolveSymbol func symMap) accSet
    ) Set.empty funcSet

  /// Collect symbols used in 'funcSet'.
  member __.collectSymbols funcSet =
    let folder accSet = function
      | Static _ | PC _ -> accSet
      | SymCode sym -> Set.add sym accSet
    Set.fold folder Set.empty funcSet

// Now we can use 'FunctionSet' like a module.
let FunctionSet = FunctionSetModule ()
