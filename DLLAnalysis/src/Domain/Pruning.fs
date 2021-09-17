module DLLAnalysis.AbsDom.Pruning

open DLLAnalysis
open DLLAnalysis.AbsDom.Signature
open DLLAnalysis.AbsDom.Register
open DLLAnalysis.AbsDom.AbsLoc

type Var =
  | R of Register
  | L of AbsLoc

module Var =
  let makeReg reg = R reg
  let makeLoc loc = L loc
  let toString = function
    | R r -> Register.toString r
    | L l -> AbsLoc.toString l

type VarSet = Set<Var>

module VarSet =
  let empty: VarSet = Set.empty

  let toString (vars: VarSet) =
    Set.map Var.toString vars
    |> String.concat ", "
    |> sprintf "{ %s }"

/// Represent conditional semantic spanwed by comparison operation.
type PruningCond =
  /// Represents the result of 'r0 & r0'.
  | And of VarSet
  /// Represents the result of 'r0 & 0xffff...'.
  | AndWithMask of VarSet
  /// Represents the result of '(r0 & r0) == 0'.
  | EqZero of VarSet
  /// Represents the result of '(r0 & 0xffff...) == 0'.
  | MaskedEqZero of VarSet

type PruningCondModule() =

  member __.toString = function
    | And vars -> sprintf "And(%s)" (VarSet.toString vars)
    | AndWithMask vars -> sprintf "AndWithMask(%s)" (VarSet.toString vars)
    | EqZero vars -> sprintf "EqZero(%s)" (VarSet.toString vars)
    | MaskedEqZero vars -> sprintf "MaskedEqZero(%s)" (VarSet.toString vars)

  member __.emptyVars = VarSet.empty

  member __.makeAnd regs locs vars =
    let regVars = Set.map Var.makeReg regs
    let locVars = Set.map Var.makeLoc locs
    And (Set.unionMany [regVars; locVars; vars])

  member __.makeMaskedAnd regs locs vars =
    let regVars = Set.map Var.makeReg regs
    let locVars = Set.map Var.makeLoc locs
    AndWithMask (Set.unionMany [regVars; locVars; vars])

  member __.tryMakeEqZero pruningCond =
    match pruningCond with
    | And vars -> Some (EqZero vars)
    | AndWithMask vars -> Some (MaskedEqZero vars)
    | EqZero _ | MaskedEqZero _ -> None

  member __.getRegs vars =
    let folder acc = function R r -> Set.add r acc | L _ -> acc
    Set.fold folder Set.empty vars

  member __.getLocs vars =
    let folder acc = function L l -> Set.add l acc | R _ -> acc
    Set.fold folder Set.empty vars

// Now we can use 'PruningElem' like a module.
let PruningCond = PruningCondModule()

/// Represents pruning condition stored in each register.
type PruningMap =
  | Bot // Note that empty map cannot be bottom for this domain.
  | CondMap of Map<Register,PruningCond>

type PruningMapModule () =
  inherit AbstractDomain<PruningMap>()

  member __.empty = CondMap Map.empty

  override __.bot = Bot

  override __.isBot x =
    match x with
    | Bot -> true
    | CondMap _ -> false

  override __.toString x =
    match x with
    | Bot -> "(bot)"
    | CondMap map ->
      let folder accStr k v =
        let keyStr = Register.toString k
        let valStr = PruningCond.toString v
        accStr + "\n" + (sprintf "%s -> %s" keyStr valStr)
      Map.fold folder "" map

  override __.join x y =
    match x, y with
    | Bot, _ -> y
    | _, Bot -> x
    | CondMap mx, CondMap my ->
      // Caution, we should leave pruning elements present in both maps, only.
      let folder accMap r px =
        match Map.tryFind r my with
        | None -> accMap
        | Some py -> if px = py then Map.add r px accMap else accMap
      CondMap (Map.fold folder Map.empty mx)

  override __.leq x y =
    match x, y with
    | Bot, _ -> true
    | _, Bot -> false
    | CondMap mx, CondMap my ->
      // Caution, all 'y' should have LESS or equal pruning elements than 'x'.
      Map.forall
        (fun k py ->
          match Map.tryFind k mx with
          | None -> true
          | Some px -> px = py
        ) my

  member __.add reg condOpt x =
    // Caution: should clear register if condition is unknown.
    match x, condOpt with
    | Bot, _ -> Bot
    | CondMap m, None -> CondMap (Map.remove reg m)
    | CondMap m, Some cond -> CondMap (Map.add reg cond m)

  member __.tryFind reg x =
    match x with
    | Bot -> None
    | CondMap m -> Map.tryFind reg m

// Now we can use 'PruningMap' like a module.
let PruningMap = PruningMapModule ()
