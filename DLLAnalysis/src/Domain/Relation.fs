module DLLAnalysis.AbsDom.Relation

open DLLAnalysis
open DLLAnalysis.AbsDom.Signature
open DLLAnalysis.AbsDom.Register
open DLLAnalysis.AbsDom.AbsLoc

type Variable =
  | NopVar
  | NoneVar
  | RegVar of Register
  | LocVar of AbsLoc

type VariableModule() =

  member __.NopVar = NopVar

  member __.NoneVar = NoneVar

  member __.RegVar reg = RegVar reg

  member __.LocVar loc = LocVar loc

  member __.toString = function
    | NopVar -> "(Nop)"
    | NoneVar -> "(None)"
    | RegVar reg -> Register.toString reg
    | LocVar loc -> AbsLoc.toString loc

// Now we can use 'Variable' like a module.
let Variable = VariableModule()

/// Must-equal analysis domain. We use equal-to set representation. A key-value
/// pair (v, V) in the map means that v must be equal to all the variables in V.
type EQRelation =
  | Bot
  | EqMap of Map<Variable,Set<Variable>>

type EQRelationModule () =
  inherit AbstractDomain<EQRelation>()

  member __.empty = EqMap Map.empty

  override __.bot = Bot

  override __.isBot x =
    match x with
    | Bot -> true
    | EqMap _ -> false // Caution: empty map means top (no constraint on state).

  override __.leq x y =
    match x, y with
    | Bot, _ -> true
    | _, Bot -> false
    | EqMap mx, EqMap my ->
      // Caution, all 'y' should have LESS or equal pruning elements than 'x'.
      Map.forall
        (fun k vy ->
          match Map.tryFind k mx with
          | None -> true
          | Some vx -> Set.isSubset vy vx
        ) my

  override __.join x y =
    match x, y with
    | Bot, _ -> y
    | _, Bot -> x
    | EqMap mx, EqMap my ->
      // Caution, we should leave pruning elements present in both maps, only.
      let folder accMap r vx =
        match Map.tryFind r my with
        | None -> accMap
        | Some vy -> Map.add r (Set.intersect vx vy) accMap
      EqMap (Map.fold folder Map.empty mx)

  override __.toString x =
    match x with
    | Bot -> "(bot)"
    | EqMap map ->
      let folder accStrs v vars =
        let vStr = Variable.toString v
        let varStrs = List.map Variable.toString (Set.toList vars)
        let eqStr = String.concat ", " varStrs
        sprintf "%s is equal to { %s }" vStr eqStr :: accStrs
      let strs = Map.fold folder [] map
      String.concat "\n" strs

  member __.debugPrint (varStr: string) x =
    match x with
    | Bot -> printfn "Bottom equality relation"
    | EqMap map ->
      let iterator v vars =
        let vStr = Variable.toString v
        if vStr.IndexOf varStr <> -1 then
          let eqStr = Set.map Variable.toString vars |> String.concat ", "
          printfn "%s is equal to { %s }" vStr eqStr
      Map.iter iterator map

  member private __.getEqualsAux v eqMap =
    // Do not have to retrieve equal set transitively.
    match Map.tryFind v eqMap with
    | None -> Set.empty
    | Some eqVars -> eqVars

  member __.getRegsEqualToReg reg x : Set<Register> =
    match x with
    | Bot -> Set.empty
    | EqMap map ->
      let varToReg v = match v with | RegVar r -> Some r | _ -> None
      Set.choose varToReg (__.getEqualsAux (RegVar reg) map)

  member __.getRegsEqualToLoc loc x : Set<Register> =
    match x with
    | Bot -> Set.empty
    | EqMap map ->
      let varToReg v = match v with | RegVar r -> Some r | _ -> None
      Set.choose varToReg (__.getEqualsAux (LocVar loc) map)

  member __.getLocsEqualToReg reg x : Set<AbsLoc> =
    match x with
    | Bot -> Set.empty
    | EqMap map ->
      let varToReg v = match v with | LocVar l -> Some l | _ -> None
      Set.choose varToReg (__.getEqualsAux (RegVar reg) map)

  member __.getLocsEqualToLoc loc x : Set<AbsLoc> =
    match x with
    | Bot -> Set.empty
    | EqMap map ->
      let varToReg v = match v with | LocVar l -> Some l | _ -> None
      Set.choose varToReg (__.getEqualsAux (LocVar loc) map)

  /// Add a relation that 'v' is equal to 'eqVars'.
  member private __.addEQRel v eqVars eqMap =
    // First, (strong-) update entry 'v' with a new equal-to variable set.
    let eqMap = Map.add v eqVars eqMap
    // Now (weak-) update each entry in 'eqVars', that they can be equal to 'v'.
    let folder accRel eqVar =
      match Map.tryFind eqVar accRel with
      | None -> Map.add eqVar (Set.singleton v) accRel
      | Some oldEqs -> Map.add eqVar (Set.add v oldEqs) accRel
    Set.fold folder eqMap eqVars

  /// Update 'v' not to be equal to any variable.
  member private __.removeEQRel v eqMap =
    Map.remove v eqMap // First, remove 'v' key from the map.
    |> Map.map (fun _ eqVars -> Set.remove v eqVars) // Now remove from vals.

  /// Update relation when 'dstReg := <srcVar>' is executed.
  member __.assignReg dstReg srcVar x : EQRelation =
    match x with
    | Bot -> Bot
    | EqMap map ->
      let dstVar = RegVar dstReg
      // 'dstVar' is equal to 'srcVar', and also the variables equal to 'srcVar'
      let eqVars = Set.add srcVar (__.getEqualsAux srcVar map)
      // 'dstVar' is no more equal to previous variable set.
      let map = __.removeEQRel dstVar map
      // Update relation to make 'dstVar' and 'eqVars' equal.
      EqMap (__.addEQRel dstVar eqVars map)

  /// Update relation when '[dstLoc] := <srcVar>' is executed.
  member __.updateLoc dstLoc srcVar x : EQRelation =
    match x with
    | Bot -> Bot
    | EqMap map ->
      let dstVar = LocVar dstLoc
      // 'dstVar' is equal to 'srcVar', and also the variables equal to 'srcVar'
      let eqVars = Set.add srcVar (__.getEqualsAux srcVar map)
      // 'dstVar' is no more equal to previous variable set.
      let map = __.removeEQRel dstVar map
      // Update relation to make 'dstVar' and 'eqVars' equal.
      EqMap (__.addEQRel dstVar eqVars map)

  /// Remove all the relation that 'reg' had with other variables.
  member __.cleanUpReg reg x : EQRelation =
    match x with
    | Bot -> Bot
    | EqMap map -> EqMap (__.removeEQRel (RegVar reg) map)

  /// Remove all the relation that 'loc' had with other variables.
  member __.cleanUpLocs locs x : EQRelation =
    match x with
    | Bot -> Bot
    | EqMap map ->
      // 'locs' set can be large, so do not call __.removeEQRel() with it.
      let isVarToRemove = function
        | NopVar | NoneVar | RegVar _ -> false
        | LocVar l -> Set.contains l locs
      Map.filter (fun k _ -> not (isVarToRemove k)) map
      |> Map.map (fun _ eqVars -> Set.filter (not << isVarToRemove) eqVars)
      |> EqMap

// Now we can use 'EQRelation' like a module.
let EQRelation = EQRelationModule ()
