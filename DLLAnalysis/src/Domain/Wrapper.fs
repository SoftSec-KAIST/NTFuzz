module DLLAnalysis.AbstractDomain

(* It is desirable if we can use all the defined abstract domains by opening a
 * single namespace. However, this is only achievable with a separate wrapping
 * module like this. Since we use class to mimic functor, we have to spawn an
 * object that plays a role of module. As namespace cannot hold a value, we
 * cannot define each abstract within a single namespace.
 *)
type PPoint = DLLAnalysis.PPoint
let PPoint = DLLAnalysis.AbsDom.PPoint.PPoint
type FlatInt = DLLAnalysis.AbsDom.FlatInt.FlatInt
let FlatInt = DLLAnalysis.AbsDom.FlatInt.FlatInt
type AbsInt = DLLAnalysis.AbsDom.AbsInt.AbsInt
let AbsInt = DLLAnalysis.AbsDom.AbsInt.AbsInt
type AbsLoc = DLLAnalysis.AbsDom.AbsLoc.AbsLoc
let AbsLoc = DLLAnalysis.AbsDom.AbsLoc.AbsLoc
type AbsLocSet = DLLAnalysis.AbsDom.AbsLoc.AbsLocSet
let AbsLocSet = DLLAnalysis.AbsDom.AbsLoc.AbsLocSet
type Function = DLLAnalysis.AbsDom.Function.Function
let Function = DLLAnalysis.AbsDom.Function.Function
type FunctionSet = DLLAnalysis.AbsDom.Function.FunctionSet
let FunctionSet = DLLAnalysis.AbsDom.Function.FunctionSet
type ArgTypeDom = DLLAnalysis.AbsDom.TypeConstr.ArgTypeDom
let ArgTypeDom = DLLAnalysis.AbsDom.TypeConstr.ArgTypeDom
type ArgTypeSet = DLLAnalysis.AbsDom.TypeConstr.ArgTypeSet
let ArgTypeSet = DLLAnalysis.AbsDom.TypeConstr.ArgTypeSet
type TypeConstr = DLLAnalysis.AbsDom.TypeConstr.TypeConstr
let TypeConstr = DLLAnalysis.AbsDom.TypeConstr.TypeConstr
type AbsVal = DLLAnalysis.AbsDom.AbsVal.AbsVal
let AbsVal = DLLAnalysis.AbsDom.AbsVal.AbsVal
type Register = DLLAnalysis.Register
let Register = DLLAnalysis.AbsDom.Register.Register
type RegMap = DLLAnalysis.AbsDom.RegMap.RegMap
let RegMap = DLLAnalysis.AbsDom.RegMap.RegMap
type Record = DLLAnalysis.AbsDom.Record.Record
let Record = DLLAnalysis.AbsDom.Record.Record
type LocSymMap = DLLAnalysis.AbsDom.LocSymMap.LocSymMap
let LocSymMap = DLLAnalysis.AbsDom.LocSymMap.LocSymMap
type AbsMem = DLLAnalysis.AbsDom.AbsMem.AbsMem
let AbsMem = DLLAnalysis.AbsDom.AbsMem.AbsMem
type Variable = DLLAnalysis.AbsDom.Relation.Variable
let Variable = DLLAnalysis.AbsDom.Relation.Variable
type EQRelation = DLLAnalysis.AbsDom.Relation.EQRelation
let EQRelation = DLLAnalysis.AbsDom.Relation.EQRelation
type PruningCond = DLLAnalysis.AbsDom.Pruning.PruningCond
let PruningCond = DLLAnalysis.AbsDom.Pruning.PruningCond
type PruningMap = DLLAnalysis.AbsDom.Pruning.PruningMap
let PruningMap = DLLAnalysis.AbsDom.Pruning.PruningMap
type State = DLLAnalysis.AbsDom.State.State
let State = DLLAnalysis.AbsDom.State.State
type StateMap = DLLAnalysis.AbsDom.State.StateMap
let StateMap = DLLAnalysis.AbsDom.State.StateMap
