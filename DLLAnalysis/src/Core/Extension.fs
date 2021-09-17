namespace DLLAnalysis

module List =

  let add elem list = elem :: list

  let foldi f acc list =
    let folder (accN, accRes) elem = (accN + 1, f accRes accN elem)
    List.fold folder (0, acc) list |> snd

  let rec private headsAux acc = function
    | [] -> failwith "List.heads(): Empty list input"
    | [_] -> List.rev acc
    | head :: tailList -> headsAux (head :: acc) tailList

  let collecti f list =
    List.concat (List.mapi f list)

module Array =

  let add elem arr = Array.append [|elem|] arr

  let foldi f acc arr =
    let folder (accIdx, accRes) elem = (accIdx + 1, f accRes accIdx elem)
    Array.fold folder (0, acc) arr |> snd

module Set =

  let diff = Set.difference

  let choose f set =
    let folder acc elem =
      match f elem with
      | None -> acc
      | Some x -> Set.add x acc
    Set.fold folder Set.empty set

  let collect f set =
    let folder acc elem = Set.union (f elem) acc
    Set.fold folder Set.empty set

module Map =

  let foldi f acc map =
    let folder (accN, accRes) k v = (accN + 1, f accRes accN k v)
    Map.fold folder (0, acc) map |> snd

  let keys map =
    Map.fold (fun acc k _ -> k :: acc) [] map

  let vals map =
    Map.fold (fun acc _ v -> v :: acc) [] map

module Dictionary =
  open System.Collections.Generic

  let toSeq dict =
    Seq.map (fun (KeyValue(k,v)) -> (k,v)) dict

  let toList (dict: IDictionary<_,_>) =
    Seq.toList (toSeq dict)

  let toMap (dict: IDictionary<_,_>) =
    let folder accMap (KeyValue(k,v)) = Map.add k v accMap
    Seq.fold folder Map.empty dict

  let keys (dict: IDictionary<_,_>) =
    let folder accList (KeyValue(k,_)) = k :: accList
    Seq.fold folder [] dict

  let vals (dict: IDictionary<_,_>) =
    let folder accList (KeyValue(_,v)) = v :: accList
    Seq.fold folder [] dict

module BitVector =
  open B2R2

  let getBitWidth bitv =
    let regType = BitVector.getType bitv
    NUInt.ofInt (RegType.toBitWidth regType)

  let castToNUInt bitv: NUInt =
    try BitVector.toUInt32 bitv with
    | Failure _ ->
      let bigInt = BitVector.getValue bitv
      let mask = BigInteger.getMask 32
      uint32 (bigInt &&& mask)

  let castToNInt bitv: NInt =
    try BitVector.toInt32 bitv with
    | Failure _ ->
      let bigInt = BitVector.getValue bitv
      let mask = BigInteger.getMask 32
      int32 (bigInt &&& mask)

  let castToInt bitv =
    try BitVector.toInt32 bitv with
    | Failure _ ->
      let bigInt = BitVector.getValue bitv
      let mask = BigInteger.getMask 32
      int (bigInt &&& mask)

  let castToUInt64 bitv =
    try BitVector.toUInt64 bitv with
    | Failure _ ->
      let bigInt = BitVector.getValue bitv
      let mask = BigInteger.getMask 64
      uint64 (bigInt &&& mask)

/// A fast stack that can check membership in log(N) time.
type Stack<'a when 'a: comparison>= List<'a> * Set<'a>

exception EmptyStackException

module Stack =

  let empty : Stack<'a> = ([], Set.empty)

  let push x (stack: Stack<'a>) : Stack<'a> =
    let list, set = stack
    (x :: list, Set.add x set)

  let pop (stack: Stack<'a>) : 'a * Stack<'a> =
    let list, set = stack
    match list with
    | [] -> raise EmptyStackException
    | hd :: tails -> (hd, (tails, Set.remove hd set))

  let contains x (stack: Stack<'a>) =
    let _, set = stack
    Set.contains x set
