namespace DLLAnalysis

/// A subroutine.
type Subroutine = Addr

module Subroutine =

  let binOf = Addr.getBinary

  let rawAddrOf = Addr.getRawAddr

  let fetchName (s: string) =
    if not (s.Contains("!")) then failwithf "Invalid subroutine string: %s" s
    s.[(s.IndexOf("!") + 1) .. ]

  let toString subrtn =
    match PESymbol.getNames subrtn with
    | [] -> Addr.toString subrtn
    | name :: _ -> name

  let toStringAll subrtn =
    PESymbol.getNames subrtn

  /// Fetch all the names of subroutine (without binary names).
  let toNameAll subrtn =
    List.map fetchName (toStringAll subrtn)

  let ofString (s: string) : Subroutine =
    PESymbol.getAddr s

  let private tryFindStackLiftAux (name: string) =
    if name.Contains("@") then
      let idx = name.LastIndexOf('@')
      let suffix = name.[idx + 1 ..]
      let success, delta = System.Int32.TryParse suffix
      if success then Some (Offset.ofInt delta) else None
    else None

  let tryFindStackLiftWithName subrtn =
    let names = toStringAll subrtn
    match List.choose tryFindStackLiftAux names with
    | [] -> None
    | delta :: _  -> Some delta
