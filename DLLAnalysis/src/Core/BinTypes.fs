namespace DLLAnalysis

open B2R2
open B2R2.BinFile

// Fundamentals types and modules for binary.

/// A binary file.
type Binary = string

/// CPU registers.
type Register = string

exception InvalidStackPtrException

// x86.
module Register =

  let toString reg = reg

  let stackPtr = "ESP"

  let ghostStackPtr = "ESP_BACKUP"

  let stackImpreciseFlag = "ESP_RESTORED"

  let basePtr = "EBP"

  let ret = "EAX"

  let sysNum = "EAX"

  let strVal = "EAX"

  let strSrc = "ESI"

  let strDst = "EDI"

  let strCnt = "ECX"

  let pgmCounter = "EIP"

  let flags: Register list = ["CF"; "OF"; "AF"; "SF"; "ZF"; "PF"]

  let args: Register list = []

  let inputs: Register list = ["ECX"; "EDX"] @ args

  let isFlag reg = List.contains reg flags

  let ofTempVar tmpRegNum = sprintf "T_%d" tmpRegNum

/// Offset of an abstract location or memory address.
type Offset = NInt

// x86.
module Offset =

  let ZERO: Offset = NInt.ZERO
  let ONE: Offset = NInt.ONE
  let TWO: Offset = NInt.TWO
  let WORD_SIZE: Offset = NInt.WORD_SIZE

  let ofInt (i: int): Offset = int32 i

  let toInt (x: Offset): int = int x

  let ofUInt64 (ui64: uint64): Offset = int32 ui64

  let toUInt64 (x: Offset) : uint64 = uint64 x

  let ofNUInt (ui: NUInt): Offset = int32 ui

  let toNUInt (x: Offset): NUInt = uint32 x

  let ofString (s: string) : Offset = int32 s

  let toString (x: Offset) = sprintf "0x%x" x

  let isAligned (x: Offset) = x % NInt.WORD_SIZE = NInt.ZERO

  let alignDown (i: NInt) = i - (i &&& (NInt.WORD_SIZE - NInt.ONE))

  let add (x: Offset) (y: Offset): Offset = x + y

  let sub (x: Offset) (y: Offset): Offset = x - y

/// Absolute memory address itself.
type RawAddr = NUInt

module RawAddr =

  let ZERO: RawAddr = 0ul

  let ofBitVector (bv: B2R2.BitVector) =
    BitVector.castToNUInt bv

  let ofUInt64 (ui64: uint64) : RawAddr =
    NUInt.ofUInt64 ui64

  let toUInt64 (addr: RawAddr): uint64 =
    NUInt.toUInt64 addr

  let ofString (str: string): RawAddr =
    System.Convert.ToUInt32(str, 16)

  let toString (addr: RawAddr) = sprintf "0x%x" addr

  let addOffset (addr: RawAddr) (offset: Offset) : RawAddr =
    addr + (uint32 offset)

  let subOffset (addr: RawAddr) (offset: Offset) : RawAddr =
    addr - (uint32 offset)

/// A pair of binary file and raw address within that binary.
type Addr = Binary * RawAddr

module Addr =

  let makeWithUI64 bin ui64 : Addr =
    (bin, NUInt.ofUInt64 ui64)

  let makeWithRawAddr bin rawAddr: Addr =
    (bin, rawAddr)

  let getBinary (addr: Addr) : Binary = fst addr

  let getRawAddr (addr: Addr) : RawAddr = snd addr

  let toString (addr: Addr) =
    let bin = getBinary addr
    let rAddr = getRawAddr addr
    sprintf "%s!%s" bin (RawAddr.toString rAddr)

  let addOffset (addr: Addr) i =
    let (bin, rAddr) = addr
    (bin, RawAddr.addOffset rAddr i)

  let subOffset (addr: Addr) i =
    let (bin, rAddr) = addr
    (bin, RawAddr.subOffset rAddr i)

type B2R2Symbol = B2R2.BinFile.Symbol

module B2R2Symbol =

  let getName (sym: B2R2Symbol) =
    sym.Name

  let getAddr (sym: B2R2Symbol) =
    sym.Address

  let isFunctionSymbol (sym: B2R2Symbol) =
    sym.Kind = SymbolKind.FunctionType

/// Program point without binary name. Corresponds to B2R2's ProgramPoint.
type RawPPoint = RawAddr * int

module RawPPoint =

  let make rawAddr idx: RawPPoint = (rawAddr, idx)

  let getRawAddr (point: RawPPoint) =
    fst point

  let getIdx (point: RawPPoint) =
    snd point

  let toString (point: RawPPoint) : string =
    let rawAddr = getRawAddr point
    let idx = getIdx point
    sprintf "%s (%d)" (RawAddr.toString rawAddr) idx

/// Program point with binary name.
type PPoint = Addr * int

module PPoint =

  let make addr idx: PPoint = (addr, idx)

  let getAddr (ppoint: PPoint) =
    fst ppoint

  let getIdx (ppoint: PPoint) =
    snd ppoint

  let getBinary (ppoint: PPoint) : Binary =
    Addr.getBinary (getAddr ppoint)

  let getRawPPoint (ppoint: PPoint) : RawPPoint =
    let addr = getAddr ppoint
    let rawAddr = Addr.getRawAddr addr
    let idx = getIdx ppoint
    RawPPoint.make rawAddr idx

  let toString (pp: PPoint) : string =
    let addr = getAddr pp
    let idx = getIdx pp
    sprintf "%s (%d)" (Addr.toString addr) idx
