namespace DLLAnalysis

/// Native integer type. x86.
type NInt = int32
type NUInt = uint32

module NInt =
  let ZERO: NInt = 0l
  let ONE: NInt = 1l
  let TWO: NInt = 2l
  let WORD_SIZE: NInt = 4l // x86.
  let BYTE_WIDTH: NInt = 8l
  let WORD_WIDTH: NInt = BYTE_WIDTH * WORD_SIZE

module NUInt =
  let ZERO: NUInt = 0ul
  let ONE: NUInt = 1ul
  let TWO: NUInt = 2ul
  let WORD_SIZE: NUInt = 4ul // x86.
  let BYTE_WIDTH: NUInt = 8ul
  let WORD_WIDTH: NUInt = BYTE_WIDTH * WORD_SIZE
  let ARCH_MAX_INT: NUInt = 0x7FFFFFFFul // x86.
  let ARCH_MAX_UINT: NUInt = 0xFFFFFFFFul // x86.
  let MAX_LIKELY_INT: NUInt = 0x7FFF0000ul // x86
  let SHL_LIMIT: NUInt = 16ul
  let VALID_WIDTHS: NUInt list = [8ul; 16ul; 32ul; 64ul]

  let ofInt (i: int) : NUInt = uint32 i

  let toInt (x: NUInt): int = int x

  let ofNInt (i: NInt): NUInt = uint32 i

  let toNInt (x: NUInt): NInt = int32 x

  let ofUInt64 (ui64: uint64): NUInt =
    if ui64 > uint64 ARCH_MAX_UINT then failwithf "Invalid range: %x" ui64
    uint32 ui64

  let toUInt64 (x: NUInt) : uint64 = uint64 x

  let add (x: NUInt) (y: NUInt) = x + y

  let sub (x: NUInt) (y: NUInt) = x - y

  let rec private powOfTwoAux e acc =
    if e = 0ul then acc else powOfTwoAux (e - 1ul) (2ul * acc)

  let powOfTwo exp = powOfTwoAux exp 1ul

  let isAndMask (ui: NUInt) = ui >= 0xffff0000ul // x86
