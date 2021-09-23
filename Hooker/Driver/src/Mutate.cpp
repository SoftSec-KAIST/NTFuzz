#include <ntifs.h>
#include "..\inc\Mutate.h"

#pragma warning(disable: 4100)

#define RATIO_UNIT 1000000
#define SPECIAL_VALUE_COUNT 5
#define MAX_STR_COPY_LEN 0x400 // This is irrelevant to MAX_STR_LEN in Log.h
#define MAX_STR_EXTEND 0x40

#define CanBePtr(v) (0x10000 < v && v < (MmUserProbeAddress - 0x10000))
#define ShouldMutate() ((Rand() % RATIO_UNIT) < mutateRatio)

void DumpUnicodeString(ULONG ptr) {
  PWCHAR str;
  ULONG i;
  if (!CanBePtr(ptr)) {
    DbgPrint("[ERROR] Invalid range pointer not filtered: %x\n", ptr);
    return;
  }

  str = (PWCHAR)ptr;
  for (i = 0; i < 256 && str[i]; i++) {
    DbgPrint("%02x ", str[i]);
  }
  DbgPrint("(End of string)\n");
}

ULONG BitFlip(ULONG input, int width) {
  int position = Rand() % width;
  return (input ^ (1ull << position));
}

ULONG ArithMutate(ULONG input, ULONG bound) {
  ULONG delta = Rand() % (bound - 1) + 1;
  if (Rand() % 2) {
    return (input + delta);
  }
  else {
    return (input - delta);
  }
}

BYTE MutateByteInternal(BYTE arg) {
  BYTE specials[SPECIAL_VALUE_COUNT] = { 0x0, 0x7f, 0x80, 0x81, 0xff };
  BYTE newArg;
  ULONG dice;

  dice = Rand() % 4;
  if (dice == 0) {
    newArg = (BYTE)BitFlip(arg, 8);
  }
  else if (dice == 1) {
    newArg = (BYTE)ArithMutate(arg, 0xf);
  }
  else if (dice == 2) {
    dice = Rand() % SPECIAL_VALUE_COUNT;
    newArg = specials[dice];
  }
  else {
    newArg = (BYTE) (Rand() & 0x10);
  }
#ifdef DEBUG_FLAG
  DbgPrint("(Old byte) 0x%x\n", arg);
  DbgPrint("(New byte) 0x%x\n", newArg);
#endif
  return newArg;
}

BYTE MutateByteVal(Log * log, ULONG argIdx, BYTE arg) {
  BYTE mutated;
  if (ShouldMutate()) {
    mutated = MutateByteInternal(arg);
#ifdef LOGGING_FLAG
    LogArgMutation(log, argIdx, arg, mutated);
#endif
    return mutated;
  }
  else {
    return arg;
  }
}

void MutateByteMem(Log * log, ULONG base, ULONG offset) {
  BYTE orig, mutated;
  if (ShouldMutate()) {
#ifdef DEBUG_FLAG
    DbgPrint("Mutate byte at memory 0x%x + 0x%x\n", base, offset);
#endif
    orig = *(PBYTE)(base + offset);
    mutated = MutateByteInternal(orig);
#ifdef LOGGING_FLAG
    LogMemMutation(log, base, offset, orig, mutated);
#endif
    *(PBYTE)(base + offset) = mutated;
  }
}

WORD MutateWordInternal(WORD arg) {
  WORD specials[SPECIAL_VALUE_COUNT] = { 0x0, 0x7fff, 0x8000, 0x8001, 0xffff };
  WORD newArg;
  ULONG dice;

  dice = Rand() % 4;
  if (dice == 0) {
    newArg = (WORD)BitFlip(arg, 16);
  }
  else if (dice == 1) {
    newArg = (WORD)ArithMutate(arg, 0x100);
  }
  else if (dice == 2) {
    dice = Rand() % SPECIAL_VALUE_COUNT;
    newArg = specials[dice];
  }
  else {
    newArg = (WORD)(Rand() & 0x100);
  }
#ifdef DEBUG_FLAG
  DbgPrint("(Old word) 0x%x\n", arg);
  DbgPrint("(New word) 0x%x\n", newArg);
#endif
  return newArg;
}

WORD MutateWordVal(Log* log, ULONG argIdx, WORD arg) {
  WORD mutated;
  if (ShouldMutate()) {
    mutated = MutateWordInternal(arg);
#ifdef LOGGING_FLAG
    LogArgMutation(log, argIdx, arg, mutated);
#endif
    return mutated;
  }
  else {
    return arg;
  }
}

void MutateWordMem(Log* log, ULONG base, ULONG offset) {
  WORD orig, mutated;
  if (ShouldMutate()) {
#ifdef DEBUG_FLAG
    DbgPrint("Mutate word at memory 0x%x + 0x%x\n", base, offset);
#endif
    orig = *(PWORD)(base + offset);
    mutated = MutateWordInternal(orig);
#ifdef LOGGING_FLAG
    LogMemMutation(log, base, offset, orig, mutated);
#endif
    *(PWORD)(base + offset) = mutated;
  }
}

DWORD MutateDwordInternal(DWORD arg) {
  DWORD specials[SPECIAL_VALUE_COUNT] = {
    0x0, 0x7fffffff, 0x80000000, 0x80000001, 0xffffffff
  };
  DWORD newArg;
  ULONG dice;

  dice = Rand() % 4;
  if (dice == 0) {
    newArg = (DWORD)BitFlip(arg, 32);
  }
  else if (dice == 1) {
    newArg = (DWORD)ArithMutate(arg, 0x10000);
  }
  else if (dice == 2) {
    dice = Rand() % SPECIAL_VALUE_COUNT;
    newArg = specials[dice];
  }
  else {
    newArg = (DWORD)(Rand() & 0xffffffff);
  }
#ifdef DEBUG_FLAG
  DbgPrint("(Old dword) 0x%x\n", arg);
  DbgPrint("(New dword) 0x%x\n", newArg);
#endif
  return newArg;
}

DWORD MutateDwordVal(Log* log, ULONG argIdx, DWORD arg) {
  DWORD mutated;
  if (ShouldMutate()) {
    mutated = MutateDwordInternal(arg);
#ifdef LOGGING_FLAG
    LogArgMutation(log, argIdx, arg, mutated);
#endif
    return mutated;
  }
  else {
    return arg;
  }
}

void MutateDwordMem(Log* log, ULONG base, ULONG offset) {
  DWORD orig, mutated;
  if (ShouldMutate()) {
#ifdef DEBUG_FLAG
    DbgPrint("Mutate dword at memory 0x%x + 0x%x\n", base, offset);
#endif
    orig = *(PDWORD)(base + offset);
    mutated = MutateDwordInternal(orig);
#ifdef LOGGING_FLAG
    LogMemMutation(log, base, offset, orig, mutated);
#endif
    *(PDWORD)(base + offset) = mutated;
  }
}

QWORD MutateQwordInternal(QWORD arg) {
  QWORD specials[SPECIAL_VALUE_COUNT] = {
    0x0,
    0x7fffffffffffffff,
    0x8000000000000000,
    0x8000000000000001,
    0xffffffffffffffff
  };
  DWORD highDword = (DWORD)(arg >> 32);
  DWORD lowDword = (DWORD)(arg & 0xffffffff);
  QWORD newArg;
  ULONG dice;

  dice = Rand() % 4;
  if (dice == 0) {
    highDword = (DWORD)BitFlip(highDword, 32);
    lowDword = (DWORD)BitFlip(lowDword, 32);
  }
  else if (dice == 1) {
    lowDword = (DWORD)ArithMutate(lowDword, 0x10000);
  }
  else if (dice == 2) {
    dice = Rand() % SPECIAL_VALUE_COUNT;
    highDword = (DWORD)(specials[dice] >> 32);
    lowDword = (DWORD)(specials[dice] & 0xffffffff);
  }
  else {
    highDword = Rand();
    lowDword = Rand();
  }
  newArg = ((QWORD)highDword << 32) + (QWORD)lowDword;
#ifdef DEBUG_FLAG
  DbgPrint("(Old qword) 0x%llx\n", arg);
  DbgPrint("(New qword) 0x%llx\n", newArg);
#endif
  return newArg;
}

void MutateQwordMem(Log* log, ULONG base, ULONG offset) {
  QWORD orig, mutated;
  if (ShouldMutate()) {
#ifdef DEBUG_FLAG
    DbgPrint("Mutate qword at memory 0x%x + 0x%x\n", base, offset);
#endif
    orig = *(PQWORD)(base + offset);
    mutated = MutateQwordInternal(orig);
#ifdef LOGGING_FLAG
    LogMemMutation(log, base, offset, (DWORD)orig, (DWORD)mutated);
    LogMemMutation(log, base, offset + 4,
                   (DWORD)(orig >> 32), (DWORD)(mutated >> 32));
#endif
    *(PQWORD)(base + offset) = mutated;
  }
}

ULONG MutatePtrInternal(ULONG arg) {
  ULONG specials[SPECIAL_VALUE_COUNT] = {
    0x0, 0x7fff0000, 0x7fffffff, 0xffff0000, 0xffffffff
  };
  ULONG_PTR newArg;
  ULONG dice;

  dice = Rand() % 4;
  if (dice == 0) {
    newArg = BitFlip(arg, 64);
  }
  else if (dice == 1) {
    newArg = ArithMutate(arg, 0x1000); // Page size.
  }
  else if (dice == 2) {
    dice = Rand() % SPECIAL_VALUE_COUNT;
    newArg = specials[dice];
  }
  else {
    newArg = (ULONG)Rand();
  }
#ifdef DEBUG_FLAG
  DbgPrint("(Old ptr) 0x%x\n", arg);
  DbgPrint("(New ptr) 0x%x\n", newArg);
#endif
  return newArg;
}

ULONG MutatePtrVal(ULONG arg) {
  ULONG mutated;
  if (ShouldMutate()) {
    mutated = MutatePtrInternal(arg);
    return mutated;
  }
  else {
    return arg;
  }
}

void MutatePtrMem(ULONG base, ULONG offset) {
  ULONG mutated;
  if (ShouldMutate()) {
#ifdef DEBUG_FLAG
    DbgPrint("Mutate pointer at memory 0x%x + 0x%x\n", base, offset);
#endif
    mutated = MutatePtrInternal(*(PULONG)(base + offset));
    *(PULONG)(base + offset) = mutated;
  }
}

void MutateStringMem(ULONG ptr) {
  SIZE_T idx, len;
  WCHAR chr;
  ULONG dice;

  if (ShouldMutate()) {
#ifdef DEBUG_FLAG
    DbgPrint("Mutate string at memory 0x%x\n", ptr);
    DbgPrint("(Old str)\n");
    DumpUnicodeString(ptr);
#endif
    len = wcslen((PWCHAR)ptr);
    dice = Rand() % 2;
    if (dice == 0 && len > 0) { // Mutate a random character.
      idx = Rand() % len;
      chr = (WCHAR)(Rand() & 0x5e) + 0x20; // 0x20-0x7e (printable)
      ((PWCHAR)ptr)[idx] = chr;
    }
    else if (len > 0) { // Truncate.
      idx = Rand() % len;
      ((PWCHAR)ptr)[idx] = 0;
    }
#ifdef DEBUG_FLAG
    DbgPrint("(New str)\n");
    DumpUnicodeString(ptr);
#endif
  }
}

ULONG CopyAndMutateString(ULONG ptr) {
  SIZE_T idx, len, extendLen;
  WCHAR chr;
  ULONG dice;
  SIZE_T bufSize;
  PVOID buf;
  NTSTATUS status;
  ULONG newPtr = ptr;

  if (ShouldMutate()) {
#ifdef DEBUG_FLAG
    DbgPrint("Mutate string pointer value pointing to 0x%x\n", ptr);
    DbgPrint("(Old str)\n");
    DumpUnicodeString(ptr);
#endif
    // Allocate and copy to fresh new memory, to reduce exception caused while
    // mutating global (read-only) string arguments. Also, this allows us to
    // perform length-extending mutation, too.
    buf = NULL;
    len = wcslen((PWCHAR)ptr);
    if (len > MAX_STR_COPY_LEN) {
      len = MAX_STR_COPY_LEN;
    }
    bufSize = sizeof(WCHAR) * (len + MAX_STR_EXTEND + 0x4);
    status = ZwAllocateVirtualMemory(ZwCurrentProcess(), &buf, 0, &bufSize,
                                     MEM_COMMIT, PAGE_READWRITE);
    if (status != STATUS_SUCCESS) {
      return ptr;
    }
#ifdef DEBUG_FLAG
    DbgPrint("Successfully allocated a fresh memory: 0x%p\n", buf);
#endif
    memcpy(buf, (PVOID)ptr, sizeof(WCHAR) * (len + 1));
    newPtr = (ULONG)buf;
    dice = Rand() % 3;
    if (dice == 0 && len > 0) { // Mutate a random character.
      idx = Rand() % len;
      chr = (WCHAR)(Rand() & 0x5e) + 0x20; // 0x20-0x7e (printable)
      ((PWCHAR)newPtr)[idx] = chr;
    }
    else if (dice == 1 && len > 0) { // Truncate.
      idx = Rand() % len;
      ((PWCHAR)newPtr)[idx] = 0;
    }
    else {
      extendLen = Rand() % MAX_STR_EXTEND;
      chr = (WCHAR)(Rand() & 0x5e) + 0x20; // 0x20-0x7e (printable)
      for (idx = len; idx < len + extendLen; idx++) {
        ((PWCHAR)newPtr)[idx] = chr;
      }
      ((PWCHAR)newPtr)[idx] = 0;
    }
#ifdef DEBUG_FLAG
    DbgPrint("(New str)\n");
    DumpUnicodeString(newPtr);
#endif
    return newPtr;
  }
  else {
    return ptr;
  }
}

ULONG MutateVal(Log* log, ULONG argIdx, ULONG arg, ArgType* typ) {

  if (typ == NULL) {
    DbgPrint("[ERROR] Invalid input to MutateVal!\n");
    return arg;
  }

  switch (typ->typeKind) {
  case FUNCPTR_TYPE:
  case HANDLE_TYPE:
    // Do not mutate function pointer and handle for now.
    break;

  case SCALAR_TYPE:
    if (typ->width == 1) {
      arg = MutateByteVal(log, argIdx, (BYTE)arg);
    }
    else if (typ->width == 2) {
      arg = MutateWordVal(log, argIdx, (WORD)arg);
    } else if (typ->width == 4) {
      arg = MutateDwordVal(log, argIdx, arg);
    }
    else {
      DbgPrint("[ERROR] Unexpected width in MutateVal: %d\n", typ->width);
    }
    break;

  case PTR_TYPE:
    // XXX. We will just log the pointer after the mutation. Note that there is
    // no mutation logging in MutatePtrVal(). Also, note that pointer should be
    // mutated before visiting its content.
    arg = MutatePtrVal(arg);
    if (CanBePtr(arg) && typ->contentType->typeKind == STRINGW_TYPE) {
      __try {
        // XXX. We will just log the string after the mutation. Note that there
        // no mutation logging in CopyAndMutateString.
        arg = CopyAndMutateString(arg);
      }
      __except (EXCEPTION_EXECUTE_HANDLER) {
#ifdef DEBUG_FLAG
        DbgPrint("Exception string pointer value mutation: 0x%x\n", arg);
#endif
      }
    }
    break;

  default:
    DbgPrint("[ERROR] Unexpected type in MutateVal: %d\n", typ->typeKind);
    break;
  }

  return arg;
}

// Logging is performed separately, so just mutate without logging.
void MutateBufContent(Log* log, ULONG base, ULONG offset, ULONG width) {
  if (width == 1) {
    MutateByteMem(log, base, offset);
  }
  else if (width == 2) {
    MutateWordMem(log, base, offset);
  }
  else if (width == 4) {
    MutateDwordMem(log, base, offset);
  }
  else if (width == 8) {
    MutateQwordMem(log, base, offset);
  }
  else {
    DbgPrint("[ERROR] Unexpected width in MutateBufContent: %d\n", width);
  }
}

void MutateBuf(Log* log, ULONG base, ULONG offset, ULONG count, ULONG width) {
  ULONG i, elemOffset;
  ULONG nMutate, mutIdx;

  if (count * width <= LARGE_BUF_MAXSIZE) {
    for (i = 0; i < count; i++) {
      elemOffset = i * width;
      MutateBufContent(log, base, offset + elemOffset, width);
    }
  }
  else {
    // Approximate binomial distribution.
    nMutate = (count * mutateRatio) / RATIO_UNIT;
#ifdef DEBUG_FLAG
    DbgPrint("nMutate decided to 0x%x\n", nMutate);
#endif
    for (i = 0; i < nMutate; i++) {
      mutIdx = Rand() % count;
      elemOffset = width * mutIdx;
      MutateBufContent(log, base, offset + elemOffset, width);
    }
  }
}

// Argument 'base' is the most recently dereferenced pointer. Argument 'struc'
// is the base address of the closest enclosing struct.
void MutateMem(Log* log, ULONG base, ULONG offset, ULONG struc, ArgType* typ) {
  ULONG size, count, i, arrOffset, fldOffset, p;
  ArgType* fldTyp;

  if (typ == NULL) {
    DbgPrint("[ERROR] Invalid input to MutateMem!\n");
    return;
  }

  __try {
    switch (typ->typeKind) {
    case FUNCPTR_TYPE:
    case HANDLE_TYPE:
      // Do not mutate function pointer and handle for now.
      break;

    case SCALAR_TYPE:
      if (typ->width == 1) {
        MutateByteMem(log, base, offset);
      }
      else if (typ->width == 2) {
        MutateWordMem(log, base, offset);
      }
      else if (typ->width == 4) {
        MutateDwordMem(log, base, offset);
      }
      else if (typ->width == 8) {
        MutateQwordMem(log, base, offset);
      }
      else {
        DbgPrint("[ERROR] Unexpected width in MutateMem: %d\n", typ->width);
      }
      break;

    case STRINGW_TYPE:
      MutateStringMem(base + offset);
      break;

    case ARRAY_TYPE:
      size = EvalSize(typ->arraySize, struc);
#ifdef DEBUG_FLAG
      DbgPrint("Array size evaluted into 0x%x\n", size);
#endif
      if (typ->countKind == ELEM_COUNT) {
        count = size;
      }
      else if (typ->width == -1) { // Can't decide count for unknown width.
        count = 1;
      }
      else {
        count = size / typ->width;
      }

      if (typ->contentType->typeKind == SCALAR_TYPE) {
        MutateBuf(log, base, offset, count, typ->contentType->width);
      }
      else {
        for (i = 0; i < count; i++) {
          arrOffset = typ->width * i;
#ifdef DEBUG_FLAG
          DbgPrint("Element at 0x%x + 0x%x + 0x%x\n", base, offset, arrOffset);
#endif
          MutateMem(log, base, offset + arrOffset, struc, typ->contentType);
        }
      }
      break;

    case STRUCT_TYPE:
      for (i = 0; i < typ->fieldCount; i++) {
        fldOffset = typ->fieldOffsets[i];
        fldTyp = typ->fieldTypes[i];
#ifdef DEBUG_FLAG
        DbgPrint("Field at 0x%x + 0x%x + 0x%x\n", base, offset, fldOffset);
#endif
        // From now, (base + offset) is the closest enclosing struct base.
        MutateMem(log, base, offset + fldOffset, (base + offset), fldTyp);
      }
      break;

    case PTR_TYPE: // This means that the content at this address is pointer.
      // XXX. We will just log the pointer after the mutation. Note that there
      // is no logging logic in MutatePtrMem(). Also, note that pointer should
      // be mutated before visiting its content.
      MutatePtrMem(base, offset);
      p = *(PULONG)(base + offset);
      if (CanBePtr(p)) {
        if (typ->contentType->typeKind == STRINGW_TYPE) {
          // XXX. We will just log the string after the mutation. Note that
          // there is no logging logic in CopyAndMutateString().
          *(PULONG)(base + offset) = CopyAndMutateString(p);
        }
        else {
          // Reset 'offset' argument to zero.
          MutateMem(log, p, 0, struc, typ->contentType);
        }
      }
      break;

    default:
      DbgPrint("[ERROR] Unexpected type in MutateMem: %d\n", typ->typeKind);
      break;
    }
  }
  __except (EXCEPTION_EXECUTE_HANDLER) {
#ifdef DEBUG_FLAG
    DbgPrint("Exception during memory mutation: 0x%x + 0x%x\n", base, offset);
#endif
  }
}

ULONG MutateArg(Log* log, SyscallType* sysType, ULONG argIdx, ULONG arg) {
  ArgType* typ;

  if (log == NULL || sysType == NULL || sysType->argNum <= argIdx) {
    DbgPrint("[ERROR] Invalid input to MutateArg!\n");
    return arg;
  }

  typ = sysType->argTypes[argIdx];

  // Mutate argument value first, to correctly log mutated pointer content.
  arg = MutateVal(log, argIdx, arg, typ);

  if (typ->typeKind == PTR_TYPE && CanBePtr(arg)) {
    if (typ->contentType->typeKind != STRINGW_TYPE) {
      // For string pointer, we already mutated its content in MutateVal.
      MutateMem(log, arg, 0, 0, typ->contentType);
    }
  }

  return arg;
}
