#include <ntifs.h>
#include "..\inc\LogInput.h"

#define CanBePtr(v) (0x10000 < v && v < (MmUserProbeAddress - 0x10000))

void LogInVal(Log* log, ULONG argIdx, ULONG arg, ArgType* typ) {

  if (typ == NULL) {
    DbgPrint("[ERROR] Invalid input to LogInVal\n");
    return;
  }

  switch (typ->typeKind) {
  case FUNCPTR_TYPE:
  case HANDLE_TYPE:
  case PTR_TYPE:
    LogArg(log, argIdx, arg, typ);
    break;

  case SCALAR_TYPE:
    if (typ->width == 1) {
      LogArg(log, argIdx, (BYTE)arg, typ);
    }
    else if (typ->width == 2) {
      LogArg(log, argIdx, (WORD)arg, typ);
    } else if (typ->width == 4) {
      LogArg(log, argIdx, arg, typ);
    }
    else {
      DbgPrint("[ERROR] Unexpected width in LogInVal: %d\n", typ->width);
    }
    break;

  default:
    DbgPrint("[ERROR] Unexpected type in LogInVal: %d\n", typ->typeKind);
    break;
  }

  return;
}

void LogInBuf(Log* log, ULONG base, ULONG offset, ULONG count, ULONG width) {
  if (count * width <= SMALL_BUF_MAXSIZE) {
    LogSmallBuf(log, base, offset, count, width);
  } else {
    // Note that for compact log, large buffer max count is already set to 0.
    LogLargeBuf(log, base, offset, count, width);
  }
}

// Argument 'base' is the most recently dereferenced pointer. Argument 'struc'
// is the base address of the closest enclosing struct.
void LogInMem(Log* log, ULONG base, ULONG offset, ULONG struc, ArgType* typ) {
  ULONG size, count, i, arrOffset, fldOffset, p;
  ArgType* fldTyp;

  if (typ == NULL) {
    DbgPrint("[ERROR] Invalid input to LogInMem!\n");
    return;
  }

  __try {
    switch (typ->typeKind) {
    case FUNCPTR_TYPE:
    case HANDLE_TYPE:
      LogInEntry(log, base, offset, *(PULONG)(base + offset), typ);
      break;

    case SCALAR_TYPE:
      if (typ->width == 1) {
        LogInEntry(log, base, offset, *(PBYTE)(base + offset), typ);
      }
      else if (typ->width == 2) {
        LogInEntry(log, base, offset, *(PWORD)(base + offset), typ);
      }
      else if (typ->width == 4) {
        LogInEntry(log, base, offset, *(PDWORD)(base + offset), typ);
      }
      else if (typ->width == 8) {
        // XXX. We will split 8-byte integer into two 4-byte integers.
        LogInEntry(log, base, offset, *(PDWORD)(base + offset), typ);
        LogInEntry(log, base, offset + 4, *(PDWORD)(base + offset + 4), typ);
      }
      else {
        DbgPrint("[ERROR] Unexpected width in LogInMem: %d\n", typ->width);
      }
      break;

    case STRINGW_TYPE:
      LogStrEntry(log, base, offset);
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
      LogArrSize(log, base, offset, count);

      if (typ->contentType->typeKind == SCALAR_TYPE) {
        LogInBuf(log, base, offset, count, typ->contentType->width);
      }
      else {
        for (i = 0; i < count; i++) {
          arrOffset = typ->width * i;
#ifdef DEBUG_FLAG
          DbgPrint("Element at 0x%x + 0x%x + 0x%x\n", base, offset, arrOffset);
#endif
          LogInMem(log, base, offset + arrOffset, struc, typ->contentType);
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
        LogInMem(log, base, offset + fldOffset, (base + offset), fldTyp);
      }
      break;

    case PTR_TYPE: // This means that the content at this address is pointer.
      p = *(PULONG)(base + offset);
      LogInEntry(log, base, offset, p, typ);
      if (CanBePtr(p)) {
        // Reset 'offset' argument to zero.
        LogInMem(log, p, 0, struc, typ->contentType);
      }
      break;

    default:
      DbgPrint("[ERROR] Unexpected type in LogInMem: %d\n", typ->typeKind);
      break;
    }
  }
  __except (EXCEPTION_EXECUTE_HANDLER) {
#ifdef DEBUG_FLAG
    DbgPrint("Exception during input logging: 0x%x + 0x%x\n", base, offset);
#endif
  }
}

ULONG LogInputArg(Log* log, SyscallType* sysType, ULONG argIdx, ULONG arg) {
  ArgType* typ;

  if (log == NULL || sysType == NULL || sysType->argNum <= argIdx) {
    DbgPrint("[ERROR] Invalid input to LogInputArg!\n");
    return arg;
  }

  typ = sysType->argTypes[argIdx];

  // Mutate argument value first, to correctly log mutated pointer content.
  LogInVal(log, argIdx, arg, typ);

  if (typ->typeKind == PTR_TYPE && CanBePtr(arg)) {
    LogInMem(log, arg, 0, 0, typ->contentType);
  }

  return arg;
}
