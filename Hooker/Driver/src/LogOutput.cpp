#include <ntifs.h>
#include "..\inc\LogOutput.h"


#define CanBePtr(v) (0x10000 < v && v < (MmUserProbeAddress - 0x10000))

// Argument 'base' is the most recently dereferenced pointer.
void LogOutMem(Log* log, ULONG base, ULONG offset, ArgType* typ) {
  ULONG i, fldOffset, p, h;
  ArgType* fldTyp;

  if (typ == NULL) {
    DbgPrint("[ERROR] Invalid input to LogOutMem\n");
    return;
  }

  __try {
    switch (typ->typeKind) {
      // We will only log HANDLE output for now.
    case HANDLE_TYPE:
      h = *(PULONG)(base + offset);
      if (h) {
        LogOutEntry(log, base, offset, h, typ);
      }
      break;

    case FUNCPTR_TYPE:
    case SCALAR_TYPE:
    case STRINGW_TYPE:
    case ARRAY_TYPE:
      break;

    case STRUCT_TYPE:
      for (i = 0; i < typ->fieldCount; i++) {
        fldOffset = typ->fieldOffsets[i];
        fldTyp = typ->fieldTypes[i];
#ifdef DEBUG_FLAG
        DbgPrint("Field at 0x%x + 0x%x + 0x%x\n", base, offset, fldOffset);
#endif
        LogOutMem(log, base, offset + fldOffset, fldTyp);
      }
      break;

    case PTR_TYPE:
      // Caution. This case means that the content at this address is pointer.
      p = *(PULONG)(base + offset);
      if (typ->inOut != IN_ARG && CanBePtr(p)) {
        LogOutMem(log, p, 0, typ->contentType);
      }
      break;

    default:
      DbgPrint("[ERROR] Unexpected type in LogOutMem: %d\n", typ->typeKind);
      break;
    }
  }
  __except (EXCEPTION_EXECUTE_HANDLER) {
#ifdef DEBUG_FLAG
    DbgPrint("Exception during output logging: 0x%x + 0x%x\n", base, offset);
#endif
  }
}

void LogOutputArg(Log* log, SyscallType* sysType, ULONG argIdx, ULONG arg) {
  ArgType* typ;

  if (log == NULL || sysType == NULL || sysType->argNum <= argIdx) {
    DbgPrint("[ERROR] Invalid input to LogOutputArg\n");
    return;
  }

  typ = sysType->argTypes[argIdx];

  if (typ->typeKind == PTR_TYPE && typ->inOut != IN_ARG && CanBePtr(arg)) {
    LogOutMem(log, arg, 0, typ->contentType);
  }

  return;
}
