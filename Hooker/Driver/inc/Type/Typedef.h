#pragma once

#include <ntddk.h>

typedef ULONGLONG QWORD, *PQWORD;
typedef ULONG DWORD, *PDWORD;
typedef USHORT WORD, *PWORD;
typedef UCHAR BYTE, *PBYTE;
typedef QWORD Base;
typedef QWORD Offset;

enum LOG_KIND {
  COMPACT_LOG, // Log compact information.
  DETAIL_LOG,  // Log detailed information.
  HANDLE_LOG,  // Log detailed information, regardless of trigger.
  OUTPUT_LOG,  // Log more output information.
};

enum COUNT_KIND { BYTE_COUNT, ELEM_COUNT };

enum SIZE_KIND {
  UNKNOWN_SIZE,
  FIXED_SIZE,
  ARG_FIELD_SIZE,
  ADJACENT_FIELD_SIZE,
  ADD,
  SUBTRACT,
  MULT,
  DIV,
};

enum INOUT_KIND { IN_ARG, OUT_ARG, INOUT_ARG };

enum TYPE_KIND {
  FUNCPTR_TYPE,
  HANDLE_TYPE,
  SCALAR_TYPE,
  STRINGW_TYPE,
  ARRAY_TYPE,
  STRUCT_TYPE,
  PTR_TYPE,
  UNKNOWN_TYPE,
};
