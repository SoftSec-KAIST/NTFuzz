#pragma once

#include "Macro.h"
#include "ParseHelper.h"
#include "Typedef.h"

#define MAX_OFFSET_COUNT 4

typedef struct _ArraySize {
  SIZE_KIND sizeKind;
  // For FIXED_SIZE.
  ULONG fixedSize;
  // For ARGFIELD_SIZE.
  ULONG idx;
  // For ARGFIELD_SIZE and ADJACENT_FIELD_SIZE.
  ULONG offsetCount;
  ULONG offsets[MAX_OFFSET_COUNT];
  // For ADD_SIZE, SUB_SIZE, MULT_SIZE.
  _ArraySize* oprnd1;
  _ArraySize* oprnd2;
} ArraySize;

ArraySize* CreateArraySize(char * buf, PULONG pReadBytes);
void DeleteArraySize(ArraySize *);
void StoreArg(ULONG argIdx, ULONG argVal);
ULONG EvalSize(ArraySize* arrSize, ULONG strucBase);