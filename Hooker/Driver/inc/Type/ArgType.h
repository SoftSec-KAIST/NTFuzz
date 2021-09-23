#pragma once

#include "Macro.h"
#include "ParseHelper.h"
#include "ArraySize.h"

typedef struct _ArgType {
  // Common properties.
  TYPE_KIND typeKind;
  // For handle, scalar, and array.
  int width;
  // For array.
  COUNT_KIND countKind;
  ArraySize* arraySize;
  // For pointer
  INOUT_KIND inOut;
  // For pointer and array.
  _ArgType* contentType;
  // For struct.
  ULONG fieldCount;
  ULONG* fieldOffsets;
  _ArgType** fieldTypes;
} ArgType;

ArgType* CreateArgType(char* buf, PULONG pReadBytes);
void DeleteArgType(ArgType*);
