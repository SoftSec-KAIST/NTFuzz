#include "..\..\inc\Type\ArgType.h"

TYPE_KIND parseTypeKind(const char* s, PULONG pReadBytes) {
  if (checkMatch(s, "funcptr", pReadBytes)) {
    return FUNCPTR_TYPE;
  }
  else if (checkMatch(s, "handle", pReadBytes)) {
    return HANDLE_TYPE;
  }
  else if (checkMatch(s, "scalar", pReadBytes)) {
    return SCALAR_TYPE;
  }
  else if (checkMatch(s, "struct", pReadBytes)) {
    return STRUCT_TYPE;
  }
  else if (checkMatch(s, "array", pReadBytes)) {
    return ARRAY_TYPE;
  }
  else if (checkMatch(s, "stringw", pReadBytes)) {
    return STRINGW_TYPE;
  }
  else if (checkMatch(s, "ptr", pReadBytes)) {
    return PTR_TYPE;
  }
  else if (checkMatch(s, "unknown", pReadBytes)) {
    return UNKNOWN_TYPE;
  }
  else {
    printFunc(("[FATAL] Invalid string provided for type kind: %s", s));
    abortFunc();
    return UNKNOWN_TYPE;
  }
}

INOUT_KIND parseInOutKind(const char* s, PULONG pReadBytes) {
  // Caution on order: "inout" matching should come before "in" matching.
  if (checkMatch(s, "inout", pReadBytes)) {
    return INOUT_ARG;
  }
  else if (checkMatch(s, "in", pReadBytes)) {
    return IN_ARG;
  }
  else if (checkMatch(s, "out", pReadBytes)) {
    return OUT_ARG;
  }
  else {
    printFunc(("[FATAL] Invalid string provided for inout specification\n"));
    abortFunc();
    return INOUT_ARG;
  }
}

COUNT_KIND parseCountKind(const char* s, PULONG pReadBytes) {
  if (checkMatch(s, "byte", pReadBytes)) {
    return BYTE_COUNT;
  }
  else if (checkMatch(s, "elem", pReadBytes)) {
    return ELEM_COUNT;
  }
  else {
    printFunc(("[FATAL] Invalid string provided for array count method\n"));
    abortFunc();
    return BYTE_COUNT;
  }
}

ArgType* CreateArgType(char* buf, PULONG pReadBytes) {
  ArgType* argType;
  TYPE_KIND typeKind;
  ULONG idx = 0;
  ULONG i, width, count;

  argType = (ArgType*)allocFunc(sizeof(ArgType));
  argType->width = 0;
  argType->countKind = BYTE_COUNT;
  argType->arraySize = NULL;
  argType->inOut = INOUT_ARG;
  argType->contentType = NULL;
  argType->fieldCount = 0;
  argType->fieldOffsets = NULL;
  argType->fieldTypes = NULL;

  consume(buf + idx, "{\"type\":\"", &idx);
  typeKind = parseTypeKind(buf + idx, &idx);
  argType->typeKind = typeKind;

  if (typeKind == HANDLE_TYPE) {
    consume(buf + idx, "\"width\":", &idx);
    width = parseInt(buf + idx, &idx);
    argType->width = width;
    if (width != 4 && width != 8) {
      printFunc(("[FATAL] Invalid handle width: %d\n", width));
      abortFunc();
    }
  }
  else if (typeKind == SCALAR_TYPE) {
    consume(buf + idx, "\"width\":", &idx);
    width = parseInt(buf + idx, &idx);
    argType->width = width;
    if (width != 1 && width != 2 && width != 4 && width != 8) {
      printFunc(("[FATAL] Invalid scalar width: %d\n", width));
      abortFunc();
    }
  }
  else if (typeKind == ARRAY_TYPE) {
    consume(buf + idx, "\"countkind\":\"", &idx);
    argType->countKind = parseCountKind(buf + idx, &idx);
    consume(buf + idx, "\"size\":", &idx);
    argType->arraySize = CreateArraySize(buf + idx, &idx);
    consume(buf + idx, "\"width\":", &idx);
    argType->width = parseInt(buf + idx, &idx);
    consume(buf + idx, "\"content\":", &idx);
    argType->contentType = CreateArgType(buf + idx, &idx);
  }
  else if (typeKind == PTR_TYPE) {
    consume(buf + idx, "\"inout\":\"", &idx);
    argType->inOut = parseInOutKind(buf + idx, &idx);
    consume(buf + idx, "\"content\":", &idx);
    argType->contentType = CreateArgType(buf + idx, &idx);
  }
  else if (typeKind == STRUCT_TYPE) {
    consume(buf + idx, "\"fieldcount\":", &idx);
    count = parseInt(buf + idx, &idx);
    argType->fieldCount = count;
    argType->fieldOffsets = (ULONG*)allocFunc(count * sizeof(ULONG));
    argType->fieldTypes = (ArgType**)allocFunc(count * sizeof(ArgType*));
    consume(buf + idx, "\"fields\":[", &idx);
    for (i = 0; i < count; i++) {
      consume(buf + idx, "\"offset\":", &idx);
      argType->fieldOffsets[i] = parseInt(buf + idx, &idx);
      consume(buf + idx, "\"content\":", &idx);
      argType->fieldTypes[i] = CreateArgType(buf + idx, &idx);
    }
    consume(buf + idx, "]", &idx);
  }
  else if (typeKind == UNKNOWN_TYPE) {
    // Assume an unknown type as a DWORD.
    argType->typeKind = SCALAR_TYPE;
    argType->width = 4;
  }

  consume(buf + idx, "}", &idx);
  *pReadBytes += idx;

  return argType;
}

void DeleteArgType(ArgType* argType) {
  ULONG i;

  if (argType->arraySize) {
    DeleteArraySize(argType->arraySize);
  }

  if (argType->contentType) {
    DeleteArgType(argType->contentType);
  }

  for (i = 0; i < argType->fieldCount; i++) {
    DeleteArgType(argType->fieldTypes[i]);
  }

  freeFunc(argType);
}
