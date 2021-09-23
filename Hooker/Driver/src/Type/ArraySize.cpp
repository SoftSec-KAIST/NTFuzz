#include "..\..\inc\Type\ArraySize.h"

#define MAX_ARG_NUM 20
#define MAX_ARR_SIZE 0x2000

ULONG args[MAX_ARG_NUM];

SIZE_KIND parseSizeKind(const char* s, PULONG pReadBytes) {
  if (checkMatch(s, "unknown", pReadBytes)) {
    return UNKNOWN_SIZE;
  }
  else if (checkMatch(s, "fixed", pReadBytes)) {
    return FIXED_SIZE;
  }
  else if (checkMatch(s, "argfield", pReadBytes)) {
    return ARG_FIELD_SIZE;
  }
  else if (checkMatch(s, "adjacentfield", pReadBytes)) {
    return ADJACENT_FIELD_SIZE;
  }
  else if (checkMatch(s, "add", pReadBytes)) {
    return ADD;
  }
  else if (checkMatch(s, "sub", pReadBytes)) {
    return SUBTRACT;
  }
  else if (checkMatch(s, "mult", pReadBytes)) {
    return MULT;
  }
  else if (checkMatch(s, "div", pReadBytes)) {
    return DIV;
  }
  else {
    printFunc(("[FATAL] Invalid string provided for array size: %s", s));
    abortFunc();
    return UNKNOWN_SIZE;
  }
}

ArraySize* CreateArraySize(char* buf, PULONG pReadBytes) {
  ULONG idx = 0;
  int i;
  SIZE_KIND sizeKind;
  ArraySize* arrSize;

  arrSize = (ArraySize*)allocFunc(sizeof(ArraySize));
  arrSize->fixedSize = 0;
  arrSize->idx = 0;
  arrSize->offsetCount = 0;
  arrSize->oprnd1 = NULL;
  arrSize->oprnd2 = NULL;

  consume(buf + idx, "{\"kind\":\"", &idx);
  sizeKind = parseSizeKind(buf + idx, &idx);
  arrSize->sizeKind = sizeKind;

  switch (sizeKind) {
  case FIXED_SIZE:
    consume(buf + idx, "\"val\":", &idx);
    arrSize->fixedSize = parseInt(buf + idx, &idx);
    break;
  case ARG_FIELD_SIZE:
    consume(buf + idx, "\"idx\":", &idx);
    arrSize->idx = parseInt(buf + idx, &idx);
    // Caution: Fall through.
  case ADJACENT_FIELD_SIZE:
    consume(buf + idx, "\"offsets\":[", &idx);
    for (i = 0; i < MAX_OFFSET_COUNT; i++) {
      // Break if offset list ended.
      if (checkMatch(buf + idx, "]", &idx)) {
        break;
      }
      // Consume if there is a comma, and then parse the offset integer.
      checkMatch(buf + idx, ",", &idx);
      arrSize->offsets[i] = parseInt(buf + idx, &idx);
    }
    arrSize->offsetCount = i;
    break;
  case ADD:
  case SUBTRACT:
  case MULT:
  case DIV:
    consume(buf + idx, "\"val1\":", &idx);
    arrSize->oprnd1 = CreateArraySize(buf + idx, &idx);
    consume(buf + idx, "\"val2\":", &idx);
    arrSize->oprnd2 = CreateArraySize(buf + idx, &idx);
    break;
  default:
    printFunc(("[FATAL] Invalid size kind: %d\n", sizeKind));
    abortFunc();
    break;
  }

  consume(buf + idx, "}", &idx);
  *pReadBytes += idx;

  return arrSize;
}

void DeleteArraySize(ArraySize* size) {
  if (size->oprnd1) {
    DeleteArraySize(size->oprnd1);
  }
  if (size->oprnd2) {
    DeleteArraySize(size->oprnd2);
  }
  freeFunc(size);
}

void StoreArg(ULONG argIdx, ULONG argVal) {
  if (argIdx < MAX_ARG_NUM) {
    args[argIdx] = argVal;
  }
}

ULONG EvalSizeInternal(ArraySize* arrSize, ULONG strucBase)
{
  ULONG i, v;
  ULONG oprnd1, oprnd2;

  switch (arrSize->sizeKind) {
  case UNKNOWN_SIZE:
    return 1;
  case FIXED_SIZE:
    return arrSize->fixedSize;
  case ARG_FIELD_SIZE:
  case ADJACENT_FIELD_SIZE:
    v = arrSize->sizeKind == ARG_FIELD_SIZE ? args[arrSize->idx] : strucBase;
    for (i = 0; i < arrSize->offsetCount; i++) {
      if (v < 0x10000 || v > MmUserProbeAddress) {
        printFunc(("[WARNING] Invalid memory in EvalSize: 0x%x\n", v));
        return 1;
      }
      __try {
        v = *(PULONG)(v + arrSize->offsets[i]);
      }
      __except (EXCEPTION_EXECUTE_HANDLER) {
        printFunc(("Exception while accessing memory in EvalSize: 0x%x\n", v));
        return 1;
      }
    }
    return v & 0xffff;

  case ADD:
    oprnd1 = EvalSizeInternal(arrSize->oprnd1, strucBase);
    oprnd2 = EvalSizeInternal(arrSize->oprnd2, strucBase);
    return oprnd1 + oprnd2;

  case SUBTRACT:
    oprnd1 = EvalSizeInternal(arrSize->oprnd1, strucBase);
    oprnd2 = EvalSizeInternal(arrSize->oprnd2, strucBase);
    return oprnd1 - oprnd2;

  case MULT:
    oprnd1 = EvalSizeInternal(arrSize->oprnd1, strucBase);
    oprnd2 = EvalSizeInternal(arrSize->oprnd2, strucBase);
    return oprnd1 * oprnd2;

  case DIV:
    oprnd1 = EvalSizeInternal(arrSize->oprnd1, strucBase);
    oprnd2 = EvalSizeInternal(arrSize->oprnd2, strucBase);
    if (oprnd2 == 0) {
      return 1; // Handle as unknown.
    }
    else {
      return oprnd1 / oprnd2;
    }

  default:
    printFunc(("[FATAL] Invalid size kind: %d\n", arrSize->sizeKind));
    abortFunc();
    return 1;
  }
}

ULONG EvalSize(ArraySize* arrSize, ULONG strucBase) {
  ULONG size = EvalSizeInternal(arrSize, strucBase);
  if (size > MAX_ARR_SIZE) {
    size = MAX_ARR_SIZE;
  }
  return size;
}
