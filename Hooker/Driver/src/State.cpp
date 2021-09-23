#include <ntddk.h>
#include "..\inc\State.h"

ULONG ntBase = 0;
bool hookInstalled = false;
WCHAR targetProg[PROGNAME_MAXLEN + 4];
ULONG targetPID = 0;
ULONG execCount = 0;
ULONG triggerCount = 0;
ULONG mutateRatio = 0;
ULONG zInit = 0;
ULONG z1 = 0;
ULONG z2 = 0;
ULONG z3 = 0;
ULONG z4 = 0;
ULONG heapPoisonValue = 0xf3dacaf0;
ULONG stackPoisonValue = 0xf3dac0da;

void SetTargetProg(PWCHAR progName) {
  wcsncpy(targetProg, progName, PROGNAME_MAXLEN);
}

bool IsTargetProg(PWCHAR inputName) {
  ULONG targLen, inputLen;
  targLen = wcslen(targetProg);
  inputLen = wcslen(inputName);
  if (inputLen >= targLen) {
    return (wcscmp(targetProg, inputName + inputLen - targLen) == 0);
  } else {
    return false;
  }
}

void SetPRNGSeed(ULONG prngSeed) {
  z1 = prngSeed;
  if (z1 < 128) { // LFSR113 requires initial seed to be larger than 127.
    z1 += 128;
  }
  z2 = z1;
  z3 = z1;
  z4 = z1;
}

void BackupInitSeed(void) {
  zInit = z1;
}

// LFSR113
ULONG Rand(void) {
  ULONG b;
  b = ((z1 << 6) ^ z1) >> 13;
  z1 = ((z1 & 4294967294U) << 18) ^ b;
  b = ((z2 << 2) ^ z2) >> 27;
  z2 = ((z2 & 4294967288U) << 2) ^ b;
  b = ((z3 << 13) ^ z3) >> 21;
  z3 = ((z3 & 4294967280U) << 7) ^ b;
  b = ((z4 << 3) ^ z4) >> 12;
  z4 = ((z4 & 4294967168U) << 13) ^ b;
  return (z1 ^ z2 ^ z3 ^ z4);
}

void DecidePoisonValue(void) {
  heapPoisonValue = (0xf3 << 24) + ((Rand() & 0xff) << 16) + (Rand() & 0xffff);
  stackPoisonValue = (0xf3 << 24) + ((Rand() & 0xff) << 16) + (Rand() & 0xffff);
}