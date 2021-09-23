#include "..\..\inc\Type\SyscallType.h"

#define SYSCALL_TABLE_MAXLEN (4096)

SyscallType* ntSyscallTypes[SYSCALL_TABLE_MAXLEN] = { NULL, };
SyscallType* w32SyscallTypes[SYSCALL_TABLE_MAXLEN] = { NULL, };

SyscallType* CreateSyscallType(char* buf, PULONG pReadBytes) {
  SyscallType* sysType;
  ULONG idx = 0;
  ULONG i, argNum, debug;

  sysType = (SyscallType*)allocFunc(sizeof(SyscallType));
  // First, parse system call name and decide whether it's in ntdll or win32u.
  consume(buf + idx, "\"", &idx);
  if (checkMatch(buf + idx, "ntdll!", &idx)) {
    sysType->isWin32 = false;
  }
  else {
    consume(buf + idx, "win32u!", &idx);
    sysType->isWin32 = true;
  }
  // Now proceed to "debug" field.
  consume(buf + idx, "\"debug\":", &idx);
  debug = parseInt(buf + idx, &idx);
  if (debug & 4) { // Bit 2.
    sysType->noMutate = true;
  }
  else {
    sysType->noMutate = false;
  }
  if (debug & 8) { // Bit 3.
    sysType->noCheck = true;
  }
  else {
    sysType->noCheck = false;
  }
  debug = debug & 3; // Bit 0-1.
  if (debug == 0) {
    sysType->kind = COMPACT_LOG;
  }
  else if (debug == 1) {
    sysType->kind = DETAIL_LOG;
  }
  else if (debug == 2) {
    sysType->kind = HANDLE_LOG;
  }
  else if (debug == 3) {
    sysType->kind = OUTPUT_LOG;
  }
  // Parse 'argnum' and 'sysnum'.
  consume(buf + idx, "\"argnum\":", &idx);
  argNum = parseInt(buf + idx, &idx);
  sysType->argNum = argNum;
  consume(buf + idx, "\"sysnum\":", &idx);
  sysType->sysNum = parseInt(buf + idx, &idx);
  sysType->argTypes = (ArgType**)allocFunc(argNum * sizeof(ArgType*));
  for (i = 0; i < argNum; i++) {
    // Consume("arg*":)
    consume(buf + idx, "\"arg", &idx);
    consume(buf + idx, "\":", &idx);
    sysType->argTypes[i] = CreateArgType(buf + idx, &idx);
  }

  consume(buf + idx, "}", &idx);
  *pReadBytes += idx;

  return sysType;
}

void DeleteSyscallType(SyscallType* sysType) {
  ULONG i;
  for (i = 0; i < sysType->argNum; i++) {
    DeleteArgType(sysType->argTypes[i]);
  }
  if (sysType->argNum > 0) {
    freeFunc(sysType->argTypes);
  }
  freeFunc(sysType);
}
