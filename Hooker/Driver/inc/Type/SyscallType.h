#pragma once

#include "Macro.h"
#include "ParseHelper.h"
#include "ArgType.h"

typedef struct _SyscallType {
  bool isWin32;
  bool noMutate;
  bool noCheck;
  LOG_KIND kind;
  ULONG argNum;
  ULONG sysNum;
  ArgType** argTypes;
} SyscallType;

extern SyscallType* ntSyscallTypes[];
extern SyscallType* w32SyscallTypes[];

SyscallType* CreateSyscallType(char* buf, PULONG pReadBytes);
void DeleteSyscallType(SyscallType*);