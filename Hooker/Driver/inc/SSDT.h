#pragma once

#include "Config.h"
#include "VersionConst.h"
#include "Descriptor.h"
#include "State.h"

extern ULONG_PTR origNtSyscalls[];
extern ULONG_PTR origW32Syscalls[];

bool CheckTablePointer(void);
bool InitTablePointer(void);
void RegisterNtHook(ULONG sysNum, ULONG_PTR newFunc);
void RegisterW32Hook(ULONG sysNum, ULONG_PTR newFunc);