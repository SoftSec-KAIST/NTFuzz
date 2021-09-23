#pragma once

#include "Type\ArgType.h"
#include "Type\SyscallType.h"
#include "State.h"
#include "Log.h"

void LogOutputArg(Log* pLog, SyscallType* sysType, ULONG argIdx, ULONG arg);
