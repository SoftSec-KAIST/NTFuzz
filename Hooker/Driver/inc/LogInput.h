#pragma once

#include "Type\ArgType.h"
#include "Type/SyscallType.h"
#include "State.h"
#include "Log.h"

ULONG LogInputArg(Log* pLog, SyscallType* sysType, ULONG argIdx, ULONG arg);
