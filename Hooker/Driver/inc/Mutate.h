#pragma once

#include "Type\ArgType.h"
#include "Type\SyscallType.h"
#include "Config.h"
#include "Mutate.h"
#include "State.h"
#include "Log.h"

ULONG MutateArg(Log* pLog, SyscallType* sysType, ULONG argIdx, ULONG arg);
