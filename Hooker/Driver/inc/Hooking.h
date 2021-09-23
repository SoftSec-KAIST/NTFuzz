#pragma once

#include "..\..\Common\Const.h"
#include "Type\SyscallType.h"
#include "SSDT.h"
#include "Log.h"
#include "State.h"
#include "ProcessHooker.h"
#include "GeneralHooker.h"

bool InstallHooks(PWCHAR filename);