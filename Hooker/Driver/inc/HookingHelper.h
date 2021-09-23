#pragma once

#include <ntifs.h>
#include "..\..\Common\Const.h"

void DisableWriteProtect(PULONG pOldAttr);
void RestoreWriteProtectBit(ULONG oldAttr);
ULONG ReadFile(PWCHAR filepath, PCHAR buf, ULONG bufLen);
NTSTATUS FindGUIProcess(PEPROCESS* pProcess);