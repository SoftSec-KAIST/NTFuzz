#pragma once

#include "..\..\Common\Const.h"

#define MAX_CHILD_COUNT 8

extern ULONG ntBase;
extern bool hookInstalled;
extern ULONG targetPID;
extern ULONG execCount;
extern ULONG triggerCount;
extern ULONG mutateRatio;
extern ULONG heapPoisonValue;
extern ULONG stackPoisonValue;

void SetTargetProg(PWCHAR progName);
bool IsTargetProg(PWCHAR imageName);
void SetPRNGSeed(ULONG prngSeed);
void BackupInitSeed(void);
ULONG Rand(void);
void DecidePoisonValue(void);