#pragma once

#include <ntddk.h>
#include "Macro.h"


#define MAX_TRY_LEN 128

bool checkMatch(const char* buf, const char* matchStr, PULONG pReadBytes);
void consume(const char* buf, const char* matchStr, PULONG pReadBytes);
ULONG parseInt(const char* buf, PULONG pReadBytes);
