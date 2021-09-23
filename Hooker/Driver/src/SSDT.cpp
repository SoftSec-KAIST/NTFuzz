#include <ntddk.h>
#include "..\inc\SSDT.h"

#define SYSCALL_TABLE_MAXLEN 4096

PSERVICE_DESCRIPTOR pNtServiceDescriptor = NULL;
PSERVICE_DESCRIPTOR pW32ServiceDescriptor = NULL;

ULONG_PTR origNtSyscalls[SYSCALL_TABLE_MAXLEN] = { NULL, };
ULONG_PTR origW32Syscalls[SYSCALL_TABLE_MAXLEN] = { NULL, };

bool CheckTablePointer(void) {
  if (pNtServiceDescriptor && pW32ServiceDescriptor) {
    return true;
  }
  else {
    return false;
  }
}

bool InitTablePointer(void)
{
  PSERVICE_DESCRIPTOR_TABLE shadowTable;

  if (!ntBase) {
    DbgPrint("Image base of 'nt' module not initialized yet.\n\n");
    return false;
  }

  shadowTable = (PSERVICE_DESCRIPTOR_TABLE)(ntBase + SHADOW_TABLE_OFFSET);
  pNtServiceDescriptor = &(shadowTable->NtosTable);
  pW32ServiceDescriptor = &(shadowTable->Win32kTable);

  return true;
}

void RegisterNtHook(ULONG sysNum, ULONG_PTR newFunc) {
  ULONG idx = sysNum & 0xfff;
  PULONG origPtr = &(pNtServiceDescriptor->ServiceTableBase[idx]);
  origNtSyscalls[idx] = *origPtr;
  DbgPrint("Backup NtSyscall # 0x%x @ 0x%x\n", sysNum, origNtSyscalls[idx]);
  InterlockedExchange((PLONG)origPtr, newFunc);
}

void RegisterW32Hook(ULONG sysNum, ULONG_PTR newFunc) {
  ULONG idx = sysNum & 0xfff;
  PULONG origPtr = &(pW32ServiceDescriptor->ServiceTableBase[idx]);
  origW32Syscalls[idx] = *origPtr;
  DbgPrint("Backup W32Syscall # 0x%x @ 0x%x\n", sysNum, origW32Syscalls[idx]);
  InterlockedExchange((PLONG)origPtr, newFunc);
}
