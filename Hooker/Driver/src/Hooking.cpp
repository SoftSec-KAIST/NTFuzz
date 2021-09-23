// Including both <ntddk.h> & <ntifs.h> often raises error. Caution.
#include "..\inc\HookingHelper.h"
#include "..\inc\Hooking.h"

#define BUFSIZE 0x800000

char jsonbuf[BUFSIZE + 0x100]; // 4MB

void InitSyscallTypes(PCHAR buf, ULONG bufLen) {
  ULONG position = 0;
  ULONG sysIdx;
  SyscallType* sysType;
  while (position < bufLen - 10) {
    DbgPrint("CreateSyscallType()\n");
    sysType = CreateSyscallType(buf + position, &position);
    DbgPrint("argNum = %d, sysNum = %d\n", sysType->argNum, sysType->sysNum);
    sysIdx = sysType->sysNum & 0xfff;
    if (sysType->isWin32) {
      DbgPrint("Updating w32SyscallTypes[%d]\n", sysIdx);
      w32SyscallTypes[sysIdx] = sysType;
    }
    else {
      DbgPrint("Updating ntSyscallTypes[%d]\n", sysIdx);
      ntSyscallTypes[sysIdx] = sysType;
    }
  }
}

bool InstallHooks(PWCHAR filepath) {
  ULONG uOldAttr = 0;
  KAPC_STATE apcState;
  PEPROCESS GUIProcess;
  NTSTATUS status;
  ULONG nBytes;
  bool success = true;

  nBytes = ReadFile(filepath, jsonbuf, BUFSIZE);
  if (nBytes == 0) {
    DbgPrint("Failed to read in type specification file\n\n");
  }

  InitSyscallTypes(jsonbuf, nBytes);
  InitLogs();

  if (!CheckTablePointer()) {
    DbgPrint("System call table pointers not initialized, cannot install\n\n");
    return false;
  }

  status = FindGUIProcess(&GUIProcess);
  if (status != STATUS_SUCCESS) {
    DbgPrint("Failed to find GUI process (NTSTATUS = 0x%x)\n\n", status);
    return false;
  }

  __try {
    DbgPrint("Calling KeStackAttachProcess()\n\n");
    KeStackAttachProcess(GUIProcess, &apcState);
    DbgPrint("Calling DisableWriteProtect()\n\n");
    DisableWriteProtect(&uOldAttr);
    DbgPrint("Calling RegisterHooks()\n\n");
    RegisterProcessHooks();
    RegisterGeneralHooks();
    DbgPrint("Calling RestoreWriteProtectBit()\n\n");
    RestoreWriteProtectBit(uOldAttr);
    DbgPrint("Calling KeUnstackDetachProcess()\n\n");
    KeUnstackDetachProcess(&apcState);
  } __except (EXCEPTION_EXECUTE_HANDLER) {
    DbgPrint("Exception while registering hooks\n\n");
    success = false;
  }

  return success;
}
