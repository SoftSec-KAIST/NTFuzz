#include <ntifs.h>
#include "..\inc\ProcessHooker.h"

#define PROGNAME_FIELD_OFFSET 12

typedef ULONG(*NtCreateUserProcess)(
  PHANDLE pHandle,
  ULONG arg2,
  ULONG arg3,
  ULONG arg4,
  ULONG arg5,
  ULONG arg6,
  ULONG arg7,
  ULONG arg8,
  ULONG arg9,
  ULONG arg10,
  ULONG arg11
);

ULONG HookNtCreateUserProcess(PHANDLE pHandle, ULONG arg2, ULONG arg3,
                              ULONG arg4, ULONG arg5, ULONG arg6, ULONG arg7,
                              ULONG arg8, ULONG arg9, ULONG arg10, ULONG arg11)
{
  ULONG sysIdx = CREATE_USER_PROC_SYSNUM & 0xfff;
  WCHAR buf[PROGNAME_MAXLEN];
  PWCHAR progName = NULL;
  bool isTarget = false;
  NtCreateUserProcess origFunc = (NtCreateUserProcess)origNtSyscalls[sysIdx];
  ULONG status;
  PEPROCESS childProc;
  ULONG ret;

  if (arg11 && arg11 < MmUserProbeAddress) {
    __try {
      progName = *(PWCHAR*)(arg11 + PROGNAME_FIELD_OFFSET);
    }
    __except (EXCEPTION_EXECUTE_HANDLER) { }
  }

  if (progName && progName < (PWCHAR)MmUserProbeAddress) {
    __try {
      wcscpy_s(buf, PROGNAME_MAXLEN, progName);
      isTarget = IsTargetProg(buf);
    }
    __except (EXCEPTION_EXECUTE_HANDLER) { }
  }

  ret = origFunc(pHandle, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
  if (isTarget && ret == STATUS_SUCCESS) {
    status = ObReferenceObjectByHandle(*pHandle, PROCESS_ALL_ACCESS, NULL,
                                       KernelMode, (PVOID*)&childProc, NULL);
    if (status == STATUS_SUCCESS) {
      DbgPrint("ObReferenceObjectByHandle() success\n");
      // Update states. Note that NtTerminateProcess could have been missed.
      execCount = 0;
      ResetLogs();
      targetPID = (ULONG)PsGetProcessId(childProc);
    }
  }

  return ret;
}

void RegisterProcessHooks(void) {
  RegisterNtHook(CREATE_USER_PROC_SYSNUM, (ULONG_PTR)HookNtCreateUserProcess);
}
