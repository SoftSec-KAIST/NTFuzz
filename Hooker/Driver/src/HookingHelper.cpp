#include "..\inc\HookingHelper.h"

#define ROOT_MAXLEN 40

// XXX. Below are copied from <winternl.h>. Hard to include in device driver.

typedef enum _SYSTEM_INFORMATION_CLASS {
  SystemBasicInformation = 0,
  SystemPerformanceInformation = 2,
  SystemTimeOfDayInformation = 3,
  SystemProcessInformation = 5,
  SystemProcessorPerformanceInformation = 8,
  SystemInterruptInformation = 23,
  SystemExceptionInformation = 33,
  SystemRegistryQuotaInformation = 37,
  SystemLookasideInformation = 45,
  SystemCodeIntegrityInformation = 103,
  SystemPolicyInformation = 134,
} SYSTEM_INFORMATION_CLASS;

typedef struct _SYSTEM_PROCESS_INFORMATION {
  ULONG NextEntryOffset;
  ULONG NumberOfThreads;
  UCHAR Reserved1[48];
  UNICODE_STRING ImageName;
  KPRIORITY BasePriority;
  HANDLE UniqueProcessId;
  // Truncated below.
} SYSTEM_PROCESS_INFORMATION, * PSYSTEM_PROCESS_INFORMATION;

typedef NTSTATUS(NTAPI* QUERYSYSTEM)(
  SYSTEM_INFORMATION_CLASS SystemInformationClass,
  PVOID SystemInformation,
  ULONG SystemInformationLength,
  PULONG ReturnLength OPTIONAL
  );

void DisableWriteProtect(PULONG pOldAttr)
{
  ULONG uAttr = 0;

  _asm
  {
    cli;
    push eax;
    mov  eax, cr0;
    mov  uAttr, eax;
    and eax, 0xFFFEFFFF; // CR0 16 BIT = 0
    mov  cr0, eax;
    pop  eax;
  };

  *pOldAttr = uAttr;
}

void RestoreWriteProtectBit(ULONG oldAttr)
{
  _asm
  {
    push eax;
    mov  eax, oldAttr;
    mov  cr0, eax;
    pop  eax;
    sti;
  }
}

ULONG ReadFile(PWCHAR filepath, PCHAR buf, ULONG bufLen) {
  WCHAR realpath[ROOT_MAXLEN + FILENAME_MAXLEN + 4];
  HANDLE handle;
  NTSTATUS status;
  IO_STATUS_BLOCK ioStatusBlock;
  UNICODE_STRING unipath;
  OBJECT_ATTRIBUTES objAttr;
  LARGE_INTEGER byteOffset;
  ULONG nBytes = 0;

  // Do not try to perform any file operations at higher IRQL levels.
  if (KeGetCurrentIrql() != PASSIVE_LEVEL) {
    DbgPrint("Invalid IRQL level\n\n");
    return nBytes;
  }

  wcsncpy(realpath, L"\\DosDevices\\", ROOT_MAXLEN);
  wcsncat(realpath, filepath, FILENAME_MAXLEN);
  RtlInitUnicodeString(&unipath, realpath);
  InitializeObjectAttributes(&objAttr, &unipath,
    OBJ_CASE_INSENSITIVE | OBJ_KERNEL_HANDLE,
    NULL, NULL);
  status = ZwCreateFile(&handle, GENERIC_READ, &objAttr, &ioStatusBlock,
                        NULL, FILE_ATTRIBUTE_NORMAL, 0, FILE_OPEN,
                        FILE_SYNCHRONOUS_IO_NONALERT, NULL, 0);
  if (status != STATUS_SUCCESS) {
    DbgPrint("ZwCreateFile failed: NTSTATUS = 0x%x\n\n", status);
    return nBytes;
  }

  byteOffset.LowPart = 0;
  byteOffset.HighPart = 0;
  status = ZwReadFile(handle, NULL, NULL, NULL, &ioStatusBlock,
    buf, bufLen, &byteOffset, NULL);
  if (status == STATUS_END_OF_FILE || status == STATUS_SUCCESS){
    DbgPrint("Successfully read %d bytes\n\n", ioStatusBlock.Information);
    nBytes = ioStatusBlock.Information;
  }
  else {
    DbgPrint("ZwReadFile failed: NTSTATUS = 0x%x\n\n", status);
  }

  ZwClose(handle);

  return nBytes;
}

NTSTATUS FindGUIProcess(PEPROCESS* pProcess) {
  NTSTATUS status = STATUS_SUCCESS;
  ULONG i;
  UNICODE_STRING funcName;
  QUERYSYSTEM zwQuerySystemInformation;
  UCHAR* buf = NULL;
  ULONG len = 0;
  SYSTEM_PROCESS_INFORMATION* procInfo = NULL;
  UNICODE_STRING procName;
  ULONG nextOffset = 0;

  RtlInitUnicodeString(&funcName, L"ZwQuerySystemInformation");
  zwQuerySystemInformation = (QUERYSYSTEM)MmGetSystemRoutineAddress(&funcName);
  if (!zwQuerySystemInformation)
  {
    DbgPrint("Failed to obtain ZwQuerySystemInformation\n\n");
    return false;
  }

  for (i = 0; i < 5 && buf == NULL; i++) {
    status = zwQuerySystemInformation(SystemProcessInformation, NULL, 0, &len);
    if (status != STATUS_INFO_LENGTH_MISMATCH) {
      return status;
    }
    if (len == 0) {
      return STATUS_UNSUCCESSFUL;
    }
    buf = (UCHAR*)ExAllocatePoolWithTag(PagedPool, len, 'prvt');
    if (buf == NULL)
    {
      return STATUS_INSUFFICIENT_RESOURCES;
    }
    status = zwQuerySystemInformation(SystemProcessInformation, buf, len, NULL);
    //if (!NT_SUCCESS(status)) {
    if (status != STATUS_SUCCESS) {
      ExFreePool(buf);
      buf = NULL;
      if (status != STATUS_INFO_LENGTH_MISMATCH) {
        return status;
      }
    }
  }

  if (buf == NULL) {
    return STATUS_INFO_LENGTH_MISMATCH;
  }

  procInfo = (PSYSTEM_PROCESS_INFORMATION)buf;
  DbgPrint("Valid procInfo found: 0x%p (len = 0x%x)\n\n", buf, len);
  RtlInitUnicodeString(&procName, L"explorer.exe");
  while (true) {
    __try {
      if (RtlEqualUnicodeString(&(procInfo->ImageName), &procName, TRUE)) {
        DbgPrint("Found Process!\n\n");
        return PsLookupProcessByProcessId(procInfo->UniqueProcessId, pProcess);
      }
      else {
        nextOffset = procInfo->NextEntryOffset;
        DbgPrint("Proceed to next process (offset = 0x%x)\n\n", nextOffset);
      }
    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
      DbgPrint("Exception while accessing procInfo\n\n");
    }

    if (nextOffset == 0) {
      return STATUS_UNSUCCESSFUL;
    }

    procInfo = (PSYSTEM_PROCESS_INFORMATION)(((UCHAR*)procInfo) + nextOffset);
    if (procInfo == NULL) {
      return STATUS_UNSUCCESSFUL;
    }
  }

  return STATUS_UNSUCCESSFUL;
}
