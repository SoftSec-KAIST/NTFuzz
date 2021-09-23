#include <windows.h>
#include "..\Common\Const.h"

#define MAX_DRIVER_COUNT 512
#define NTOS_PATH "\\SystemRoot\\system32\\ntoskrnl.exe"


typedef struct _RTL_PROCESS_MODULE_INFORMATION
{
    HANDLE Section;
    ULONG MappedBase;
    ULONG ImageBase;
    ULONG ImageSize;
    ULONG Flags;
    USHORT LoadOrderIndex;
    USHORT InitOrderIndex;
    USHORT LoadCount;
    USHORT OffsetToFileName;
    CHAR FullPathName[256];
} RTL_PROCESS_MODULE_INFORMATION, *PRTL_PROCESS_MODULE_INFORMATION;

typedef struct _RTL_PROCESS_MODULES
{
    ULONG NumberOfModules;
    RTL_PROCESS_MODULE_INFORMATION Modules[1];
} RTL_PROCESS_MODULES, *PRTL_PROCESS_MODULES;

typedef enum _SYSTEM_INFORMATION_CLASS {
  SystemBasicInformation = 0,
  SystemModuleInformation = 11,
} SYSTEM_INFORMATION_CLASS;

typedef NTSTATUS(NTAPI* QUERYSYSTEM)(
  SYSTEM_INFORMATION_CLASS SystemInformationClass,
  PVOID SystemInformation,
  ULONG SystemInformationLength,
  PULONG ReturnLength OPTIONAL
);

void GetSystemModuleInfo(PRTL_PROCESS_MODULES* p, ULONG * n) {
  HMODULE hModule;
  QUERYSYSTEM queryFunc;
  PRTL_PROCESS_MODULES moduleBuf;
  ULONG moduleBufSize;
  ULONG nBytes, size;
  NTSTATUS stat;

  hModule = LoadLibrary(L"ntdll.dll");
  moduleBufSize = sizeof(ULONG);
  moduleBufSize += MAX_DRIVER_COUNT * sizeof(RTL_PROCESS_MODULE_INFORMATION);
  queryFunc = (QUERYSYSTEM)GetProcAddress(hModule, "NtQuerySystemInformation");
  moduleBuf = (PRTL_PROCESS_MODULES)malloc(moduleBufSize);
  memset(moduleBuf, 0, moduleBufSize);
  stat = queryFunc(SystemModuleInformation, moduleBuf, moduleBufSize, &nBytes);
  if (stat != 0) {
    printf("Failed to query system modules: error = %d \n", GetLastError());
    *n = 0;
  }
  size = *(PULONG)moduleBuf;
  *p = moduleBuf;
  *n = min(size, MAX_DRIVER_COUNT);
}

ULONG GetNtModuleBase(void) {
  PRTL_PROCESS_MODULES moduleInfo = NULL;
  ULONG nModules, i;

  GetSystemModuleInfo(&moduleInfo, &nModules);
  for (i = 0; i < nModules; i++) {
    if (strcmp(moduleInfo->Modules[i].FullPathName, NTOS_PATH) == 0) {
      return moduleInfo->Modules[i].ImageBase;
    }
  }
  return 0;
}