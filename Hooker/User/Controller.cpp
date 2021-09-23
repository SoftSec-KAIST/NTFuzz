#include <stdio.h>
#include <string.h>
#include <windows.h>
#include <winioctl.h>
#include "..\Common\Const.h"
#include "ModuleInfo.h"

#define NTOS_PATH "\\SystemRoot\\system32\\ntoskrnl.exe"

HANDLE CreateDeviceHandle(void) {
  HANDLE hDevice;

  hDevice = CreateFileW(DEVICE_NAME_USER, GENERIC_READ | GENERIC_WRITE, 0,
                        NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  if (hDevice == INVALID_HANDLE_VALUE) {
    printf("Error: CreatFile Failed : %d\n", GetLastError());
  }

  return hDevice;
}

bool InitBase(void) {
  HANDLE hDevice = NULL;
  BOOL bResult;
  DWORD writtenBytes;
  ULONG ntBase;
  bool success = true;

  ntBase = GetNtModuleBase();
  if (ntBase == 0) {
    return false;
  }

  hDevice = CreateDeviceHandle();
  if (hDevice == INVALID_HANDLE_VALUE) {
    return false;
  }

  bResult = DeviceIoControl(hDevice, IOCTL_INIT_BASE, &ntBase, sizeof(ntBase),
                            NULL, 0, &writtenBytes, NULL);
  if (!bResult) {
    printf("DeviceIoControl returned error: %d", GetLastError());
    success = false;
  }

  CloseHandle(hDevice);
  return success;
}

bool InstallHook(char * filename) {
  HANDLE hDevice;
  BOOL bResult;
  size_t nameLen;
  size_t inputLen;
  ULONG ntBase;
  WCHAR inputBuf[FILENAME_MAXLEN];
  DWORD writtenBytes;
  bool success = true;

  ntBase = GetNtModuleBase();
  if (ntBase == 0) {
    return false;
  }

  hDevice = CreateDeviceHandle();
  if (hDevice == INVALID_HANDLE_VALUE) {
    return false;
  }

  mbstowcs_s(&nameLen, inputBuf, FILENAME_MAXLEN, filename, strlen(filename));
  inputLen = sizeof(WCHAR) * (nameLen + 1);

  bResult = DeviceIoControl(hDevice, IOCTL_INSTALL_HOOK, inputBuf, inputLen,
                            NULL, 0, &writtenBytes, NULL);
  if (!bResult) {
    printf("DeviceIoControl returned error: %d", GetLastError());
    success = false;
  }

  CloseHandle(hDevice);

  return success;
}

bool SetTarget(char* progName) {
  HANDLE hDevice;
  BOOL bResult;
  size_t nameLen;
  size_t inputLen;
  WCHAR inputBuf[FILENAME_MAXLEN];
  DWORD writtenBytes;
  bool success = true;

  hDevice = CreateDeviceHandle();

  if (hDevice == INVALID_HANDLE_VALUE) {
    return false;
  }

  mbstowcs_s(&nameLen, inputBuf, PROGNAME_MAXLEN, progName, strlen(progName));
  inputLen = sizeof(WCHAR) * (nameLen + 1);

  bResult = DeviceIoControl(hDevice, IOCTL_SET_TARGET, inputBuf, inputLen,
                            NULL, 0, &writtenBytes, NULL);
  if (!bResult) {
    printf("DeviceIoControl returned error: %d", GetLastError());
    success = false;
  }

  CloseHandle(hDevice);

  return success;
}

bool GetCount(PULONG pCount) {
  HANDLE hDevice;
  BOOL bResult;
  DWORD writtenBytes;
  bool success = true;

  if (pCount == NULL) {
    printf("Pointer to store count is invalid\n");
    return false;
  }

  hDevice = CreateDeviceHandle();
  if (hDevice == INVALID_HANDLE_VALUE) {
    return false;
  }

  bResult = DeviceIoControl(hDevice, IOCTL_GET_EXEC_COUNT, NULL, 0,
                            pCount, sizeof(ULONG), &writtenBytes, NULL);
  if (!bResult) {
    printf("DeviceIoControl returned error: %d", GetLastError());
    success = false;
  }

  CloseHandle(hDevice);

  return success;
}

bool SetRatio(char* ratioStr) {
  char* end;
  LONG ratio;
  HANDLE hDevice = NULL;
  BOOL bResult;
  DWORD writtenBytes;
  bool success = true;

  ratio = strtol(ratioStr, &end, 10);
  if (ratioStr == end) {
    printf("Invalid ratio string\n");
    return false;
  }

  hDevice = CreateDeviceHandle();
  if (hDevice == INVALID_HANDLE_VALUE) {
    return false;
  }

  bResult = DeviceIoControl(hDevice, IOCTL_SET_RATIO, &ratio, sizeof(ULONG),
                            NULL, 0, &writtenBytes, NULL);
  if (!bResult) {
    printf("DeviceIoControl returned error: %d", GetLastError());
    success = false;
  }

  CloseHandle(hDevice);

  return success;
}

bool SetConfig(char* triggerStr, char* seedStr) {
  char* end;
  HANDLE hDevice;
  BOOL bResult;
  DWORD writtenBytes;
  ULONG data[2];
  bool success = true;

  data[0] = strtol(triggerStr, &end, 10);
  if (triggerStr == end) {
    printf("Invalid trigger count string\n");
    return false;
  }

  data[1] = strtol(seedStr, &end, 10);
  if (seedStr == end) {
    printf("Invalid PRNG seed string\n");
    return false;
  }

  hDevice = CreateDeviceHandle();
  if (hDevice == INVALID_HANDLE_VALUE) {
    return false;
  }

  bResult = DeviceIoControl(hDevice, IOCTL_SET_CONFIG, data, sizeof(data),
                            NULL, 0, &writtenBytes, NULL);
  if (!bResult) {
    printf("DeviceIoControl returned error: %d", GetLastError());
    success = false;
  }

  CloseHandle(hDevice);

  return success;
}

bool AddPoison(void) {
  HANDLE hDevice = NULL;
  BOOL bResult;
  DWORD writtenBytes;
  bool success = true;

  hDevice = CreateDeviceHandle();
  if (hDevice == INVALID_HANDLE_VALUE) {
    return false;
  }

  bResult = DeviceIoControl(hDevice, IOCTL_ADD_POISON, NULL, 0, NULL, 0,
                            &writtenBytes, NULL);
  if (!bResult) {
    printf("DeviceIoControl returned error: %d", GetLastError());
    success = false;
  }

  CloseHandle(hDevice);
  return success;
}

void usage(char ** argv) {
  printf("(Usage)\n");
  printf("  %s base\n", argv[0]);
  printf("  %s hook <JSON Filepath>\n", argv[0]);
  printf("  %s target <Image Name>\n", argv[0]);
  printf("  %s count\n", argv[0]);
  printf("  %s ratio <Mutation Ratio>\n", argv[0]);
  printf("  %s config <Trigger> <PRNG Seed>\n", argv[0]);
  printf("  %s poison\n", argv[0]);
  exit(1);
}

int main(int argc, char ** argv)
{
  ULONG count = 0;
  bool success = false;

  if (argc < 2) {
    usage(argv);
  }

  if (!strcmp(argv[1], "base")) {
    if (argc != 2) {
      usage(argv);
    }
    success = InitBase();
  }
  else if (!strcmp(argv[1], "hook")) {
    if (argc != 3) {
      usage(argv);
    }
    success = InstallHook(argv[2]);
  }
  else if (!strcmp(argv[1], "target")) {
    if (argc != 3) {
      usage(argv);
    }
    success = SetTarget(argv[2]);
  }
  else if (!strcmp(argv[1], "count")) {
    if (argc != 2) {
      usage(argv);
    }
    success = GetCount(&count);
    printf("Execution count: %d\n", count);
  }
  else if (!strcmp(argv[1], "ratio")) {
    if (argc != 3) {
      usage(argv);
    }
    success = SetRatio(argv[2]);
  }
  else if (!strcmp(argv[1], "config")) {
    if (argc != 4) {
      usage(argv);
    }
    success = SetConfig(argv[2], argv[3]);
  }
  else if (!strcmp(argv[1], "poison")) {
    if (argc != 2) {
      usage(argv);
    }
    success = AddPoison();
  }
  else {
    usage(argv);
  }

  if (success) {
    printf("Operation success!\n");
    return 0;
  }
  else {
    printf("Operation failed!\n");
    return -1;
  }
}
