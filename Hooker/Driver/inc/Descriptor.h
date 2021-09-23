#pragma once

#include <ntddk.h>

extern "C"
{
  typedef struct _SERVICE_DESCRIPTOR
  {
    PULONG  ServiceTableBase;
    PULONG  ServiceCounterTableBase;
    ULONG   NumberOfService;
    PVOID   ParamTableBase;
  } SERVICE_DESCRIPTOR, * PSERVICE_DESCRIPTOR;

  typedef struct _SERVICE_DESCRIPTOR_TABLE
  {
    SERVICE_DESCRIPTOR  NtosTable;
    SERVICE_DESCRIPTOR  Win32kTable;
    // Maybe more fields here, but truncated.
  } SERVICE_DESCRIPTOR_TABLE, * PSERVICE_DESCRIPTOR_TABLE;
}
