#include <ntddk.h>
#include <string.h>
#include "..\..\Common\Const.h"
#include "..\inc\State.h"
#include "..\inc\SSDT.h"
#include "..\inc\Hooking.h"
#include "..\inc\Log.h"
#include "..\inc\Poison.h"

void PrintIrpInfo(PIRP Irp);
NTSTATUS OpenDevice(PDEVICE_OBJECT DeviceObject, PIRP Irp);
NTSTATUS CloseDevice(PDEVICE_OBJECT DeviceObject, PIRP Irp);
NTSTATUS ControlDevice(PDEVICE_OBJECT DeviceObject, PIRP Irp);
void UnloadDriver(PDRIVER_OBJECT pDriverObject);

// DEBUG
void PrintIrpInfo(PIRP Irp)
{
  PIO_STACK_LOCATION  irpSp;
  irpSp = IoGetCurrentIrpStackLocation(Irp);

  DbgPrint("Irp->AssociatedIrp.SystemBuffer = 0x%p\n",
            Irp->AssociatedIrp.SystemBuffer);
  DbgPrint("irpSp->Parameters.DeviceIoControl.Type3InputBuffer = 0x%p\n",
            irpSp->Parameters.DeviceIoControl.Type3InputBuffer);
  DbgPrint("irpSp->Parameters.DeviceIoControl.InputBufferLength = %d\n",
            irpSp->Parameters.DeviceIoControl.InputBufferLength);
  DbgPrint("Irp->UserBuffer (outBuf) = 0x%p\n", Irp->UserBuffer);
  DbgPrint("irpSp->Parameters.DeviceIoControl.OutputBufferLength = %d\n",
            irpSp->Parameters.DeviceIoControl.OutputBufferLength);
  return;
}

// Called by the I/O system when the IOCTL is opened.
NTSTATUS OpenDevice(PDEVICE_OBJECT DeviceObject, PIRP Irp)
{
  // Just complete the request successfully.
  UNREFERENCED_PARAMETER(DeviceObject);
  DbgPrint("OpenDevice() called\n\n");

  Irp->IoStatus.Status = STATUS_SUCCESS;
  Irp->IoStatus.Information = 0;
  IoCompleteRequest(Irp, IO_NO_INCREMENT);

  return STATUS_SUCCESS;
}

// Called by the I/O system when the IOCTL is closed.
NTSTATUS CloseDevice(PDEVICE_OBJECT DeviceObject, PIRP Irp)
{
  // Just complete the request successfully.
  UNREFERENCED_PARAMETER(DeviceObject);
  DbgPrint("CloseDevice() called\n\n");

  Irp->IoStatus.Status = STATUS_SUCCESS;
  Irp->IoStatus.Information = 0;
  IoCompleteRequest(Irp, IO_NO_INCREMENT);

  return STATUS_SUCCESS;
}

// Called by the I/O system when the IOCTL function is called.
NTSTATUS ControlDevice(PDEVICE_OBJECT DeviceObject, PIRP Irp)
{
  PIO_STACK_LOCATION  irpSp; // Current stack pointer
  NTSTATUS ntStatus = STATUS_SUCCESS;
  PWCHAR inBufW;
  PULONG inBufL;
  PULONG outBufL;
  WCHAR filename[FILENAME_MAXLEN + 4];

  UNREFERENCED_PARAMETER(DeviceObject);

  irpSp = IoGetCurrentIrpStackLocation(Irp);

  switch ((int)irpSp->Parameters.DeviceIoControl.IoControlCode)
  {
  case IOCTL_INIT_BASE:
    DbgPrint("IOCTL_INIT_BASE called\n\n");
    PrintIrpInfo(Irp);
    __try {
      inBufL = (PULONG)irpSp->Parameters.DeviceIoControl.Type3InputBuffer;
      if ((PULONG)MmUserProbeAddress < inBufL) {
        inBufL = (PULONG)MmUserProbeAddress;
      }
      DbgPrint("Input: 0x%x\n", inBufL[0]);
      ntBase = inBufL[0];
    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
      ntStatus = GetExceptionCode();
      DbgPrint("Exception while accessing input buffer: 0x%x\n\n", ntStatus);
    }
    break;

  case IOCTL_INSTALL_HOOK:
    DbgPrint("IOCTL_INSTALL_HOOK called\n\n");
    if (hookInstalled) {
      break;
    }
    PrintIrpInfo(Irp);
    __try {
      inBufW = (PWCHAR)irpSp->Parameters.DeviceIoControl.Type3InputBuffer;
      if ((PWCHAR)MmUserProbeAddress < inBufW) {
        inBufW = (PWCHAR)MmUserProbeAddress;
      }
      wcsncpy(filename, inBufW, FILENAME_MAXLEN);
    } __except(EXCEPTION_EXECUTE_HANDLER) {
      ntStatus = GetExceptionCode();
      DbgPrint("Exception while accessing input buffer: 0x%x\n\n", ntStatus);
      break;
    }

    if (!InitTablePointer()) {
      DbgPrint("Failed to initialize pointer to SDT!\n\n");
      return STATUS_INTERNAL_ERROR;
    }

    if (!InstallHooks(filename)) {
      DbgPrint("Failed to install hooks!\n\n");
      return STATUS_INTERNAL_ERROR;
    }
    hookInstalled = true;
    break;

  case IOCTL_SET_TARGET:
    DbgPrint("IOCTL_SET_TARGET called\n\n");
    PrintIrpInfo(Irp);
    __try {
      inBufW = (PWCHAR)irpSp->Parameters.DeviceIoControl.Type3InputBuffer;
      if ((PWCHAR)MmUserProbeAddress < inBufW) {
        inBufW = (PWCHAR)MmUserProbeAddress;
      }
      SetTargetProg(inBufW);
    } __except (EXCEPTION_EXECUTE_HANDLER) {
      ntStatus = GetExceptionCode();
      DbgPrint("Exception in IOCTL_SET_TARGET: 0x%x\n\n", ntStatus);
    }
    break;

  case IOCTL_GET_EXEC_COUNT:
    DbgPrint("IOCTL_GET_EXEC_COUNT called\n\n");
    PrintIrpInfo(Irp);
    __try {
      outBufL = (PULONG)Irp->UserBuffer;
      if ((PULONG)MmUserProbeAddress < outBufL) {
        outBufL = (PULONG)MmUserProbeAddress;
      }
      DbgPrint("Output: 0x%x\n", execCount);
      *outBufL = execCount;
    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
      ntStatus = GetExceptionCode();
      DbgPrint("Exception in IOCTL_GET_EXEC_COUNT: 0x%x\n\n", ntStatus);
    }
    break;

  case IOCTL_SET_RATIO:
    DbgPrint("IOCTL_SET_RATIO called\n\n");
    PrintIrpInfo(Irp);
    __try {
      inBufL = (PULONG)irpSp->Parameters.DeviceIoControl.Type3InputBuffer;
      if ((PULONG)MmUserProbeAddress < inBufL) {
        inBufL = (PULONG)MmUserProbeAddress;
      }
      DbgPrint("Input: 0x%x\n", *inBufL);
      mutateRatio = *inBufL;
    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
      ntStatus = GetExceptionCode();
      DbgPrint("Exception in IOCTL_SET_RATIO: 0x%x\n\n", ntStatus);
    }
    break;

  case IOCTL_SET_CONFIG:
    DbgPrint("IOCTL_SET_CONFIG called\n\n");
    PrintIrpInfo(Irp);
    __try {
      inBufL = (PULONG)irpSp->Parameters.DeviceIoControl.Type3InputBuffer;
      if ((PULONG)MmUserProbeAddress < inBufL) {
        inBufL = (PULONG)MmUserProbeAddress;
      }
      DbgPrint("Input: 0x%x, 0x%x\n", inBufL[0], inBufL[1]);
      triggerCount = inBufL[0];
      SetPRNGSeed(inBufL[1]);
      BackupInitSeed();
      DecidePoisonValue();
    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
      ntStatus = GetExceptionCode();
      DbgPrint("Exception in IOCTL_SET_CONFIG: 0x%x\n\n", ntStatus);
    }
    break;

  case IOCTL_ADD_POISON:
    DbgPrint("IOCTL_ADD_POISON called\n\n");
    PrintIrpInfo(Irp);
    if (!AddHeapPoisoning()) {
      ntStatus = STATUS_INVALID_PARAMETER;
    }
    break;

  default:
    ntStatus = STATUS_INVALID_DEVICE_REQUEST;
    DbgPrint("Unsupported IOCTL %x\n\n",
              irpSp->Parameters.DeviceIoControl.IoControlCode);
    break;
  }

  // Finish the I/O operation by simply completing the packet and returning
  // the same status as in the packet itself.
  Irp->IoStatus.Status = ntStatus;
  IoCompleteRequest(Irp, IO_NO_INCREMENT);

  return ntStatus;
}

void UnloadDriver(PDRIVER_OBJECT pDriverObject)
{
  UNICODE_STRING dosDevName;

  DbgPrint("Driver is being unloaded\n\n");

  RtlInitUnicodeString(&dosDevName, DOS_DEVICE_NAME_KERNEL);
  IoDeleteSymbolicLink(&dosDevName);

  if (pDriverObject->DeviceObject) {
    IoDeleteDevice(pDriverObject->DeviceObject);
  }
}

extern "C" {
  NTSTATUS
  DriverEntry(PDRIVER_OBJECT pDriverObject, PUNICODE_STRING pRegistryPath)
  {
    NTSTATUS ntStatus = STATUS_SUCCESS;
    UNICODE_STRING devName;
    UNICODE_STRING dosDevName;
    PDEVICE_OBJECT pDeviceObject = NULL;

    UNREFERENCED_PARAMETER(pRegistryPath);
    DbgPrint("DriverEntry started!\n");

    RtlInitUnicodeString(&devName, DEVICE_NAME_KERNEL);
    RtlInitUnicodeString(&dosDevName, DOS_DEVICE_NAME_KERNEL);

    ntStatus = IoCreateDevice(
      pDriverObject,            // Driver Object
      0,                        // Don't use a device extension
      &devName,                 // Device name
      FILE_DEVICE_UNKNOWN,      // Device type
      FILE_DEVICE_SECURE_OPEN,  // Device characteristics
      FALSE,                    // Not an exclusive device
      &pDeviceObject);          // Device Object
    if (ntStatus != STATUS_SUCCESS) {
      DbgPrint("Failed to create device!\n\n");
      return ntStatus;
    }

    pDriverObject->MajorFunction[IRP_MJ_CREATE] = OpenDevice;
    pDriverObject->MajorFunction[IRP_MJ_CLOSE] = CloseDevice;
    pDriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = ControlDevice;
    pDriverObject->DriverUnload = UnloadDriver;

    IoCreateSymbolicLink(&dosDevName, &devName);
    if (ntStatus != STATUS_SUCCESS) {
      DbgPrint("Failed to create symbolic link!\n\n");
      IoDeleteDevice(pDeviceObject);
      return ntStatus;
    }

    return STATUS_SUCCESS;
  }
}
