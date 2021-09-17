module DLLAnalysis.Const

let NUMREGARG = 0 // x86. Do not consider 'this' in 'ecx' here.
let MIN_MEMFUNC_SIZE = 8
let MAX_MEMFUNC_SIZE = 4096

let STRUCT_DEPTH_LIMIT = 3
let READ_WRITE_NLOC_LIMIT = 1024
let SIDE_EFFECT_LIMIT = 50

let SYMBOL_PREFIX = "s"
let INT_SUFFIX = "_int"
let LOC_SUFFIX = "_loc"
let FUNC_SUFFIX = "_fun"
let TYP_SUFFIX = "_typ"

let NT_SYSCALL_DLL = "ntdll"
let GUI_SYSCALL_DLL = "win32u"

let BLACKLIST_SYSCALLS =
  [
    // To prevent jumping to unexpected address in userland.
    "ntdll!NtContinue";
    "ntdll!NtContinueEx";
    // To prevent userland access violation.
    "ntdll!NtAllocateVirtualMemory";
    "ntdll!NtAllocateVirtualMemoryEx";
    "ntdll!NtFreeVirtualMemory";
    "ntdll!NtProtectVirtualMemory";
    // Should manually hook to manage processes.
    "ntdll!NtCreateProcess";
    "ntdll!NtCreateProcessEx";
    "ntdll!NtCreateUserProcess";
    // Terminating system call.
    "ntdll!NtTerminateProcess";
    "ntdll!NtTerminateThread";
    "ntdll!NtRaiseException"; // (Aborts when 'HandleException' arg is false)
    // To prevent delay.
    "ntdll!NtDelayExecution";
    "ntdll!NtSuspendThread";
    "ntdll!NtSuspendProcess";
  ]
  |> Set.ofList

let EVALSET_SYSCALLS =
  [
    // docs.microsoft.com/en-us/windows/win32/api/winternl/
    "ntdll!NtClose";
    "ntdll!NtCreateFile";
    "ntdll!NtDeviceIoControlFile";
    "ntdll!NtNotifyChangeMultipleKeys";
    "ntdll!NtOpenFile";
    "ntdll!NtQueryInformationProcess";
    "ntdll!NtQueryInformationThread";
    "ntdll!NtQueryMultipleValueKey";
    "ntdll!NtQueryObject";
    "ntdll!NtQuerySystemInformation";
    "ntdll!NtQuerySystemTime";
    "ntdll!NtRenameKey";
    "ntdll!NtSetInformationKey";
    // docs.microsoft.com/en-us/windows-hardware/drivers/ddi/ntifs/
    // (Nt*)
    "ntdll!NtAllocateVirtualMemory";
    "ntdll!NtClose";
    "ntdll!NtCreateFile";
    "ntdll!NtCreateSection";
    // "ntdll!NtCreateSectionEx"; // Not always supported.
    "ntdll!NtDeviceIoControlFile";
    "ntdll!NtDuplicateToken";
    "ntdll!NtFlushBuffersFileEx";
    "ntdll!NtFreeVirtualMemory";
    "ntdll!NtFsControlFile";
    "ntdll!NtLockFile";
    "ntdll!NtOpenFile";
    "ntdll!NtOpenProcessTokenEx";
    "ntdll!NtOpenThreadTokenEx";
    "ntdll!NtQueryDirectoryFile";
    "ntdll!NtQueryInformationByName";
    "ntdll!NtQueryInformationFile";
    "ntdll!NtQueryInformationToken";
    "ntdll!NtQueryObject";
    "ntdll!NtQueryQuotaInformationFile";
    "ntdll!NtQuerySecurityObject";
    "ntdll!NtQueryVirtualMemory";
    "ntdll!NtQueryVolumeInformationFile";
    "ntdll!NtReadFile";
    "ntdll!NtSetInformationFile";
    "ntdll!NtSetInformationThread";
    "ntdll!NtSetInformationToken";
    "ntdll!NtSetQuotaInformationFile";
    "ntdll!NtSetSecurityObject";
    "ntdll!NtUnlockFile";
    "ntdll!NtWriteFile";
     // (Zw*)
    "ntdll!NtAllocateVirtualMemory";
    "ntdll!NtCreateEvent";
    "ntdll!NtDeleteFile";
    "ntdll!NtDeviceIoControlFile";
    "ntdll!NtDuplicateObject";
    "ntdll!NtDuplicateToken";
    "ntdll!NtFlushBuffersFile";
    "ntdll!NtFlushBuffersFileEx";
    "ntdll!NtFlushVirtualMemory";
    "ntdll!NtFreeVirtualMemory";
    "ntdll!NtFsControlFile";
    "ntdll!NtLockFile";
    "ntdll!NtNotifyChangeKey";
    "ntdll!NtOpenDirectoryObject";
    "ntdll!NtOpenProcessTokenEx";
    "ntdll!NtOpenThreadTokenEx";
    "ntdll!NtQueryDirectoryFile";
    "ntdll!NtQueryEaFile";
    "ntdll!NtQueryInformationToken";
    "ntdll!NtQueryObject";
    "ntdll!NtQueryQuotaInformationFile";
    "ntdll!NtQuerySecurityObject";
    "ntdll!NtQueryVirtualMemory";
    "ntdll!NtQueryVolumeInformationFile";
    "ntdll!NtSetEaFile";
    "ntdll!NtSetEvent";
    "ntdll!NtSetInformationToken";
    "ntdll!NtSetInformationVirtualMemory";
    "ntdll!NtSetQuotaInformationFile";
    "ntdll!NtSetSecurityObject";
    "ntdll!NtSetVolumeInformationFile";
    "ntdll!NtUnlockFile";
    "ntdll!NtWaitForSingleObject";
    // docs.microsoft.com/en-us/windows-hardware/drivers/ddi/ntddk/
    // (Nt*)
    "ntdll!NtOpenProcess";
    // (Zw*)
    "ntdll!NtAllocateLocallyUniqueId";
    "ntdll!NtDeviceIoControlFile";
    "ntdll!NtOpenProcess";
    "ntdll!NtPowerInformation";
    "ntdll!NtQueryVolumeInformationFile";
    "ntdll!NtSetInformationThread";
    "ntdll!NtTerminateProcess";
    // https://docs.microsoft.com/en-us/windows-hardware/drivers/ddi/wdm/
    // (Nt*)
    "ntdll!NtCommitComplete";
    "ntdll!NtCommitEnlistment";
    "ntdll!NtCommitTransaction";
    "ntdll!NtCreateEnlistment";
    "ntdll!NtCreateResourceManager";
    "ntdll!NtCreateTransaction";
    "ntdll!NtCreateTransactionManager";
    "ntdll!NtEnumerateTransactionObject";
    "ntdll!NtGetNotificationResourceManager";
    "ntdll!NtOpenEnlistment";
    "ntdll!NtOpenResourceManager";
    "ntdll!NtOpenTransaction";
    "ntdll!NtOpenTransactionManager";
    "ntdll!NtPowerInformation";
    "ntdll!NtPrepareComplete";
    "ntdll!NtPrepareEnlistment";
    "ntdll!NtPrePrepareComplete";
    "ntdll!NtPrePrepareEnlistment";
    "ntdll!NtQueryInformationEnlistment";
    "ntdll!NtQueryInformationResourceManager";
    "ntdll!NtQueryInformationTransaction";
    "ntdll!NtQueryInformationTransactionManager";
    "ntdll!NtReadOnlyEnlistment";
    "ntdll!NtRecoverEnlistment";
    "ntdll!NtRecoverResourceManager";
    "ntdll!NtRecoverTransactionManager";
    "ntdll!NtRenameTransactionManager";
    "ntdll!NtRollbackComplete";
    "ntdll!NtRollbackEnlistment";
    "ntdll!NtRollbackTransaction";
    "ntdll!NtRollforwardTransactionManager";
    "ntdll!NtSetInformationEnlistment";
    "ntdll!NtSetInformationResourceManager";
    "ntdll!NtSetInformationTransaction";
    "ntdll!NtSetInformationTransactionManager";
    "ntdll!NtSinglePhaseReject";
    // (Zw*)
    "ntdll!NtClose";
    "ntdll!NtCommitComplete";
    "ntdll!NtCommitEnlistment";
    "ntdll!NtCommitTransaction";
    "ntdll!NtCreateDirectoryObject";
    "ntdll!NtCreateEnlistment";
    "ntdll!NtCreateFile";
    "ntdll!NtCreateKey";
    "ntdll!NtCreateKeyTransacted";
    "ntdll!NtCreateResourceManager";
    "ntdll!NtCreateSection";
    "ntdll!NtCreateTransaction";
    "ntdll!NtCreateTransactionManager";
    "ntdll!NtDeleteKey";
    "ntdll!NtDeleteValueKey";
    "ntdll!NtEnumerateKey";
    "ntdll!NtEnumerateTransactionObject";
    "ntdll!NtEnumerateValueKey";
    "ntdll!NtFlushKey";
    "ntdll!NtGetNotificationResourceManager";
    "ntdll!NtLoadDriver";
    "ntdll!NtMakeTemporaryObject";
    "ntdll!NtMapViewOfSection";
    "ntdll!NtOpenEnlistment";
    "ntdll!NtOpenEvent";
    "ntdll!NtOpenFile";
    "ntdll!NtOpenKey";
    "ntdll!NtOpenKeyEx";
    "ntdll!NtOpenKeyTransacted";
    "ntdll!NtOpenKeyTransactedEx";
    "ntdll!NtOpenResourceManager";
    "ntdll!NtOpenSection";
    "ntdll!NtOpenSymbolicLinkObject";
    "ntdll!NtOpenTransaction";
    "ntdll!NtOpenTransactionManager";
    "ntdll!NtPrepareComplete";
    "ntdll!NtPrepareEnlistment";
    "ntdll!NtPrePrepareComplete";
    "ntdll!NtPrePrepareEnlistment";
    "ntdll!NtQueryFullAttributesFile";
    "ntdll!NtQueryInformationEnlistment";
    "ntdll!NtQueryInformationFile";
    "ntdll!NtQueryInformationResourceManager";
    "ntdll!NtQueryInformationTransaction";
    "ntdll!NtQueryInformationTransactionManager";
    "ntdll!NtQueryKey";
    "ntdll!NtQuerySymbolicLinkObject";
    "ntdll!NtQueryValueKey";
    "ntdll!NtReadFile";
    "ntdll!NtReadOnlyEnlistment";
    "ntdll!NtRecoverEnlistment";
    "ntdll!NtRecoverResourceManager";
    "ntdll!NtRecoverTransactionManager";
    "ntdll!NtRollbackComplete";
    "ntdll!NtRollbackEnlistment";
    "ntdll!NtRollbackTransaction";
    "ntdll!NtRollforwardTransactionManager";
    "ntdll!NtSetInformationEnlistment";
    "ntdll!NtSetInformationFile";
    "ntdll!NtSetInformationResourceManager";
    "ntdll!NtSetInformationTransaction";
    "ntdll!NtSetValueKey";
    "ntdll!NtSinglePhaseReject";
    "ntdll!NtUnloadDriver";
    "ntdll!NtUnmapViewOfSection";
    "ntdll!NtWriteFile";
  ]
  |> Set.ofList

let BASE_DIR =
  let exePath = System.Reflection.Assembly.GetEntryAssembly().Location
  let buildDir = System.IO.Path.GetDirectoryName(exePath)
  System.IO.Directory.GetParent(buildDir).FullName

let HEADER_DIR =
  let headerProjectDir = System.IO.Path.Combine(BASE_DIR, "WinHeader")
  System.IO.Path.Combine(headerProjectDir, "HeaderData")
