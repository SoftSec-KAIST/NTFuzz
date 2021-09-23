#include "..\inc\Poison.h"

#define EXALLOC_CPY_LEN 5 // Should be greater or equal than 5.
#define ENTRY_CPY_LEN 11 // Should be greater or equal than 10.

typedef PVOID(__stdcall *ExAlloc)(ULONG, SIZE_T, ULONG);

ExAlloc allocTrmpln = NULL;
PVOID entryTrmpln = NULL;

bool UnprotectMemory(ULONG addr, ULONG len, PMDL* pPMdl, PVOID * pp) {
  NTSTATUS status;
  PMDL pMdl;
  PVOID p;

  pMdl = IoAllocateMdl((PVOID)addr, len, FALSE, FALSE, NULL);
  if (pMdl == NULL) {
    DbgPrint("IoAllocateMdl failed()\n");
    return false;
  }
  __try {
    MmProbeAndLockPages(pMdl, KernelMode, IoReadAccess);
  }
  __except (EXCEPTION_EXECUTE_HANDLER) {
    DbgPrint("Exception during MmProbeAndLockPages\n");
    IoFreeMdl(pMdl);
    return false;
  }

  p = MmMapLockedPagesSpecifyCache(pMdl, KernelMode, MmNonCached, NULL, FALSE,
                                   NormalPagePriority);
  if (p == NULL) {
    MmUnlockPages(pMdl);
    IoFreeMdl(pMdl);
    return false;
  }

  status = MmProtectMdlSystemAddress(pMdl, PAGE_EXECUTE_READWRITE);
  if (status != STATUS_SUCCESS) {
    MmUnmapLockedPages(p, pMdl);
    MmUnlockPages(pMdl);
    IoFreeMdl(pMdl);
    return false;
  }

  *pPMdl = pMdl;
  *pp = p;
  return true;
}

void ReprotectMemory(PMDL pMdl, PVOID p) {
  MmProtectMdlSystemAddress(pMdl, PAGE_EXECUTE_READ);
  MmUnmapLockedPages(p, pMdl);
  MmUnlockPages(pMdl);
  IoFreeMdl(pMdl);
}

void PanicByZero(void) {
  PULONG p = (PULONG)0xffffdfdf;
  *p = 0;
}

PVOID __stdcall
HookExAlloc(ULONG arg1, SIZE_T size, ULONG arg3) {
  PVOID p;
  ULONG i;

  p = allocTrmpln(arg1, size, arg3);
  if (p) {
    // cf. nt!MiMakePrototypePteDirect(). This pool type seems to indicate some
    // special kind of pool: poisoning this pool raises many crashes that we
    // can't reproduce (maybe false positive).
    if (arg1 != 0x80000001) {
      // Initialize with poison value.
      for (i = 0; i < (size / 4); i++) {
        *(((PULONG)p) + i) = heapPoisonValue;
      }
    }
  }

  return p;
}

bool AddHeapPoisoning(void) {
  ULONG trmplnAddr, trmplnLen, dst, src;
  ULONG exAllocAddr;
  PMDL pMdlExAlloc;
  PVOID pExAlloc;

  if (ntBase == 0) {
    return false;
  }
  exAllocAddr = ntBase + EXALLOC_OFFSET;

  trmplnLen = EXALLOC_CPY_LEN + 5 + 16; // 16-bytes for padding.
  allocTrmpln = (ExAlloc)ExAllocatePoolWithTag(NonPagedPool, trmplnLen, 'prvt');
  trmplnAddr = (ULONG)allocTrmpln;
  if (allocTrmpln == NULL) {
    return false;
  }
  memset(allocTrmpln, 0, trmplnLen);

  DbgPrint("Unprotecting 0x%x\n", exAllocAddr);
  if (!UnprotectMemory(exAllocAddr, EXALLOC_CPY_LEN, &pMdlExAlloc, &pExAlloc)) {
    return false;
  }

  // First, construct trampoline code for ExAllocatePoolWithTag: (1) Copy the
  // head instructions from the original ExAllocatePoolWithTag, and (2) Append
  // code that jumps to the original ExAllocatePoolWithTag location.
  memcpy((PVOID)trmplnAddr, (PVOID)exAllocAddr, EXALLOC_CPY_LEN);
  dst = exAllocAddr + EXALLOC_CPY_LEN;
  src = trmplnAddr + EXALLOC_CPY_LEN + 5;
  *(PCHAR)(trmplnAddr + EXALLOC_CPY_LEN) = '\xe9';
  *(PULONG)(trmplnAddr + EXALLOC_CPY_LEN + 1) = dst - src;

  // Now, fix ExAllocatePoolWithTag funciton code to jump to HookExAlloc().
  dst = (ULONG)HookExAlloc;
  src = exAllocAddr + 5;
  *(PCHAR)exAllocAddr = '\xe9';
  *(PULONG)(exAllocAddr + 1) = dst - src;

  ReprotectMemory(pMdlExAlloc, pExAlloc);

  return true;
}
