#include <ntddk.h>
#include "..\inc\Log.h"

#define PADDING 1

ULONG compactLogIdx = 0;
ULONG compactLogWrapped = 0;
Log compactLogs[COMPACT_LOG_WINDOW_SIZE + PADDING];
ULONG detailLogIdx = 0;
ULONG detailLogWrapped = 0;
Log detailLogs[DETAIL_LOG_WINDOW_SIZE + PADDING];
ULONG handleLogIdx = 0;
ULONG handleLogWrapped = 0;
Log handleLogs[HANDLE_LOG_WINDOW_SIZE + PADDING];
ULONG outputLogIdx = 0;
ULONG outputLogWrapped = 0;
Log outputLogs[OUTPUT_LOG_WINDOW_SIZE + PADDING];

void initArgs(Log* log, ULONG max) {
  ULONG size;

  log->argMax = max;
  if (max > 0) {
    size = sizeof(Arg) * max;
    log->args = (Arg*)ExAllocatePoolWithTag(PagedPool, size, 'prvt');
  }
}

void initInputEntries(Log* log, ULONG max) {
  ULONG size;

  log->inEntryNum = 0;
  log->inEntryMax = max;
  if (max > 0) {
    size = sizeof(Entry) * max;
    log->inEntries = (Entry*)ExAllocatePoolWithTag(PagedPool, size, 'prvt');
  }
}

void initOutputEntries(Log* log, ULONG max) {
  ULONG size;

  log->outEntryNum = 0;
  log->outEntryMax = max;

  if (max == 0) {
    log->outEntries = NULL;
    return;
  }

  size = sizeof(Entry) * max;
  log->outEntries = (Entry*)ExAllocatePoolWithTag(PagedPool, size, 'prvt');
}

void initArrSizes(Log* log, ULONG max) {
  ULONG size;

  log->arrSizeNum = 0;
  log->arrSizeMax = max;

  if (max == 0) {
    log->arrSizes = NULL;
    return;
  }

  size = sizeof(ArrSizeInfo) * max;
  log->arrSizes = (ArrSizeInfo*)ExAllocatePoolWithTag(PagedPool, size, 'prvt');
}

void initStrEntries(Log* log, ULONG max) {
  ULONG size, strSize, i;
  wchar_t* s;

  log->strEntryNum = 0;
  log->strEntryMax = max;

  if (max == 0) {
    log->strEntries = NULL;
    return;
  }

  size = sizeof(StrEntry) * max;
  log->strEntries = (StrEntry*)ExAllocatePoolWithTag(PagedPool, size, 'prvt');
  strSize = sizeof(wchar_t) * (STR_MAXLEN + 1);
  for (i = 0; i < max; i++) {
    s = (wchar_t*)ExAllocatePoolWithTag(PagedPool, strSize + 8, 'prvt');
    log->strEntries[i].str = s;
  }
}

void initSmallBufs(Log* log, ULONG max) {
  ULONG size, i;
  char* p;

  log->smallBufNum = 0;
  log->smallBufMax = max;

  if (max == 0) {
    log->smallBufs = NULL;
    return;
  }

  size = sizeof(Buffer) * max;
  log->smallBufs = (Buffer*)ExAllocatePoolWithTag(PagedPool, size, 'prvt');
  for (i = 0; i < max; i++) {
    p = (char*)ExAllocatePoolWithTag(PagedPool, SMALL_BUF_MAXSIZE + 8, 'prvt');
    log->smallBufs[i].content = p;
  }
}

void initLargeBufs(Log* log, ULONG max) {
  ULONG size, i;
  char* p;

  log->largeBufNum = 0;
  log->largeBufMax = max;

  if (max == 0) {
    log->largeBufs = NULL;
    return;
  }

  size = sizeof(Buffer) * max;
  log->largeBufs = (Buffer*)ExAllocatePoolWithTag(PagedPool, size, 'prvt');
  for (i = 0; i < max; i++) {
    p = (char*)ExAllocatePoolWithTag(PagedPool, LARGE_BUF_MAXSIZE + 8, 'prvt');
    log->largeBufs[i].content = p;
  }
}

void initOutBufs(Log* log, ULONG max) {
  ULONG size, i;
  char* p;

  log->outBufNum = 0;
  log->outBufMax = max;

  if (max == 0) {
    log->outBufs = NULL;
    return;
  }

  size = sizeof(Buffer) * max;
  log->outBufs = (Buffer*)ExAllocatePoolWithTag(PagedPool, size, 'prvt');
  for (i = 0; i < max; i++) {
    p = (char*)ExAllocatePoolWithTag(PagedPool, OUT_BUF_MAXSIZE + 8, 'prvt');
    log->outBufs[i].content = p;
  }
}

void initMutations(Log* log, ULONG max) {
  ULONG size;

  log->mutationNum = 0;
  log->mutationMax = max;

  if (max == 0) {
    log->mutations = NULL;
    return;
  }

  size = sizeof(Mutation) * max;
  log->mutations = (Mutation*)ExAllocatePoolWithTag(PagedPool, size, 'prvt');
}

void InitLogs(void) {
  ULONG i;
  Log* log;

  // Initialize compact logs.
  for (i = 0; i < COMPACT_LOG_WINDOW_SIZE + PADDING; i++) {
    log = &compactLogs[i];
    initArgs(log, ARG_MAXNUM);
    initInputEntries(log, IN_ENTRY_MAXNUM_SMALL);
    initOutputEntries(log, OUT_ENTRY_MAXNUM);
    initArrSizes(log, ARR_SIZE_MAXNUM);
    initStrEntries(log, STR_ENTRY_MAXNUM);
    initSmallBufs(log, SMALL_BUF_MAXNUM);
    initLargeBufs(log, OUT_ENTRY_MAXNUM);
    initOutBufs(log, 0);
    initMutations(log, MUTATION_MAXNUM);
  }

  // Initialize detail logs.
  for (i = 0; i < DETAIL_LOG_WINDOW_SIZE + PADDING; i++) {
    log = &detailLogs[i];
    initArgs(log, ARG_MAXNUM);
    initInputEntries(log, IN_ENTRY_MAXNUM_LARGE);
    initOutputEntries(log, OUT_ENTRY_MAXNUM);
    initArrSizes(log, ARR_SIZE_MAXNUM);
    initStrEntries(log, STR_ENTRY_MAXNUM);
    initSmallBufs(log, SMALL_BUF_MAXNUM);
    initLargeBufs(log, LARGE_BUF_MAXNUM);
    initOutBufs(log, 0);
    initMutations(log, MUTATION_MAXNUM);
  }

  // Initialize handle logs.
  for (i = 0; i < HANDLE_LOG_WINDOW_SIZE + PADDING; i++) {
    log = &handleLogs[i];
    initArgs(log, ARG_MAXNUM);
    initInputEntries(log, IN_ENTRY_MAXNUM_LARGE);
    initOutputEntries(log, OUT_ENTRY_MAXNUM);
    initArrSizes(log, ARR_SIZE_MAXNUM);
    initStrEntries(log, STR_ENTRY_MAXNUM);
    initSmallBufs(log, SMALL_BUF_MAXNUM);
    initLargeBufs(log, LARGE_BUF_MAXNUM);
    initOutBufs(log, 0);
    initMutations(log, MUTATION_MAXNUM);
  }

  // Initialize output logs.
  for (i = 0; i < OUTPUT_LOG_WINDOW_SIZE + PADDING; i++) {
    log = &outputLogs[i];
    initArgs(log, ARG_MAXNUM);
    initInputEntries(log, IN_ENTRY_MAXNUM_LARGE);
    initOutputEntries(log, OUT_ENTRY_MAXNUM);
    initArrSizes(log, ARR_SIZE_MAXNUM);
    initStrEntries(log, STR_ENTRY_MAXNUM);
    initSmallBufs(log, SMALL_BUF_MAXNUM);
    initLargeBufs(log, LARGE_BUF_MAXNUM);
    initOutBufs(log, OUT_BUF_MAXNUM);
    initMutations(log, MUTATION_MAXNUM);
  }
}

void ResetLogs(void) {
  compactLogWrapped = 0;
  compactLogIdx = 0;
  detailLogWrapped = 0;
  detailLogIdx = 0;
  handleLogWrapped = 0;
  handleLogIdx = 0;
  outputLogWrapped = 0;
  outputLogIdx = 0;
}

Log* ObtainLog(SyscallType* sysType) {
  Log* log;
  if (sysType->kind == COMPACT_LOG) {
    if (compactLogIdx >= COMPACT_LOG_WINDOW_SIZE) {
      compactLogWrapped = 1;
      compactLogIdx = 0;
    }
    log = &compactLogs[compactLogIdx++];
  }
  else if (sysType->kind == DETAIL_LOG) {
    if (detailLogIdx >= DETAIL_LOG_WINDOW_SIZE) {
      detailLogWrapped = 1;
      detailLogIdx = 0;
    }
    log = &detailLogs[detailLogIdx++];
  }
  else if (sysType->kind == HANDLE_LOG) {
    if (handleLogIdx >= HANDLE_LOG_WINDOW_SIZE) {
      handleLogWrapped = 1;
      handleLogIdx = 0;
    }
    log = &handleLogs[handleLogIdx++];
  }
  else if (sysType->kind == OUTPUT_LOG) {
    if (outputLogIdx >= OUTPUT_LOG_WINDOW_SIZE) {
      outputLogWrapped = 1;
      outputLogIdx = 0;
    }
    log = &outputLogs[outputLogIdx++];
  }
  else {
    DbgPrint("[ERROR] Invalid log kind: %d\n", sysType->kind);
    log = NULL;
  }
  return log;
}

void SetupLog(Log* log, ULONG execCount, SyscallType * sysType) {
  log->epoch = execCount;
  log->sysNum = sysType->sysNum;
  log->argNum = sysType->argNum;
  log->ret = 0;
  log->inEntryNum = 0;
  log->outEntryNum = 0;
  log->arrSizeNum = 0;
  log->strEntryNum = 0;
  log->smallBufNum = 0;
  log->largeBufNum = 0;
  log->mutationNum = 0;
}

void LogArg(Log* log, ULONG argIdx, ULONG argVal, ArgType* typ) {
  if (argIdx < log->argNum && argIdx < log->argMax) {
    // Let's abuse inEntries to log arguments. We'll base = 0 to indicate this.
    log->args[argIdx].val = argVal;
    log->args[argIdx].typ = typ;
  }
}

void LogInEntry(Log* log, ULONG base, ULONG offset, ULONG val, ArgType* typ) {
  ULONG num = log->inEntryNum;
  if (num < log->inEntryMax) {
    log->inEntries[num].base = base;
    log->inEntries[num].offset = offset;
    log->inEntries[num].val = val;
    log->inEntries[num].typ = typ;
    log->inEntryNum = num + 1;
  }
}

void LogOutEntry(Log* log, ULONG base, ULONG offset, ULONG val, ArgType* typ) {
  ULONG num = log->outEntryNum;
  if (num < log->outEntryMax) {
    log->outEntries[num].base = base;
    log->outEntries[num].offset = offset;
    log->outEntries[num].val = val;
    log->outEntries[num].typ = typ;
    log->outEntryNum = num + 1;
  }
}

void LogArrSize(Log* log, ULONG base, ULONG offset, ULONG count) {
  ULONG num = log->arrSizeNum;
  if (num < log->arrSizeMax) {
    log->arrSizes[num].base = base;
    log->arrSizes[num].offset = offset;
    log->arrSizes[num].count = count;
    log->arrSizeNum = num + 1;
  }
}

ULONG LookupArrSize(Log* log, ULONG base, ULONG offset) {
  ULONG i;
  ArrSizeInfo arrSize;

  for (i = 0; i < log->arrSizeNum; i++) {
    arrSize = log->arrSizes[i];
    if (arrSize.base == base && arrSize.offset == offset) {
      return arrSize.count;
    }
  }
  return 1;
}

void LogStrEntry(Log* log, ULONG base, ULONG offset) {
  ULONG num = log->strEntryNum;
  ULONG len;
  wchar_t* src;
  if (num < log->strEntryMax) {
    src = (wchar_t*)(base + offset);
    len = wcslen(src);
    log->strEntries[num].base = base;
    log->strEntries[num].offset = offset;
    log->strEntries[num].len = len;
    // Will truncate if source string is longer than MAX_STR_LEN.
    wcsncpy(log->strEntries[num].str, src, min(STR_MAXLEN, len));
    log->strEntryNum = num + 1;
  }
}

void LogSmallBuf(Log* log, ULONG base, ULONG offset, ULONG count, ULONG width) {
  ULONG num = log->smallBufNum;
  ULONG size;
  void* src;
  if (num < log->smallBufMax) {
    size = count * width;
    src = (void*)(base + offset);
    log->smallBufs[num].base = base;
    log->smallBufs[num].offset = offset;
    log->smallBufs[num].count = count;
    log->smallBufs[num].width = width;
    memcpy(log->smallBufs[num].content, src, min(size, SMALL_BUF_MAXSIZE));
    log->smallBufNum = num + 1;
  }
}

void LogLargeBuf(Log* log, ULONG base, ULONG offset, ULONG count, ULONG width) {
  ULONG num = log->largeBufNum;
  ULONG size;
  void* src;
  if (num < log->largeBufMax) {
    size = count * width;
    src = (void*)(base + offset);
    log->largeBufs[num].base = base;
    log->largeBufs[num].offset = offset;
    log->largeBufs[num].count = count;
    log->largeBufs[num].width = width;
    memcpy(log->largeBufs[num].content, src, min(size, LARGE_BUF_MAXSIZE));
    log->largeBufNum = num + 1;
  }
}

void LogOutBuf(Log* log, ULONG base, ULONG offset, ULONG count, ULONG width) {
  ULONG num = log->outBufNum;
  ULONG size;
  void* src;
  if (num < log->outBufMax) {
    size = count * width;
    src = (void*)(base + offset);
    log->outBufs[num].base = base;
    log->outBufs[num].offset = offset;
    log->outBufs[num].count = count;
    log->outBufs[num].width = width;
    memcpy(log->outBufs[num].content, src, min(size, OUT_BUF_MAXSIZE));
    log->outBufNum = num + 1;
  }
}

void LogArgMutation(Log* log, ULONG argIdx, ULONG orig, ULONG mut) {
  ULONG num = log->mutationNum;
  if (num < log->mutationMax) {
    // Let's abuse inEntries to log arguments. We'll base = 0 to indicate this.
    log->mutations[num].base = 0;
    log->mutations[num].offset = argIdx;
    log->mutations[num].orig = orig;
    log->mutations[num].mutated = mut;
    log->mutationNum = num + 1;
  }
}

void LogMemMutation(Log* log, ULONG base, ULONG offset, ULONG orig, ULONG mut) {
  ULONG num = log->mutationNum;
  if (num < log->mutationMax) {
    log->mutations[num].base = base;
    log->mutations[num].offset = offset;
    log->mutations[num].orig = orig;
    log->mutations[num].mutated = mut;
    log->mutationNum = num + 1;
  }
}

void LogReturn(Log* log, ULONG ret) {
  log->ret = ret;
}