#pragma once

#include "Type/Typedef.h"
#include "Type/SyscallType.h"

#define COMPACT_LOG_WINDOW_SIZE 512
#define DETAIL_LOG_WINDOW_SIZE 128
#define HANDLE_LOG_WINDOW_SIZE 128
#define OUTPUT_LOG_WINDOW_SIZE 16
#define ARG_MAXNUM 20
#define IN_ENTRY_MAXNUM_SMALL 128
#define IN_ENTRY_MAXNUM_LARGE 512
#define OUT_ENTRY_MAXNUM 16
#define ARR_SIZE_MAXNUM 16
#define STR_ENTRY_MAXNUM 8
#define STR_MAXLEN 0x200
#define SMALL_BUF_MAXNUM 8
#define SMALL_BUF_MAXSIZE 0x200
#define LARGE_BUF_MAXNUM 2
#define LARGE_BUF_MAXSIZE 0x1000
#define OUT_BUF_MAXNUM 4
#define OUT_BUF_MAXSIZE 0x1000
#define MUTATION_MAXNUM 256

typedef struct _Arg {
  ULONG val;
  ArgType* typ;
} Arg;

typedef struct _Entry {
  ULONG base;
  ULONG offset;
  ULONG val;
  ArgType* typ;
} Entry;

typedef struct _ArrSizeInfo {
  ULONG base;
  ULONG offset;
  ULONG count;
} ArrSizeInfo;

typedef struct _StrEntry {
  ULONG base;
  ULONG offset;
  ULONG len;
  wchar_t* str;
} StrEntry;

// Large buffer for scalar array.
typedef struct _Buffer {
  ULONG base;
  ULONG offset;
  ULONG count;
  ULONG width;
  char* content;
} Buffer;

typedef struct _Mutation {
  ULONG base;
  ULONG offset;
  ULONG orig;
  ULONG mutated;
} Mutation;

typedef struct _Log {
  ULONG epoch;
  ULONG sysNum;
  ULONG argNum;
  ULONG argMax;
  Arg* args;
  ULONG inEntryNum;
  ULONG inEntryMax;
  Entry* inEntries;
  ULONG outEntryNum;
  ULONG outEntryMax;
  Entry* outEntries;
  ULONG arrSizeNum;
  ULONG arrSizeMax;
  ArrSizeInfo* arrSizes;
  ULONG strEntryNum;
  ULONG strEntryMax;
  StrEntry* strEntries;
  ULONG smallBufNum;
  ULONG smallBufMax;
  Buffer* smallBufs;
  ULONG largeBufNum;
  ULONG largeBufMax;
  Buffer* largeBufs;
  ULONG outBufNum;
  ULONG outBufMax;
  Buffer* outBufs;
  ULONG mutationNum;
  ULONG mutationMax;
  Mutation* mutations;
  ULONG ret;
} Log;

// Initialize log structs.
void InitLogs(void);
// Reset log status when a new process execution starts.
void ResetLogs(void);
// Obtain a log struct to use.
Log* ObtainLog(SyscallType* sysType);
// Setup log struct and make it prepared for recording payloads.
void SetupLog(Log* log, ULONG execCount, SyscallType * sysType);
void LogArg(Log* log, ULONG argIdx, ULONG argVal, ArgType* typ);
void LogInEntry(Log* log, ULONG base, ULONG offset, ULONG val, ArgType* typ);
void LogOutEntry(Log* log, ULONG base, ULONG offset, ULONG val, ArgType* typ);
void LogArrSize(Log* log, ULONG base, ULONG offset, ULONG count);
ULONG LookupArrSize(Log* log, ULONG base, ULONG offset);
void LogStrEntry(Log* log, ULONG base, ULONG offset);
void LogSmallBuf(Log* log, ULONG base, ULONG offset, ULONG count, ULONG width);
void LogLargeBuf(Log* log, ULONG base, ULONG offset, ULONG count, ULONG width);
void LogOutBuf(Log* log, ULONG base, ULONG offset, ULONG count, ULONG width);
void LogArgMutation(Log* log, ULONG argIdx, ULONG orig, ULONG mut);
void LogMemMutation(Log* log, ULONG base, ULONG offset, ULONG orig, ULONG mut);
void LogReturn(Log* log, ULONG ret);
