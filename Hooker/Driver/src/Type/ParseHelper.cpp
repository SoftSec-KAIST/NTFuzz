#include "..\..\inc\Type\ParseHelper.h"

bool checkMatch(const char* buf, const char* matchStr, PULONG pReadBytes) {
  size_t len = strlen(matchStr);

  if (strncmp(buf, matchStr, len) == 0) {
    *pReadBytes += len;
    return true;
  }
  else {
    return false;
  }
}

void consume(const char* buf, const char* matchStr, PULONG pReadBytes) {
  size_t matchLen = strlen(matchStr);
  int i = 0;

  for (i = 0; i < MAX_TRY_LEN; i++) {
    if (strncmp(buf + i, matchStr, matchLen) == 0) {
      *pReadBytes += (i + matchLen);
      return;
    }
  }
  printFunc(("Failed to find matching string %s from %30s\n", matchStr, buf));
  abortFunc();
}

ULONG parseInt(const char* buf, PULONG pReadBytes) {
  ULONG result = 0;
  int i;

  for (i = 0; '0' <= buf[i] && buf[i] <= '9'; i++) {
    result = result * 10 + (buf[i] - '0');
  }
  *pReadBytes += i;
  return result;
}
