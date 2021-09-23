#pragma once

#define allocFunc(size) ExAllocatePoolWithTag(PagedPool, size, 'prvt');
#define freeFunc(ptr) ExFreePool(ptr)
#define printFunc(content) DbgPrint content
#define abortFunc() DbgPrint(NULL)
