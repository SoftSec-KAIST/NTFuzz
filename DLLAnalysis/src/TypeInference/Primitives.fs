module DLLAnalysis.TypeAnalysis.Primitives

open DLLAnalysis
open WinHeader

let private handle = Handle
let private handlePtrOut = Ptr (handle, Out)
let private dword = Scalar 4UL
let private archWord = Scalar 4UL // x86
let private archWordPtrInOut = Ptr (archWord, InOut)
let private pSizeOut = Ptr (archWord, Out)
let private pSizeInOut = Ptr (archWord, InOut)
let private ulongPtr = archWord
let private memExtParam = APIInfo.findType "MEM_EXTENDED_PARAMETER"
let private objAttrs = APIInfo.findType "OBJECT_ATTRIBUTES"
let private largeInt = APIInfo.findType "LARGE_INTEGER"
let private sectionInherit = APIInfo.findType "SECTION_INHERIT"

let ntCreateSectionType =
  [
    handlePtrOut; // PHANDLE SectionHandle
    dword; // ACCESS_MASK DesiredAccess
    Ptr (objAttrs, In); // POBJECT_ATTRIBUTES ObjectAttributes
    Ptr (largeInt, In); // PLARGE_INTEGER MaximumSize,
    dword; // ULONG SectionPageProtection
    dword; // ULONG AllocationAttributes
    handle // HANDLE FileHandle
  ]

let ntCreateSectionExType =
  let extParams = Ptr (Array (CountSize, ArgField (8, []), memExtParam), InOut)
  ntCreateSectionType @ [extParams; dword]

let ntMapViewOfSectionType =
  // Note that we'll consider the returned void pointer as an 8-byte integer.
  [
    handle; // HANDLE SectionHandle
    handle; // HANDLE ProcessHandle
    archWordPtrInOut; // PVOID * BaseAddress
    ulongPtr; // ULONG_PTR ZeroBits
    archWord; // SIZE_T CommitSize
    Ptr (largeInt, InOut); // PLARGE_INTEGER SectionOffset
    pSizeInOut; // PSIZE_T ViewSize
    sectionInherit; // SECTION_INHERIT InheritDisposition
    dword; // ULONG AllocationType
    dword; // ULONG Win32Protect
  ]

let ntMapViewOfSectionExType =
  [
    handle; // HANDLE SectionHandle
    handle; // HANDLE ProcessHandle
    archWordPtrInOut; // PVOID * BaseAddress
    Ptr (largeInt, InOut); // PLARGE_INTEGER SectionOffset
    pSizeInOut; // PSIZE_T ViewSize
    dword; // ULONG AllocationType
    dword; // ULONG Win32Protect
    Ptr (Array (CountSize, ArgField (8, []), memExtParam), InOut); // Params.
    dword; // Param count.
  ]

let ntUnmapViewOfSectionType =
  // We'll consider the void pointer to unmap as an architecture word.
  [handle; archWord]

let ntUnmapViewOfSectionExType =
  ntUnmapViewOfSectionType @ [dword]

// Encoded syscalls are not analyzed, so manually fill in. Note that we omit the
// blacklisted syscalls, since they are not hooked anyway.
let ENCODED_SYSCALL_TYPES =
  [
    ("ntdll!NtCreateSection", ntCreateSectionType);
    ("ntdll!NtCreateSectionEx", ntCreateSectionExType);
    ("ntdll!NtMapViewOfSection", ntMapViewOfSectionType);
    ("ntdll!NtMapViewOfSectionEx", ntMapViewOfSectionExType);
    ("ntdll!NtUnmapViewOfSection", ntUnmapViewOfSectionType);
    ("ntdll!NtUnmapViewOfSectionEx", ntUnmapViewOfSectionExType);
  ]
  |> Map.ofList
