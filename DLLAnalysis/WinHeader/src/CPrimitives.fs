module WinHeader.C.Primitives

open WinHeader.C

// https://docs.microsoft.com/en-us/windows/win32/winprog/windows-data-types

let lenField = {
  FieldType = Word
  FieldName = "Length"
  MainSizeAnnot = NoSizeAnnot
  AuxSizeAnnot = NoSizeAnnot
  AstrCount = 0
  ArrSize = None
  IsBitField = false
}

let maxLenField = { lenField with FieldName = "MaximumLength" }

let bufField = {
  FieldType = CStrPtrW
  FieldName = "Buffer"
  MainSizeAnnot = NoSizeAnnot
  AuxSizeAnnot = NoSizeAnnot
  AstrCount = 0
  ArrSize = None
  IsBitField = false
}

let unicodeString =
  CStruct [lenField; maxLenField; bufField]

let archWord = DWord // x86

let types =
  [|
    "void", Void;
    // https://docs.microsoft.com/en-us/cpp/cpp/data-type-ranges
    "int", DWord;
    "__int8", Byte;
    "__int16", Word;
    "__int32", DWord;
    "__int64", QWord;
    "bool", Byte;
    "char", Char;
    "short", Word;
    "long", DWord; // Caution. In Windows, 'long' is 32bit even in 64bit OS.
    "float", DWord;
    "double", QWord;
    "wchar_t", WChar;

    "ATOM", Word;
    "BOOL", DWord;
    "BOOLEAN", Byte;
    "BYTE", Byte;
    "CCHAR", Char;
    "CHAR", Char;
    "COLORREF", DWord;
    "DWORD", DWord;
    "DWORDLONG", QWord;
    "DWORD_PTR", CPtr Void;
    "DWORD32", DWord;
    "DWORD64", QWord;
    "FLOAT", DWord;
    "HACCEL", Handle;
    "HALF_PTR", DWord;
    "HANDLE", Handle;
    "HBITMAP", Handle;
    "HBRUSH", Handle;
    "HCOLORSPACE", Handle;
    "HCONV", Handle;
    "HCONVLIST", Handle;
    "HCURSOR", Handle;
    "HDC", Handle;
    "HDDEDATA", Handle;
    "HDESK", Handle;
    "HDROP", Handle;
    "HDWP", Handle;
    "HENHMETAFILE", Handle;
    "HFILE", Handle;
    "HFONT", Handle;
    "HGDIOBJ", Handle;
    "HGLOBAL", Handle;
    "HHOOK", Handle;
    "HICON", Handle;
    "HINSTANCE", Handle;
    "HKEY", Handle;
    "HKL", Handle;
    "HLOCAL", Handle;
    "HMENU", Handle;
    "HMETAFILE", Handle;
    "HMODULE", Handle;
    "HMONITOR", Handle;
    "HPALETTE", Handle;
    "HPEN", Handle;
    "HRESULT", Handle;
    "HRGN", Handle;
    "HRSRC", Handle;
    "HSZ", Handle;
    "HWINSTA", Handle;
    "HWND", Handle;
    "INT", DWord;
    "INT_PTR", IntPtr;
    "INT8", Byte;
    "INT16", Word;
    "INT32", DWord;
    "INT64", QWord;
    "LANGID", Word;
    "LCID", DWord;
    "LCTYPE", DWord;
    "LGRPID", DWord;
    "LONG", DWord; // Caution. In Windows, 'long' is 32bit even in 64bit OS.
    "LONGLONG", QWord;
    "LONG_PTR", IntPtr;
    "LONG32", DWord;
    "LONG64", QWord;
    "LPARAM", CPtr archWord;
    "LPBOOL", CPtr DWord;
    "LPBYTE", CPtr Byte;
    "LPCOLORREF", CPtr DWord;
    "LPCSTR", CStrPtrA;
    "LPCTSTR", CStrPtrW;
    "LPCVOID", CPtr Void;
    "LPCWSTR", CStrPtrW;
    "LPDWORD", CPtr DWord;
    "LPHANDLE", CPtr Handle;
    "LPINT", CPtr DWord;
    "LPLONG", CPtr DWord;
    "LPSTR", CStrPtrA;
    "LPTSTR", CStrPtrW;
    "LPVOID", CPtr Void;
    "LPWORD", CPtr Word;
    "LPWSTR", CStrPtrW;
    "LRESULT", CPtr archWord;
    "PBOOL", CPtr DWord;
    "PBOOLEAN", CPtr Byte;
    "PBYTE", CPtr Byte;
    "PCHAR", CPtr Byte;
    "PCSTR", CStrPtrA;
    "PCTSTR", CStrPtrW;
    "PCWSTR", CStrPtrW;
    "PDWORD", CPtr DWord;
    "PDWORDLONG", CPtr QWord;
    "PDWORD_PTR", CPtr IntPtr;
    "PDWORD32", CPtr DWord;
    "PDWORD64", CPtr QWord;
    "PFLOAT", CPtr DWord;
    "PHALF_PTR", CPtr DWord;
    "PHANDLE", CPtr Handle;
    "PHKEY", CPtr Handle;
    "PINT", CPtr DWord;
    "PINT_PTR", CPtr IntPtr;
    "PINT8", CPtr Byte;
    "PINT16", CPtr Word;
    "PINT32", CPtr DWord;
    "PINT64", CPtr QWord;
    "PLCID", CPtr DWord;
    "PLONG", CPtr DWord;
    "PLONGLONG", CPtr QWord;
    "PLONG_PTR", CPtr IntPtr;
    "PLONG32", CPtr DWord;
    "PLONG64", CPtr QWord;
    "POINTER_32", DWord;
    "POINTER_64", CPtr Void;
    "PSHORT", CPtr Word;
    "PSIZE_T", CPtr archWord;
    "PSSIZE_T", CPtr archWord;
    "PSTR", CStrPtrA;
    "PTBYTE", CPtr Word;
    "PTCHAR", CStrPtrW;
    "PTSTR", CStrPtrW;
    "PUCHAR", CPtr Byte; // Caution: 'U' stands for unsigned, not unicode.
    "PUHALF_PTR", CPtr DWord;
    "PUINT", CPtr DWord;
    "PUINT_PTR", CPtr IntPtr;
    "PUINT8", CPtr Byte;
    "PUINT16", CPtr Word;
    "PUINT32", CPtr DWord;
    "PUINT64", CPtr QWord;
    "PULONG", CPtr DWord;
    "PULONGLONG", CPtr QWord;
    "PULONG_PTR", CPtr IntPtr;
    "PULONG32", CPtr DWord;
    "PULONG64", CPtr QWord;
    "PUSHORT", CPtr Word;
    "PVOID", CPtr Void;
    "PWCHAR", CStrPtrW;
    "PWORD", CPtr Word;
    "PWSTR", CStrPtrW;
    "QWORD", QWord;
    "SC_HANDLE", Handle;
    "SC_LOCK", Handle;
    "SERVICE_STATUS_HANDLE", Handle;
    "SHORT", Word;
    "SIZE_T", archWord;
    "SSIZE_T", archWord;
    "TBYTE", Word;
    "TCHAR", WChar;
    "UCHAR", Char; // Caution: 'U' stands for unsigned, not unicode.
    "UHALF_PTR", DWord;
    "UINT", DWord;
    "UINT_PTR", IntPtr;
    "UINT8", Byte;
    "UINT16", Word;
    "UINT32", DWord;
    "UINT64", QWord;
    "ULONG", DWord;
    "ULONGLONG", QWord;
    "ULONG_PTR", IntPtr;
    "ULONG32", DWord;
    "ULONG64", QWord;
    "UNICODE_STRING", unicodeString;
    "PUNICODE_STRING", CPtr unicodeString;
    "USHORT", Word;
    "USN", QWord;
    "VOID", Void;
    "WCHAR", WChar;
    "WORD", Word;
    "WPARAM", CPtr Void;

    // Handle types obtained by "grep DECLARE_HANDLE *.h"

    "AUTHZ_ACCESS_CHECK_RESULTS_HANDLE", Handle;
    "AUTHZ_CLIENT_CONTEXT_HANDLE", Handle;
    "AUTHZ_RESOURCE_MANAGER_HANDLE", Handle;
    "AUTHZ_AUDIT_EVENT_HANDLE", Handle;
    "AUTHZ_AUDIT_EVENT_TYPE_HANDLE", Handle;
    "AUTHZ_SECURITY_EVENT_PROVIDER_HANDLE", Handle;
    "AUTHZ_CAP_CHANGE_SUBSCRIPTION_HANDLE", Handle;
    "FH_SERVICE_PIPE_HANDLE", Handle;
    "IDDCX_ADAPTER", Handle;
    "IDDCX_MONITOR", Handle;
    "IDDCX_SWAPCHAIN", Handle;
    "IDDCX_OPMCTX", Handle;
    "IDDCX_ADAPTER", Handle;
    "IDDCX_MONITOR", Handle;
    "IDDCX_SWAPCHAIN", Handle;
    "IDDCX_OPMCTX", Handle;
    "IDDCX_ADAPTER", Handle;
    "IDDCX_MONITOR", Handle;
    "IDDCX_SWAPCHAIN", Handle;
    "IDDCX_OPMCTX", Handle;
    "IDDCX_ADAPTER", Handle;
    "IDDCX_MONITOR", Handle;
    "IDDCX_SWAPCHAIN", Handle;
    "IDDCX_OPMCTX", Handle;
    "HINTERACTIONCONTEXT", Handle;
    "LPM_HANDLE", Handle;
    "RHANDLE", Handle;
    "HACMDRIVERID", Handle;
    "HACMDRIVER", Handle;
    "HACMSTREAM", Handle;
    "HACMOBJ", Handle;
    "HPSS", Handle;
    "HPSSWALK", Handle;
    "HRASCONN", Handle;
    "HPSXA", Handle;
    "HUIANODE", Handle;
    "HUIAPATTERNOBJECT", Handle;
    "HUIATEXTRANGE", Handle;
    "HUIAEVENT", Handle;
    "UCMCONNECTOR", Handle;
    "UCMCONNECTOR", Handle;
    "UCMTCPCIPORTCONTROLLER", Handle;
    "UCMUCSIPPM", Handle;
    "UCMUCSI_CONNECTOR_COLLECTION", Handle;
    "UDECXUSBDEVICE", Handle;
    "UDECXUSBENDPOINT", Handle;
    "UDECXUSBDEVICE", Handle;
    "UDECXUSBENDPOINT", Handle;
    "URSIORESLIST", Handle;
    "HIC", Handle;
    "HVIDEO", Handle;
    "HTOUCHINPUT", Handle;
    "HSYNTHETICPOINTERDEVICE", Handle;
    "HRAWINPUT", Handle;
    "HGESTUREINFO", Handle;
    "HTTP_PUSH_WAIT_HANDLE", Handle;
    "HAMSICONTEXT", Handle;
    "HAMSISESSION", Handle;
    "HCMNOTIFICATION", Handle;
    "CO_MTA_USAGE_COOKIE", Handle;
    "COMPRESSOR_HANDLE", Handle;
    "HCS_SYSTEM", Handle;
    "HCS_PROCESS", Handle;
    "HCS_OPERATION", Handle;
    "HCS_CALLBACK", Handle;
    "D3DKMDT_HVIDPN", Handle;
    "D3DKMDT_HVIDEOPRESENTSOURCESET", Handle;
    "D3DKMDT_HVIDEOPRESENTTARGETSET", Handle;
    "D3DKMDT_HVIDPNTOPOLOGY", Handle;
    "D3DKMDT_HVIDPNSOURCEMODESET", Handle;
    "D3DKMDT_HVIDPNTARGETMODESET", Handle;
    "D3DKMDT_HMONITORSOURCEMODESET", Handle;
    "D3DKMDT_HMONITORFREQUENCYRANGESET", Handle;
    "D3DKMDT_HMONITORDESCRIPTORSET", Handle;
    "HWINWATCH", Handle;
    "HCONVLIST", Handle;
    "HCONV", Handle;
    "HSZ", Handle;
    "HDDEDATA", Handle;
    "HDEVQUERY", Handle;
    "DXGK_DEBUG_REPORT_HANDLE", Handle;
    "FEATURE_STATE_CHANGE_SUBSCRIPTION", Handle;
    "HSTRING_BUFFER", Handle;
    "HIMC", Handle;
    "HIMCC", Handle;
    "HIFTIMESTAMPCHANGE", Handle;
    "MBBREQUEST", Handle;
    "HKEY", Handle;
    "HMETAFILE", Handle;
    "HRGN", Handle;
    "HRSRC", Handle;
    "HSPRITE", Handle;
    "HLSURF", Handle;
    "HSTR", Handle;
    "HTASK", Handle;
    "HWINSTA", Handle;
    "HKL", Handle;
    "HWAVE", Handle;
    "HWAVEIN", Handle;
    "HWAVEOUT", Handle;
    "HMIDI", Handle;
    "HMIDIIN", Handle;
    "HMIDIOUT", Handle;
    "HMIDISTRM", Handle;
    "HMIXEROBJ", Handle;
    "HMIXER", Handle;
    "HMMIO", Handle;
    "HCOMDB", Handle;
    "NCRYPT_DESCRIPTOR_HANDLE", Handle;
    "NCRYPT_STREAM_HANDLE", Handle;
    "NETADAPTER", Handle;
    "NETCONFIGURATION", Handle;
    "NETREQUEST", Handle;
    "NETREQUESTQUEUE", Handle;
    "NETOFFLOAD", Handle;
    "NETPACKETQUEUE", Handle;
    "NETPOWERSETTINGS", Handle;
    "PEPHANDLE", Handle;
    "PEPHANDLE", Handle;
    "PCSTREAMRESOURCE", Handle;
    "HPTPROVIDER", Handle;
    "PRJ_NAMESPACE_VIRTUALIZATION_CONTEXT", Handle;
    "PRJ_DIR_ENTRY_BUFFER_HANDLE", Handle;
    "HRECOALT", Handle;
    "HRECOCONTEXT", Handle;
    "HRECOGNIZER", Handle;
    "HRECOLATTICE", Handle;
    "HRECOWORDLIST", Handle;
    "HRECOALT", Handle;
    "HRECOCONTEXT", Handle;
    "HRECOGNIZER", Handle;
    "HRECOLATTICE", Handle;
    "HRECOWORDLIST", Handle;
    "APARTMENT_SHUTDOWN_REGISTRATION_COOKIE", Handle;
    "ROPARAMIIDHANDLE", Handle;
    "SPSTATEHANDLE", Handle;
    "SPSTATEHANDLE", Handle;
    "SPWORDHANDLE", Handle;
    "SPRULEHANDLE", Handle;
    "SPGRAMMARHANDLE", Handle;
    "SPRECOCONTEXTHANDLE", Handle;
    "SPPHRASERULEHANDLE", Handle;
    "SPPHRASEPROPERTYHANDLE", Handle;
    "SPTRANSITIONID", Handle;
    "SERCX2SYSTEMDMATRANSMIT", Handle;
    "SERCX2SYSTEMDMARECEIVE", Handle;
    "SERCX2PIOTRANSMIT", Handle;
    "SERCX2PIORECEIVE", Handle;
    "SERCX2CUSTOMRECEIVE", Handle;
    "SERCX2CUSTOMTRANSMIT", Handle;
    "SERCX2CUSTOMRECEIVETRANSACTION", Handle;
    "SERCX2CUSTOMTRANSMITTRANSACTION", Handle;
    "HDROP", Handle;
    "HSWDEVICE", Handle;
    "UCXCONTROLLER", Handle;
    "UCXROOTHUB", Handle;
    "UCXUSBDEVICE", Handle;
    "UCXENDPOINT", Handle;
    "UCXSSTREAMS", Handle;
    "UCXCONTROLLER", Handle;
    "UCXROOTHUB", Handle;
    "UCXUSBDEVICE", Handle;
    "UCXENDPOINT", Handle;
    "UCXSSTREAMS", Handle;
    "UCXCONTROLLER", Handle;
    "UCXROOTHUB", Handle;
    "UCXUSBDEVICE", Handle;
    "UCXENDPOINT", Handle;
    "UCXSSTREAMS", Handle;
    "UCXCONTROLLER", Handle;
    "UCXROOTHUB", Handle;
    "UCXUSBDEVICE", Handle;
    "UCXENDPOINT", Handle;
    "UCXSSTREAMS", Handle;
    "UCXCONTROLLER", Handle;
    "UCXROOTHUB", Handle;
    "UCXUSBDEVICE", Handle;
    "UCXENDPOINT", Handle;
    "UCXSSTREAMS", Handle;
    "UFXDEVICE", Handle;
    "UFXENDPOINT", Handle;
    "USBD_HANDLE", Handle;
    "USBDI_HANDLE", Handle;
    "USBD_FUNCTION_HANDLE", Handle;
    "USB_CHANGE_REGISTRATION_HANDLE", Handle;
    "USBPM_CLIENT", Handle;
    "USBPM_HUB", Handle;
    "USBPM_CONNECTOR", Handle;
    "USBPM_CLIENT", Handle;
    "USBPM_HUB", Handle;
    "USBPM_CONNECTOR", Handle;
    "VMBCHANNEL", Handle;
    "VMBPACKET", Handle;
    "VMBPACKETCOMPLETION", Handle;
    "POHANDLE", Handle;
    "WEB_SOCKET_HANDLE", Handle;
    "HBM", Handle;
    "HDEV", Handle;
    "HSURF", Handle;
    "DHSURF", Handle;
    "DHPDEV", Handle;
    "HDRVOBJ", Handle;
    "HSEMAPHORE", Handle;
    "HFASTMUTEX", Handle;
    "HWND", Handle;
    "HHOOK", Handle;
    "HEVENT", Handle;
    "HACCEL", Handle;
    "HBITMAP", Handle;
    "HBRUSH", Handle;
    "HCOLORSPACE", Handle;
    "HDC", Handle;
    "HGLRC", Handle;
    "HDESK", Handle;
    "HENHMETAFILE", Handle;
    "HFONT", Handle;
    "HICON", Handle;
    "HMENU", Handle;
    "HPALETTE", Handle;
    "HPEN", Handle;
    "HWINEVENTHOOK", Handle;
    "HMONITOR", Handle;
    "HUMPD", Handle;
    "DPI_AWARENESS_CONTEXT", Handle;
    "SAFER_LEVEL_HANDLE", Handle;
    "SC_HANDLE", Handle;
    "SERVICE_STATUS_HANDLE", Handle;
    "RECORDER_LOG", Handle;
    "WPP_RECORDER_COUNTER", Handle;

    // Direct3 family handle is 4-byte, so handle separately.
    "D3DKMT_HANDLE", D3Handle;

    // Add-hoc types that we manually identified (decided not to parse).
    "IUnknown", CPtr Void;
    "va_list", CPtr Void;

    // Undefined types (from syscall prototype).
    "UNIVERSAL_FONT_ID", archWord;
    "PUNIVERSAL_FONT_ID", CPtr archWord;
    "ARCTYPE", archWord;
    "KERNEL_PVOID", CPtr Void;
    "PCHWIDTHINFO", CPtr archWord;
    "DPI_INFORMATION", archWord;
    "FONT_FILE_INFO", archWord;
    "TMDIFF", archWord;
    "PFONT_REALIZATION_INFO2", archWord;
    "TMW_INTERNAL", archWord;
    "WIDTHDATA", archWord;
    "HLSURF_INFORMATION_CLASS", DWord;
    "LFTYPE", archWord;
    "_POLYPATBLT", archWord;
    "_POLYPATBLT", archWord;

    // Declared as int, but let's capture the subtleness.
    "uintptr_t", IntPtr;
    "intptr_t", IntPtr;

    // opaque types.
    "_TP_POOL", archWord;
    "_TP_CLEANUP_GROUP", archWord;
    "_TP_CALLBACK_INSTANCE", archWord;
    "_ACTIVATION_CONTEXT", archWord;
    "_PROC_THREAD_ATTRIBUTE_LIST", archWord;

    // To remove confusion when described as a struct of two DWords.
    "LARGE_INTEGER", QWord;

  |] |> Map.ofArray
