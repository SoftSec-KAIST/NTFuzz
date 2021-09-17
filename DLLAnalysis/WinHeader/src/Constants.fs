module WinHeader.Constants

open WinHeader.Utils

let WORDSIZE = 4UL // x86

let CTAGS_FILENAME = "ctags.txt"

let wildcardMacros =
  [ "ANYSIZE_ARRAY" ]

// TODO. Automatically extract macros from header in the future.

let macros =
  [
    "CCHDEVICENAME", 32UL;
    "CCHFORMNAME", 32UL;
    "SIZE_OF_80387_REGISTERS", 80UL;
    "MAX_PATH", 260UL;
    "LF_FACESIZE", 32UL;
    "LF_FULLFACESIZE", 64UL;
    "OUTPUTDUPL_CREATE_MAX_KEYEDMUTXES", 3UL;
    "MAX_ENUM_ADAPTERS", 16UL;
    "D3DKMT_MAX_TRACKED_WORKLOAD_INSTANCES", 32UL;
    "D3DDDI_MAX_BROADCAST_CONTEXT", 64UL;
    "D3DDDI_MAX_WRITTEN_PRIMARIES", 16UL;
    "D3DDDI_MAX_OBJECT_SIGNALED", 32UL;
    "D3DDDI_MAX_OBJECT_WAITED_ON", 32UL;
    "D3DKMT_MAX_WAITFORVERTICALBLANK_OBJECTS", 8UL;
    "MAXIMUM_SUPPORTED_EXTENSION", 512UL ;
    "MM_MAX_NUMAXES", 16UL;
    "OFS_MAXPATHNAME", 128UL;
    "MAX_MODULE_NAME32", 255UL;
    "EXCEPTION_MAXIMUM_PARAMETERS", 15UL;
    "UNWIND_HISTORY_TABLE_SIZE", 12UL;
    "WOW64_SIZE_OF_80387_REGISTERS", 80UL;
    "WOW64_MAXIMUM_SUPPORTED_EXTENSION", 512UL;
    "MAX_DEFAULTCHAR", 2UL;
    "MAX_LEADBYTES", 12UL;
    "POINTER_DEVICE_PRODUCT_STRING_MAX", 520UL;
    "CCHILDREN_TITLEBAR", 5UL;
    "CCHILDREN_SCROLLBAR", 5UL;
    "KL_NAMELENGTH", 9UL;
    "GAMMARAMP_SIZE", 1536UL;
    "ENTRY_SIZE", 24UL;
    "CRED_MAX_DOMAIN_TARGET_NAME_LENGTH", 337UL;
    "CRED_MAX_STRING_LENGTH", 256UL;
    "CRED_MAX_GENERIC_TARGET_NAME_LENGTH", 32767UL;
    "CRED_MAX_USERNAME_LENGTH", 513UL;
  ]
  |> Map.ofList

let blacklist =
  [
    "Windows.";
    "ChString";
    "gdiplusheaders";
  ]

let skipArgClauseKeyword =
  [
    "_Inexpressible_";
  ]

let skipClauseKeyword =
  [
    "_When_";
    "_Always_";
    "_At_";
    "_In_range_";
    "_Out_range_";
    "_Deref_in_range_";
    "_Deref_out_range_";
    "__in_data_source";
    "__out_data_source";
    "__in_validated";
    "__out_validated";
    "_Return_type_success_";
    "_Field_range_";
    "_Function_class_";
    "__out_post_ecount_opt";
    "__out_post_bcount_opt";
  ]

let private matchKeyword idx (str: string) keyword =
  let key_len = String.length keyword
  idx + key_len - 1 < str.Length && str.[idx .. idx + key_len - 1] = keyword

let isSkipClause idx str =
  List.exists (matchKeyword idx str) skipClauseKeyword

let isSkipArgClause idx str =
  List.exists (matchKeyword idx str) skipArgClauseKeyword

let skipClause (str: string) idx =
  let keyword = List.find (matchKeyword idx str) skipClauseKeyword
  let keyLen = String.length keyword
  let remainStr = str.[idx + keyLen .. ]
  let parenCloseIdx = findParenCloseIdx remainStr
  if parenCloseIdx = -1 then failwithf "Invalid %s syntax" keyword
  idx + keyLen + parenCloseIdx + 1

let skipArgClause (str: string) idx =
  let keyword = List.find (matchKeyword idx str) skipArgClauseKeyword
  let keyLen = String.length keyword
  let remainStr = str.[idx + keyLen .. ]
  let parenCloseIdx = findParenCloseIdx remainStr
  if parenCloseIdx = -1 then failwithf "Invalid %s syntax" keyword
  (keyword, idx + keyLen + parenCloseIdx + 1)
