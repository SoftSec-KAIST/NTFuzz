namespace WinHeader.C

open WinHeader.Constants
open WinHeader.Utils

exception InterfaceException

type CType =
  | TypeName of string
  | Void
  | CFunc
  | Handle
  | D3Handle
  | Char
  | WChar
  | Byte
  | Word
  | DWord
  | QWord
  | IntPtr // Declared as int (for arithematic), but the underlying is pointer.
  | CStrPtrA
  | CStrPtrW
  | CPtr of CType
  | CStruct of Field list
  | CUnion of Field list
  | CEnum

and Field = {
  FieldType : CType
  FieldName : string
  MainSizeAnnot : SizeAnnot
  AuxSizeAnnot : SizeAnnot
  AstrCount : int
  ArrSize : SizeExp option
  IsBitField : bool
}

module CType =

  let rec toString = function
    | TypeName name -> sprintf "%s" name
    | Void -> "void"
    | CFunc -> "Function"
    | Handle -> "HANDLE"
    | D3Handle -> "D3_HANDLE"
    | Char -> "CHAR"
    | WChar -> "WCHAR"
    | Byte -> "BYTE"
    | Word -> "WORD"
    | DWord -> "DWORD"
    | QWord -> "QWORD"
    | IntPtr -> "INTEGER_PTR"
    | CStrPtrA -> "StringA"
    | CStrPtrW -> "StringW"
    | CPtr cType -> sprintf "%s*" (toString cType)
    | CStruct fields ->
      let fieldStrs = List.map fieldToString fields
      let fStr = String.concat "  " fieldStrs
      sprintf "struct {\n  %s}" fStr
    | CUnion fields ->
      let fieldStrs = List.map fieldToString fields
      let fStr = String.concat " " fieldStrs
      sprintf "union {\n  %s}" fStr
    | CEnum -> "enum {...}"

  and fieldToString fld =
    let typStr = match fld.FieldType with // simplify for nested case.
                 | CStruct _ -> "struct { ...} "
                 | CUnion _ -> "union { ...} "
                 | t -> toString t
    let ptrStr = String.replicate fld.AstrCount "*"
    let arrStr = match fld.ArrSize with
                 | None -> ""
                 | Some size -> sprintf "[%s]" (SizeExp.toString size)
    sprintf "%s%s %s%s;\n" typStr ptrStr fld.FieldName arrStr

  let rec appendPtrNtimes n typ =
    if n <= 0 then typ else appendPtrNtimes (n - 1) (CPtr typ)

  let promotePtrToStr mainAnnot auxAnnot typ =
    let isConstAnnot = SizeAnnot.isConst mainAnnot || SizeAnnot.isConst auxAnnot
    match typ with
    // Assume character pointer as string unless the size is given as constant.
    | CPtr WChar when not isConstAnnot -> CStrPtrW
    | CPtr Char when not isConstAnnot -> CStrPtrA
    | _ -> typ

  let isPtr = function
    | TypeName _ | Void | CFunc | Handle | D3Handle
    | Char | WChar | Byte | Word | DWord | QWord | IntPtr -> false
    | CStrPtrA | CStrPtrW | CPtr _ -> true
    | CStruct _ | CUnion _ | CEnum -> false

  let isSinglePtr = function
    | TypeName _ | Void | CFunc | Handle | D3Handle
    | Char | WChar | Byte | Word | DWord | QWord | IntPtr -> false
    | CStrPtrA | CStrPtrW -> true
    | CPtr typ -> not (isPtr typ)
    | CStruct _ | CUnion _ | CEnum -> false

  let isBitFields fields =
    List.exists (fun fld -> fld.IsBitField) fields

  let rec alignOf cType =
    match cType with
    | TypeName _ -> failwith "Invalid alignOf(TypeName)"
    | Void -> failwith "Invalid alignOf(Void)"
    | CFunc -> failwith "Invalid alignOf(CFunc)"
    | Handle | D3Handle | Char | WChar | Byte | Word | DWord | QWord
    | IntPtr | CStrPtrA | CStrPtrW | CPtr _ | CEnum -> sizeOf cType
    | CStruct fields when isBitFields fields ->
      // For bit field, the alignment of any element is the alignment of struct.
      alignOf (List.head fields).FieldType
    | CStruct fields | CUnion fields ->
      let fieldAligns = List.map alignOfField fields
      List.max fieldAligns

   and alignOfField field =
    let fieldType = appendPtrNtimes field.AstrCount field.FieldType
    alignOf fieldType

  and sizeOf cType =
    match cType with
    | TypeName _ -> failwith "Invalid sizeOf(TypeName)"
    | Void -> failwith "Invalid sizeOf(Void)"
    | CFunc -> failwith "Invalid sizeOf(CFunc)"
    | Handle -> WORDSIZE // Handle is defined as a void pointer in Windows.
    | D3Handle -> 4UL // Handle is defined as a void pointer in Windows.
    | Char -> 1UL
    | WChar -> 2UL
    | Byte -> 1UL
    | Word -> 2UL
    | DWord -> 4UL
    | QWord -> 8UL
    | IntPtr -> WORDSIZE
    | CStrPtrA -> WORDSIZE // String pointer is also a pointer.
    | CStrPtrW -> WORDSIZE // String pointer is also a pointer.
    | CPtr _ -> WORDSIZE
    | CStruct fields when isBitFields fields ->
      // For bit field, the size of any element is the size of struct.
      sizeOf (List.head fields).FieldType
    | CStruct fields -> // Add the size of fields, while considering alignment.
      let align = alignOf cType
      // Note that we need another aligning after the summation of field sizes.
      alignUp (List.fold addFieldSize 0UL fields) align
    | CUnion fields -> // Find the size of largest field.
      let align = alignOf cType
      let fieldSizes = List.map sizeOfField fields
      // Note that we need another aligning after choosing the maximum size.
      alignUp (List.max fieldSizes) align
    | CEnum -> 4UL // enum is implemented with 'int' type.

  and sizeOfField field =
    let fieldType = appendPtrNtimes field.AstrCount field.FieldType
    let fieldTypeSize = sizeOf fieldType
    let arrSize = match field.ArrSize with
                  | None -> 1UL
                  | Some size -> SizeExp.tryEval size
    arrSize * fieldTypeSize

  and private addFieldSize accSize field =
    let align = alignOfField field
    let size = sizeOfField field
    (alignUp accSize align) + size

module Field =
  // Just for function aliases.
  let isBitFields = CType.isBitFields
  let toString = CType.fieldToString
  let sizeOf = CType.sizeOfField
  let alignOf = CType.alignOfField

/// Type names used in 'typedef' syntax.
type DefName =
  | Name of string
  | PtrName of string

module DefName =
  let toString = function
    | Name s -> s
    | PtrName s -> sprintf "*%s" s

  let isMatching typName = function
    | Name s -> typName = s
    | PtrName s -> typName = s

/// This represents 'typedef A B, *PB;', for example.
type Typedef = {
  Names : DefName list
  Type : CType
}

module Typedef =
  let toString typDef =
    let tStr = CType.toString typDef.Type
    let nameStrs = List.map DefName.toString typDef.Names
    let nStr = String.concat ", " nameStrs
    sprintf "typedef %s %s;" tStr nStr

  /// In typedef syntax, multiple types ared declared at once (for example,
  /// 'typedef A B, *PB;'. After extracting and parsing this typedef statement,
  /// we should retrieve the matching type for the requested type name.
  let retrieveMatchingType typName typDef =
    match List.tryFind (DefName.isMatching typName) typDef.Names with
    | None -> failwith "No matching name from typedef (parsed wrong text)"
    | Some (Name _) -> typDef.Type
    | Some (PtrName _) -> CPtr typDef.Type
