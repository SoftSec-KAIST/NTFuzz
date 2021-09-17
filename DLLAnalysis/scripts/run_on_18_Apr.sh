#!/bin/bash
SCRIPTFILE=$(readlink -f "$0")
SCRIPTDIR=$(dirname "$SCRIPTFILE")
BASEDIR=$SCRIPTDIR/..
OUTDIR=$BASEDIR/output
LOGDIR=$BASEDIR/log

mkdir -p $OUTDIR
mkdir -p $LOGDIR

# You can add "--distort N" option for the ablation expreiment.
dotnet $BASEDIR/build/DLLAnalysis.dll type \
  --verbose 1 --mode all \
  -b $BASEDIR/binaries/17134.1/ntdll.dll \
     $BASEDIR/binaries/17134.1/kernelbase.dll \
     $BASEDIR/binaries/17134.1/kernel32.dll \
     $BASEDIR/binaries/17134.1/win32u.dll \
     $BASEDIR/binaries/17134.1/gdi32.dll \
     $BASEDIR/binaries/17134.1/gdi32full.dll \
     $BASEDIR/binaries/17134.1/user32.dll \
  -o $OUTDIR/Types_18_Apr.json \
  > $LOGDIR/log_type_18_Apr.txt 2>&1

dotnet $BASEDIR/build/DLLAnalysis.dll code \
  --dlls $BASEDIR/binaries/17134.1/ntdll.dll \
         $BASEDIR/binaries/17134.1/win32u.dll \
  --ntoskrnl $BASEDIR/binaries/17134.1/ntoskrnl.exe \
             $BASEDIR/binaries/17134.1/ntkrpamp.pdb \
  --hookerfile $OUTDIR/GeneralHooker_18_Apr.cpp \
  --constfile $OUTDIR/VersionConst_18_Apr.h \
  > $LOGDIR/log_code_18_Apr.txt 2>&1
