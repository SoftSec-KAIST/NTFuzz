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
  -b $BASEDIR/binaries/18362.592/ntdll.dll \
     $BASEDIR/binaries/18362.592/kernelbase.dll \
     $BASEDIR/binaries/18362.592/kernel32.dll \
     $BASEDIR/binaries/18362.592/win32u.dll \
     $BASEDIR/binaries/18362.592/gdi32.dll \
     $BASEDIR/binaries/18362.592/gdi32full.dll \
     $BASEDIR/binaries/18362.592/user32.dll \
     $BASEDIR/binaries/18362.592/dxcore.dll \
  -o $OUTDIR/Types_20_Jan.json \
  > $LOGDIR/log_type_20_Jan.txt 2>&1

dotnet $BASEDIR/build/DLLAnalysis.dll code \
  --dlls $BASEDIR/binaries/18362.592/ntdll.dll \
         $BASEDIR/binaries/18362.592/win32u.dll \
  --ntoskrnl $BASEDIR/binaries/18362.592/ntoskrnl.exe \
             $BASEDIR/binaries/18362.592/ntkrpamp.pdb \
  --hookerfile $OUTDIR/GeneralHooker_20_Jan.cpp \
  --constfile $OUTDIR/VersionConst_20_Jan.h \
  > $LOGDIR/log_code_20_Jan.txt 2>&1
