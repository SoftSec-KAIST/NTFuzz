#!/bin/bash
SCRIPTFILE=$(readlink -f "$0")
SCRIPTDIR=$(dirname "$SCRIPTFILE")
BASEDIR=$SCRIPTDIR/..
OUTDIR=$BASEDIR/output
LOGDIR=$BASEDIR/log

mkdir -p $OUTDIR
mkdir -p $LOGDIR
dotnet $BASEDIR/build/DLLAnalysis.dll type \
  --verbose 1 --mode eval \
  -b $BASEDIR/binaries/17134.1/ntdll.dll \
     $BASEDIR/binaries/17134.1/kernelbase.dll \
     $BASEDIR/binaries/17134.1/kernel32.dll \
     $BASEDIR/binaries/17134.1/win32u.dll \
     $BASEDIR/binaries/17134.1/gdi32.dll \
     $BASEDIR/binaries/17134.1/gdi32full.dll \
     $BASEDIR/binaries/17134.1/user32.dll \
  -o $OUTDIR/Types_eval.json \
  > $LOGDIR/log_eval.txt 2>&1
