#!/bin/bash
SCRIPTFILE=$(readlink -f "$0")
SCRIPTDIR=$(dirname "$SCRIPTFILE")
BASEDIR=$SCRIPTDIR/..
OUTDIR=$BASEDIR/output
LOGDIR=$BASEDIR/log

mkdir -p $OUTDIR
mkdir -p $LOGDIR
dotnet $BASEDIR/build/DLLAnalysis.dll type \
  --verbose 1 --mode dummy \
  -b $BASEDIR/binaries/17134.1/ntdll.dll \
     $BASEDIR/binaries/17134.1/win32u.dll \
  -o $OUTDIR/Types_dummy.json \
  > $LOGDIR/log_dummy.txt 2>&1
