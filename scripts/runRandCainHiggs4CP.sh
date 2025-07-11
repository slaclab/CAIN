#!/bin/bash

set -e    # exit script on first error


cd $CA


# Usage example:  ./runCain.sh v00136 LinearQED_C3_x1000_po0p90_lpc+
JOBID=$1
COMPTON=$2

JOBDIR="$JOBID"_"$COMPTON"




rm -fR out/"$JOBDIR"

mkdir -p out/"$JOBDIR"

exec > $CA/out/"$JOBDIR"/runCain_"$JOBDIR".txt
exec 2>&1


RANDNC=$(od -An -N3 -i /dev/random | xargs)


echo " JOBID= " $JOBID   " COMPTON= " $COMPTON   " JOBDIR= " $JOBDIR  " RANDNC= " $RANDNC


cd $CA/out/"$JOBDIR"

cp $CA/in/Higgs4CP_"$COMPTON".i Higgs4CP_"$JOBDIR".i
sed -i "s!^ *SET Rand=.*!SET Rand=$RANDNC;!"  Higgs4CP_"$JOBDIR".i
$CA/exec/cain.exe < Higgs4CP_"$JOBDIR".i > log_Higgs4CP_"$JOBDIR".txt 2>&1
cd $CA/out/"$JOBDIR"
mv ComptonPlots.top Higgs4CP_"$JOBDIR"_ComptonPlots.top
td Higgs4CP_"$JOBDIR"_ComptonPlots.top POSTSCRIPT
$CA/ps2pdfRotate.sh Higgs4CP_"$JOBDIR"_ComptonPlots
mv Plots.top Higgs4CP_"$JOBDIR"_Plots.top
td Higgs4CP_"$JOBDIR"_Plots.top POSTSCRIPT
$CA/ps2pdfRotate.sh Higgs4CP_"$JOBDIR"_Plots
rm *.ps

cd $CA






