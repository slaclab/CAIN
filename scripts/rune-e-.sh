#!/bin/bash

set -e    # exit script on first error


cd $CA


# Usage example:  ./rune-e-.sh v00136 LinearQED_C3_electron_only
JOBID=$1
COMPTON=$2

JOBDIR="$JOBID"_"$COMPTON"




rm -fR out/"$JOBDIR"

mkdir -p out/"$JOBDIR"

exec > $CA/out/"$JOBDIR"/rune-e-_"$JOBDIR".txt
exec 2>&1

echo " JOBID= " $JOBID   " COMPTON= " $COMPTON   " JOBDIR= " $JOBDIR 


cd $CA/out/"$JOBDIR"

cp ../../in/Higgs4CP_"$COMPTON".i Higgs4CP_"$JOBDIR".i

../../exec/cain.exe < Higgs4CP_"$JOBDIR".i > log_Higgs4CP_"$JOBDIR".txt 2>&1
cd $CA/out/"$JOBDIR"
mv ComptonPlots.top Higgs4CP_"$JOBDIR"_ComptonPlots.top
td Higgs4CP_"$JOBDIR"_ComptonPlots.top POSTSCRIPT
mv Plots.top Higgs4CP_"$JOBDIR"_Plots.top
td Higgs4CP_"$JOBDIR"_Plots.top POSTSCRIPT

cd $CA/out/"$JOBDIR"
cp ../../in/HiggsIP_electron_only.i HiggsIP_"$JOBDIR".i
../../exec/cain.exe < HiggsIP_"$JOBDIR".i > log_HiggsIP_"$JOBDIR".txt 2>&1
cd $CA/out/"$JOBDIR"
mv HiggsIP.top HiggsIP_"$JOBDIR"_plots.top
td HiggsIP_"$JOBDIR"_plots.top POSTSCRIPT

cd $CA






