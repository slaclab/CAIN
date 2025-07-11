#!/bin/bash

set -e    # exit script on first error


cd $CA


# Usage example:  ./run2Linacs v00176 2Linacs_C3_x1000_po0p90_lpc+
JOBID=$1
COMPTON=$2

JOBDIR="$JOBID"_"$COMPTON"




rm -fR out/"$JOBDIR"

mkdir -p out/"$JOBDIR"

exec > $CA/out/"$JOBDIR"/runCain_"$JOBDIR".txt
exec 2>&1

echo " JOBID= " $JOBID   " COMPTON= " $COMPTON   " JOBDIR= " $JOBDIR 


cd $CA/out/"$JOBDIR"

cp ../../in/Higgs2CP_"$COMPTON".i Higgs2CP_"$JOBDIR".i

../../exec/cain.exe < Higgs2CP_"$JOBDIR".i > log_Higgs2CP_"$JOBDIR".txt 2>&1
#../../exec/cain.exe < Higgs2CP_"$JOBDIR".i 
cd $CA/out/"$JOBDIR"
mv ComptonXPlots.top Higgs2CP_"$JOBDIR"_ComptonXPlots.top
td Higgs2CP_"$JOBDIR"_ComptonXPlots.top POSTSCRIPT
$CA/ps2pdfRotate.sh Higgs2CP_"$JOBDIR"_ComptonXPlots
mv ComptonYPlots.top Higgs2CP_"$JOBDIR"_ComptonYPlots.top
td Higgs2CP_"$JOBDIR"_ComptonYPlots.top POSTSCRIPT
$CA/ps2pdfRotate.sh Higgs2CP_"$JOBDIR"_ComptonYPlots
mv Plots.top Higgs2CP_"$JOBDIR"_Plots.top
td Higgs2CP_"$JOBDIR"_Plots.top POSTSCRIPT
$CA/ps2pdfRotate.sh Higgs2CP_"$JOBDIR"_Plots
mv HiggsLumi.top HiggsLumi_"$JOBDIR".top
td HiggsLumi_"$JOBDIR".top POSTSCRIPT
$CA/ps2pdfRotate.sh HiggsLumi_"$JOBDIR"
mv HiggsIP.top HiggsIP_"$JOBDIR".top
td HiggsIP_"$JOBDIR".top POSTSCRIPT
$CA/ps2pdfRotate.sh HiggsIP_"$JOBDIR"
rm *.ps

cd $CA






