#!/bin/bash

set -e    # exit script on first error


cd $CA


# Usage example:  ./runCain.sh v00136 LinearQED_C3_x1000_po0p90_lpc+
JOBID=$1
COMPTON=$2

JOBDIR="$JOBID"_"$COMPTON"




#rm -fR out/"$JOBDIR"

#mkdir -p out/"$JOBDIR"

exec > $CA/out/"$JOBDIR"/runCain_"$JOBDIR".txt
exec 2>&1


RANDNC=$(od -An -N3 -i /dev/random | xargs)


echo " JOBID= " $JOBID   " COMPTON= " $COMPTON   " JOBDIR= " $JOBDIR  " RANDNC= " $RANDNC


cd $CA/out/"$JOBDIR"


#cp $CA/in/Higgs4CP_"$COMPTON".i Higgs4CP_"$JOBDIR".i
sed -i "s!^ *SET Rand=.*!SET Rand=$RANDNC;!"  Higgs4CP_"$JOBDIR".i
$CA/exec/cain.exe < Higgs4CP_"$JOBDIR".i > log_Higgs4CP_"$JOBDIR".txt 2>&1
cd $CA/out/"$JOBDIR"
mv ComptonPlots.top Higgs4CP_"$JOBDIR"_ComptonPlots.top
td Higgs4CP_"$JOBDIR"_ComptonPlots.top POSTSCRIPT
$CA/ps2pdfRotate.sh Higgs4CP_"$JOBDIR"_ComptonPlots
mv ComptonPlots2.top Higgs4CP_"$JOBDIR"_ComptonPlots2.top
td Higgs4CP_"$JOBDIR"_ComptonPlots2.top POSTSCRIPT
$CA/ps2pdfRotate.sh Higgs4CP_"$JOBDIR"_ComptonPlots2
mv Plots.top Higgs4CP_"$JOBDIR"_Plots.top
td Higgs4CP_"$JOBDIR"_Plots.top POSTSCRIPT
$CA/ps2pdfRotate.sh Higgs4CP_"$JOBDIR"_Plots
rm *.ps

cd $CA/out/"$JOBDIR"
cp $CA/in/HiggsIP_2dEcmRange.i HiggsIP_"$JOBDIR".i
$CA/exec/cain.exe < HiggsIP_"$JOBDIR".i > log_HiggsIP_"$JOBDIR".txt 2>&1
cd $CA/out/"$JOBDIR"

mv HiggsIP.top HiggsIP_"$JOBDIR".top
td HiggsIP_"$JOBDIR".top POSTSCRIPT
$CA/ps2pdfRotate.sh HiggsIP_"$JOBDIR"

mv BackgroundPlots.top BackgroundPlots_"$JOBDIR".top
td BackgroundPlots_"$JOBDIR".top POSTSCRIPT
$CA/ps2pdfRotate.sh BackgroundPlots_"$JOBDIR"

mv BackgroundPlotsRvsS.top BackgroundPlotsRvsS_"$JOBDIR".top
td BackgroundPlotsRvsS_"$JOBDIR".top POSTSCRIPT
$CA/ps2pdfRotate.sh BackgroundPlotsRvsS_"$JOBDIR"

mv BgndPlotsChgPtvsR.top BgndPlotsChgPtvsR_"$JOBDIR".top
td BgndPlotsChgPtvsR_"$JOBDIR".top POSTSCRIPT
$CA/ps2pdfRotate.sh BgndPlotsChgPtvsR_"$JOBDIR"

mv BgndPlotsPhPtvsR.top BgndPlotsPhPtvsR_"$JOBDIR".top
td BgndPlotsPhPtvsR_"$JOBDIR".top POSTSCRIPT
$CA/ps2pdfRotate.sh BgndPlotsPhPtvsR_"$JOBDIR"

mv ElectronEnergy.top ElectronEnergy_"$JOBDIR".top
td ElectronEnergy_"$JOBDIR".top POSTSCRIPT
$CA/ps2pdfRotate.sh ElectronEnergy_"$JOBDIR"

mv PhotonEnergy.top PhotonEnergy_"$JOBDIR".top
td PhotonEnergy_"$JOBDIR".top POSTSCRIPT
$CA/ps2pdfRotate.sh PhotonEnergy_"$JOBDIR"

mv IncPair.top IncPair_"$JOBDIR".top
td IncPair_"$JOBDIR".top POSTSCRIPT
$CA/ps2pdfRotate.sh IncPair_"$JOBDIR"

mv HiggsLumi.top HiggsLumi_"$JOBDIR".top
td HiggsLumi_"$JOBDIR".top POSTSCRIPT
$CA/ps2pdfRotate.sh HiggsLumi_"$JOBDIR"
rm *.ps

cd $CA






