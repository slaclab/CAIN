#!/bin/bash

set -e    # exit script on first error



exec > postCain.txt
exec 2>&1




$CA/exec/cain.exe < postRande+e-+logEcmRange.sh> logpost_e+e-.txt 2>&1

mv e+e-Lumi.top e+e-Lumi_"$JOBDIR".top
td e+e-Lumi_"$JOBDIR".top POSTSCRIPT
$CA/ps2pdfRotate.sh e+e-Lumi_"$JOBDIR"
rm *.ps

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



if [ $INCPAIR -ne 0 ]
then
    mv IncPair.top IncPair_"$JOBDIR".top
    td IncPair_"$JOBDIR".top POSTSCRIPT
    $CA/ps2pdfRotate.sh IncPair_"$JOBDIR"

    mv IncPairRvsS.top IncPairRvsS_"$JOBDIR".top
    td IncPairRvsS_"$JOBDIR".top POSTSCRIPT
    $CA/ps2pdfRotate.sh IncPairRvsS_"$JOBDIR"
fi





rm *.ps

cd $CA






