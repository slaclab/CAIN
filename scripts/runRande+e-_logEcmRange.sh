#!/bin/bash

set -e    # exit script on first error


cd $CA


# Usage example:  ./rune+e-_logEcmRange.sh v00507 
JOBID=$1

JOBDIR="$JOBID"_e+e-

echo " JOBDIR= " $JOBDIR


#rm -fR out/"$JOBDIR"

#mkdir -p out/"$JOBDIR"

exec > $CA/out/"$JOBDIR"/runCain_"$JOBDIR".txt
exec 2>&1




RANDNC=$(od -An -N3 -i /dev/random | xargs)
RANDIP=$(od -An -N3 -i /dev/random | xargs)


echo " JOBID= " $JOBID   " COMPTON= " $COMPTON   " JOBDIR= " $JOBDIR  " RANDNC= " $RANDNC  " RANDIP= " $RANDIP





cd $CA/out/"$JOBDIR"
#cp ../../in/e+e-IP_logEcmRange.i  e+e-IP_"$JOBDIR".i
sed -i "s!^ *SET Rand=.*!SET Rand=$RANDNC;!"  e+e-_"$JOBDIR".i
sed -i "s!xxxxxxxx!$JOBID!"  e+e-_"$JOBDIR".i
$CA/exec/cain.exe < e+e-_"$JOBDIR".i > log_e+e-_"$JOBDIR".txt 2>&1
cd $CA/out/"$JOBDIR"

mv e+e-Lumi.top e+e-Lumi_"$JOBDIR".top
td e+e-Lumi_"$JOBDIR".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh e+e-Lumi_"$JOBDIR"
rm *.ps

mv HiggsIP.top HiggsIP_"$JOBDIR".top
td HiggsIP_"$JOBDIR".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh HiggsIP_"$JOBDIR"

mv BackgroundPlots.top BackgroundPlots_"$JOBDIR".top
td BackgroundPlots_"$JOBDIR".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh BackgroundPlots_"$JOBDIR"

mv BackgroundPlotsRvsS.top BackgroundPlotsRvsS_"$JOBDIR".top
td BackgroundPlotsRvsS_"$JOBDIR".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh BackgroundPlotsRvsS_"$JOBDIR"

mv BgndPlotsChgPtvsR.top BgndPlotsChgPtvsR_"$JOBDIR".top
td BgndPlotsChgPtvsR_"$JOBDIR".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh BgndPlotsChgPtvsR_"$JOBDIR"

mv BgndPlotsPhPtvsR.top BgndPlotsPhPtvsR_"$JOBDIR".top
td BgndPlotsPhPtvsR_"$JOBDIR".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh BgndPlotsPhPtvsR_"$JOBDIR"

mv ElectronEnergy.top ElectronEnergy_"$JOBDIR".top
td ElectronEnergy_"$JOBDIR".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh ElectronEnergy_"$JOBDIR"

mv PhotonEnergy.top PhotonEnergy_"$JOBDIR".top
td PhotonEnergy_"$JOBDIR".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh PhotonEnergy_"$JOBDIR"

INCPAIR=$(grep incpair higgs.txt | sed 's/^incpair *\([0,1]\).*/\1/' )


if [ $INCPAIR -ne 0 ]
then
    mv IncPair.top IncPair_"$JOBDIR".top
    td IncPair_"$JOBDIR".top POSTSCRIPT
    $CA/scripts/ps2pdfRotate.sh IncPair_"$JOBDIR"

    mv IncPairRvsS.top IncPairRvsS_"$JOBDIR".top
    td IncPairRvsS_"$JOBDIR".top POSTSCRIPT
    $CA/scripts/ps2pdfRotate.sh IncPairRvsS_"$JOBDIR"
fi





rm *.ps

cd $CA






