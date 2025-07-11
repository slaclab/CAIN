#!/bin/bash

set -e    # exit script on first error




CA="$PWD"

echo " CA= " $CA

# Usage example:  ./runCain.sh v00136 LinearQED_C3_x1000_po0p90_lpc+
JOBID=$1
COMPTON=$2

JOBDIR="$JOBID"_"$COMPTON"




rm -fR out/"$JOBDIR"

mkdir -p out/"$JOBDIR"

exec > $CA/out/"$JOBDIR"/runCain_"$JOBDIR".txt
exec 2>&1




echo " JOBID= " $JOBID   " COMPTON= " $COMPTON   " JOBDIR= " $JOBDIR 


cd $CA/out/"$JOBDIR"


cp ../../in/Higgs4CP_"$COMPTON".i Higgs4CP_"$JOBDIR".i
sed -i "s!xxxxxxxx!$JOBID!"  Higgs4CP_"$JOBDIR".i
../../exec/cain.exe < Higgs4CP_"$JOBDIR".i > log_Higgs4CP_"$JOBDIR".txt 2>&1
cd $CA/out/"$JOBDIR"
mv ComptonPlots.top Higgs4CP_"$JOBDIR"_ComptonPlots.top
#../../exec/td64 Higgs4CP_"$JOBDIR"_ComptonPlots.top POSTSCRIPT
#$CA/ps2pdfRotate.sh Higgs4CP_"$JOBDIR"_ComptonPlots
mv ComptonPlots2.top Higgs4CP_"$JOBDIR"_ComptonPlots2.top
#../../exec/td64 Higgs4CP_"$JOBDIR"_ComptonPlots2.top POSTSCRIPT
#$CA/ps2pdfRotate.sh Higgs4CP_"$JOBDIR"_ComptonPlots2
mv ComptonPlots3.top Higgs4CP_"$JOBDIR"_ComptonPlots3.top
#../../exec/td64 Higgs4CP_"$JOBDIR"_ComptonPlots3.top POSTSCRIPT
#$CA/ps2pdfRotate.sh Higgs4CP_"$JOBDIR"_ComptonPlots3
mv Plots.top Higgs4CP_"$JOBDIR"_Plots.top
#../../exec/td64 Higgs4CP_"$JOBDIR"_Plots.top POSTSCRIPT
#$CA/ps2pdfRotate.sh Higgs4CP_"$JOBDIR"_Plots
#rm *.ps

cd $CA/out/"$JOBDIR"
cp ../../in/HiggsIP_logEcmRange.i HiggsIP_"$JOBDIR".i
sed -i "s!xxxxxxxx!$JOBID!"  HiggsIP_"$JOBDIR".i
../../exec/cain.exe < HiggsIP_"$JOBDIR".i > log_HiggsIP_"$JOBDIR".txt 2>&1
cd $CA/out/"$JOBDIR"

mv HiggsIP.top HiggsIP_"$JOBDIR".top
#../../exec/td64 HiggsIP_"$JOBDIR".top POSTSCRIPT
#$CA/ps2pdfRotate.sh HiggsIP_"$JOBDIR"


mv BackgroundPlots.top BackgroundPlots_"$JOBDIR".top
#../../exec/td64 BackgroundPlots_"$JOBDIR".top POSTSCRIPT
#$CA/ps2pdfRotate.sh BackgroundPlots_"$JOBDIR"

mv BackgroundPlotsRvsS.top BackgroundPlotsRvsS_"$JOBDIR".top
#../../exec/td64 BackgroundPlotsRvsS_"$JOBDIR".top POSTSCRIPT
#$CA/ps2pdfRotate.sh BackgroundPlotsRvsS_"$JOBDIR"

mv BgndPlotsChgPtvsR.top BgndPlotsChgPtvsR_"$JOBDIR".top
#../../exec/td64 BgndPlotsChgPtvsR_"$JOBDIR".top POSTSCRIPT
#$CA/ps2pdfRotate.sh BgndPlotsChgPtvsR_"$JOBDIR"

mv BgndPlotsPhPtvsR.top BgndPlotsPhPtvsR_"$JOBDIR".top
#../../exec/td64 BgndPlotsPhPtvsR_"$JOBDIR".top POSTSCRIPT
#$CA/ps2pdfRotate.sh BgndPlotsPhPtvsR_"$JOBDIR"

mv ElectronEnergy.top ElectronEnergy_"$JOBDIR".top
#../../exec/td64 ElectronEnergy_"$JOBDIR".top POSTSCRIPT
#$CA/ps2pdfRotate.sh ElectronEnergy_"$JOBDIR"

mv PhotonEnergy.top PhotonEnergy_"$JOBDIR".top
#../../exec/td64 PhotonEnergy_"$JOBDIR".top POSTSCRIPT
#$CA/ps2pdfRotate.sh PhotonEnergy_"$JOBDIR"

mv HiggsLumi.top HiggsLumi_"$JOBDIR".top
#../../exec/td64 HiggsLumi_"$JOBDIR".top POSTSCRIPT
#$CA/ps2pdfRotate.sh HiggsLumi_"$JOBDIR"

INCPAIR=$(grep incpair higgs.txt | sed 's/^incpair *\([0,1]\).*/\1/' )

echo " INCPAIR= " $INCPAIR

if [ $INCPAIR -ne 0 ]
then
    mv IncPair.top IncPair_"$JOBDIR".top
#   ../../exec/td64 IncPair_"$JOBDIR".top POSTSCRIPT
#   $CA/ps2pdfRotate.sh IncPair_"$JOBDIR"
fi


#rm *.ps


cd $CA






