#!/bin/bash

set -e    # exit script on first error


exec > postCain.txt
exec 2>&1


$CA/exec/cain.exe < post_HiggsIP.i > log_post_HiggsIP.txt 2>&1

td HiggsIP_post.top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh HiggsIP_post


td BackgroundPlots_post.top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh BackgroundPlots_post

td BackgroundPlotsRvsS_post.top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh BackgroundPlotsRvsS_post

td BgndPlotsChgPtvsR_post.top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh BgndPlotsChgPtvsR_post

td BgndPlotsPhPtvsR_post.top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh BgndPlotsPhPtvsR_post

td ElectronEnergy_post.top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh ElectronEnergy_post

td PhotonEnergy_post.top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh PhotonEnergy_post

td HiggsLumi_post.top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh HiggsLumi_post

INCPAIR=$(grep incpair higgs.txt | sed 's/^incpair *\([0,1]\).*/\1/' )

echo " INCPAIR= " $INCPAIR

if [ $INCPAIR -ne 0 ]
then
    td IncPair_post.top POSTSCRIPT
    $CA/scripts/ps2pdfRotate.sh IncPair_post
    td IncPairRvsS_post.top POSTSCRIPT
    $CA/scripts/ps2pdfRotate.sh IncPairRvsS_post
fi


rm *.ps







