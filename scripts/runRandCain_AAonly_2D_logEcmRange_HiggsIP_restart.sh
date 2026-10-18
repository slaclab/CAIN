#!/bin/bash

set -e    # exit script on first error


cd $CA


# Usage example:  ./runRandCain_AAonly_2D_logEcmRange_HiggsIP_restart.sh v08805 BW_BH_NPH3_x1000_po0p90_lpc+ 2700
JOBID=$1
COMPTON=$2
ITSTART=$3

JOBDIR="$JOBID"_"$COMPTON"
JOBRESTART="$JOBDIR"_it_"$ITSTART"           # <-- fixed: was missing $ on JOBDIR


exec > $CA/out/"$JOBDIR"/runCain_"$JOBRESTART".txt
exec 2>&1


RANDIP=$(od -An -N3 -i /dev/random | xargs)


# Get nstepip from the STORE file of the original run, then compute the
# restart start-time and the number of remaining steps for the PUSH command
NSTEPIP=$(grep nstepip $CA/out/"$JOBDIR"/higgs.txt | sed 's/[dD]/E/g' | awk '{printf "%d", $2}')

if [ -z "$NSTEPIP" ]; then
    echo " ERROR: nstepip not found in $CA/out/$JOBDIR/higgs.txt"
    exit 1
fi

if [ "$ITSTART" -ge "$NSTEPIP" ]; then
    echo " ERROR: ITSTART= $ITSTART >= nstepip= $NSTEPIP  -- nothing to restart"
    exit 1
fi

NSTEPLEFT=$((NSTEPIP - ITSTART))

echo " JOBID= " $JOBID   " COMPTON= " $COMPTON   " JOBDIR= " $JOBDIR  " ITSTART= " $ITSTART " RANDIP= " $RANDIP " NSTEPIP= " $NSTEPIP " NSTEPLEFT= " $NSTEPLEFT


cd $CA/out/"$JOBDIR"
cp $CA/in/HiggsIP_AAonly_2D_logEcmRange.i HiggsIP_"$JOBRESTART".i
sed -i "s!^ *SET Rand=.*!SET Rand=$RANDIP;!"  HiggsIP_"$JOBRESTART".i
sed -i "s!xxxxxxxx!$JOBID!"  HiggsIP_"$JOBRESTART".i
sed -i "s!higgs_it_\(.*\)\.dat!higgs_it_\1_$JOBRESTART.dat!"  HiggsIP_"$JOBRESTART".i
sed -i "s!IF it=$ITSTART!IF it=-$ITSTART!"  HiggsIP_"$JOBRESTART".i

# --- Restart modifications ---
# 1) Read the checkpointed beam instead of the original higgs.dat
sed -i "s!^ *BEAM  *FILE='higgs.dat';! BEAM  FILE='higgs_it_$ITSTART.dat';!"  HiggsIP_"$JOBRESTART".i

# 2) Start the iteration counter at ITSTART so Mod(it,50) printout and
#    the it=... checkpoint WRITEs stay consistent with the original run
sed -i "s!^ *SET it=0;! SET it=$ITSTART;!"  HiggsIP_"$JOBRESTART".i

# 3) Start the PUSH at the time of step ITSTART and do the remaining steps.
#    Start time = tmin + ITSTART*(tmax-tmin)/nstepip ; Nsteps = nstepip-ITSTART,
#    which keeps the time step dt=(tmax-tmin)/nstepip identical to the original run.
sed -i "s!^ *PUSH  *Time=(tmin,tmax,nstepip) *;! PUSH  Time=(tmin+$ITSTART*(tmax-tmin)/$NSTEPIP,tmax,$NSTEPLEFT) ;!"  HiggsIP_"$JOBRESTART".i

$CA/exec/cain.exe < HiggsIP_"$JOBRESTART".i > log_HiggsIP_"$JOBRESTART".txt 2>&1
cd $CA/out/"$JOBDIR"

mv HiggsIP.top HiggsIP_"$JOBRESTART".top
td HiggsIP_"$JOBRESTART".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh HiggsIP_"$JOBRESTART"


mv BackgroundPlots.top BackgroundPlots_"$JOBRESTART".top
td BackgroundPlots_"$JOBRESTART".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh BackgroundPlots_"$JOBRESTART"

mv BackgroundPlotsRvsS.top BackgroundPlotsRvsS_"$JOBRESTART".top
td BackgroundPlotsRvsS_"$JOBRESTART".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh BackgroundPlotsRvsS_"$JOBRESTART"

mv BgndPlotsChgPtvsR.top BgndPlotsChgPtvsR_"$JOBRESTART".top
td BgndPlotsChgPtvsR_"$JOBRESTART".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh BgndPlotsChgPtvsR_"$JOBRESTART"

mv BgndPlotsPhPtvsR.top BgndPlotsPhPtvsR_"$JOBRESTART".top
td BgndPlotsPhPtvsR_"$JOBRESTART".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh BgndPlotsPhPtvsR_"$JOBRESTART"

mv ElectronEnergy.top ElectronEnergy_"$JOBRESTART".top
td ElectronEnergy_"$JOBRESTART".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh ElectronEnergy_"$JOBRESTART"

mv PhotonEnergy.top PhotonEnergy_"$JOBRESTART".top
td PhotonEnergy_"$JOBRESTART".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh PhotonEnergy_"$JOBRESTART"

mv HiggsLumi.top HiggsLumi_"$JOBRESTART".top
td HiggsLumi_"$JOBRESTART".top POSTSCRIPT
$CA/scripts/ps2pdfRotate.sh HiggsLumi_"$JOBRESTART"

INCPAIR=$(grep incpair higgs.txt | sed 's/^incpair *\([0,1]\).*/\1/' )

echo " INCPAIR= " $INCPAIR

if [ $INCPAIR -ne 0 ]
then
    mv IncPair.top IncPair_"$JOBRESTART".top
    td IncPair_"$JOBRESTART".top POSTSCRIPT
    $CA/scripts/ps2pdfRotate.sh IncPair_"$JOBRESTART"
fi


rm *.ps

cd $CA
