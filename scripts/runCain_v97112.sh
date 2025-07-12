#!/bin/bash

set -e    # exit script on first error

echo $1




if [ $# == 0 ]; then
    DOBATCH=INTERACTIVE
else
    DOBATCH=$1
fi




vid=v97112 ; comptonid=BW_BH_NPH3_x1000_po0p90_lpc+; jobdir="$vid"_"$comptonid"; mkdir out/"$jobdir"; cp in/Higgs4CP_"$comptonid".i out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^bfieldext=.*/bfieldext=5,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^outputip=.*/outputip=1,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^wenhbeam=.*/wenhbeam=1.0,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^wenhpair=.*/wenhpair=1.0,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^wenhpp=.*/wenhpp=1.0,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's!^smeshval=.*!smeshval=sigz/3,!' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^offy=.*/offy=0.00*sigy,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^ifldbe=.*/ifldbe=0,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^ibealt=.*/ibealt=-1,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^wscaler=.*/wscaler=10,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^wscalel=.*/wscalel=10,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^bdipole=.*/bdipole=0.0,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^eplotmax=.*/eplotmax=1000.,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^eplotmin=.*/eplotmin=0.,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^plotincp=.*/plotincp=-1,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^nsigbb=.*/nsigbb=2,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^ansep=.*/ansep=2.00D10,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's!^eesep=.*!eesep=10.0D9,  gammasep=eesep/Emass,!' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^sigzsep=.*/sigzsep=3.0*micron,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^dorigin=.*/dorigin=-66.0*nm,/' out/"$jobdir"/Higgs4CP_"$jobdir".i 
sed -i 's/^xnumber.*,/xnumber=1300.,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^dcp.*,/dcp=0.04*mm,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^an=.*/an=0.63D10,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^betax=.*/betax=0.010*mm,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^betay=.*/betay=0.010*mm,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^emitx=.*!emitx=60.0*nm/gamma,!' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^emity=.*!emity=60.0*nm/gamma,!' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^emitxsep=.*!emitxsep=120.0*nm*40./gammasep,!' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^emitysep=.*!emitysep=120.0*nm/40./gammasep,!' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^cfbeamstr=.*/cfbeamstr=1,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^cfpair=.*/cfpair=1,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^incpair=.*/incpair=1,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^nstepip=.*/nstepip=100,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^pushexternal=.*/pushexternal=1,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^ *thr=.*!thr=0.002/2,!'  out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^ *thl=.*!thl=0.002/2,!'  out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^ *deltar=.*!deltar=0.00,!'  out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^ *deltal=.*!deltal=0.00,!'  out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^ *crabon=.*!crabon=1,!'  out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^ *laserthl=.*!laserthl=0.0*thr+laserthloff,!'  out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^ *laserthr=.*!laserthr=0.0*thl+laserthroff,!'  out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^ *laserthloff=.*!laserthloff=0.0,!'  out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^ *laserthroff=.*!laserthroff=0.0,!'  out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^ *SET.*spinleft=.*; !/SET MsgLevel=1000000,spinleft=-0.9; !/'  out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/MVPH=.*, ML/MVPH=1600000, ML/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/NP=.*, AN/NP=64000, AN/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^ee=.*!ee=140D9,  gamma=ee/Emass,!' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^pulseE=.*!pulseE=1.4*0.72,!' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^agammax=.*!agammax=21.21*nm,!' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's!^agammay=.*!agammay=21.21*nm,!' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^ *sigt=.*/sigt=sigz,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^sige=.*/sige=0.0005,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^sigz=.*/sigz=10.0*micron,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^nbunch=.*/nbunch=121, reprate=120,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^d\([1,2]\)=.*/d\1=1,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^egamma=.*/egamma=0,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
sed -i 's/^BEAM LEFT.*/BEAM LEFT, AN=1.00000*an,/' out/"$jobdir"/Higgs4CP_"$jobdir".i
if [ $DOBATCH == "BATCH" ]; then sbatch -A atlas:usatlas ./scripts/sbatch_batch.sh ./scripts/runRandCain_example_logEcmRange.sh "$vid" "$comptonid"; else nohup ./scripts/runRandCain_example_logEcmRange.sh "$vid" BW_BH_NPH3_x1000_po0p90_lpc+ > /dev/null 2>&1 & fi
