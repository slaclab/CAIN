#!/bin/bash
export ATLAS_LOCAL_ROOT_BASE=/cvmfs/atlas.cern.ch/repo/ATLASLocalRootBase
source $ATLAS_LOCAL_ROOT_BASE/user/atlasLocalSetup.sh -2
source $HOME/iana-Cain-2.42_v6.26.08
cd $CA
echo " SLURM_ARRAY_TASK_ID= " $SLURM_ARRAY_TASK_ID
echo " dollar hash= " $#
"$1" "$2" "$3" "$4"
#echo " filename= " $filename
#echo " SLURM_ARRY_JOB_ID= " $SLURM_ARRAY_JOB_ID " SLURM_ARRAY_TASK_ID= " $SLURM_ARRAY_TASK_ID
#cp -p slurm-"$SLURM_ARRAY_JOB_ID"_"$SLURM_ARRAY_TASK_ID".out "$filename"/"$filename".txt
