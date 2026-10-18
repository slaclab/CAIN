#!/bin/bash

set -e    # exit script on first error

cd $CA/out

pwd

ln -fs v06980_BW_BH_NPH3_x1000_po0p90_lpc+/ XCC_125
ln -fs v06888_BW_BH_NPH3_x1000_po0p90_lpc+/ XCC_380
ln -fs v06972_BW_BH_NPH3_x1000_po0p90_lpc+/ Optical_216
ln -fs v07112_BW_BH_NPH3_x1000_po0p90_lpc+/ XCC_280
ln -fs v06311_BW_BH_NPH3_x1000_po0p90_lpc+/ PWFA_15TeV
ln -fs v06400_BW_BH_NPH3_x1000_po0p90_lpc+/ PWFA_10TeV
