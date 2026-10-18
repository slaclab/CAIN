#!/bin/bash

set -e    # exit script on first error


# Usage example:  .run_v08770_higgs_scan.sh


cd $CA/out/v08770_BW_BH_NPH3_x1000_po0p90_lpc+

cp $CA/scripts/*.C .

ln -fs higgs.dat higgs_v08770.dat

rm cat_higgs_scan_v08770.txt

pwd


root -l -q 'higgs_scan.C("higgs_v08770.dat", 1, 4900e9, 5100e9)' > higgs_scan_v08770_1_4900_5100.txt
root -l -q 'higgs_scan.C("higgs_v08770.dat", 1, 4000e9, 4900e9)' > higgs_scan_v08770_1_4000_4900.txt
root -l -q 'higgs_scan.C("higgs_v08770.dat", 1, 100e9, 4000e9)' > higgs_scan_v08770_1_100_4000.txt
root -l -q 'higgs_scan.C("higgs_v08770.dat", 1, 0e9, 100e9)' > higgs_scan_v08770_1_0_100.txt

root -l -q 'higgs_scan.C("higgs_v08770.dat", 2, 4900e9, 5100e9)' > higgs_scan_v08770_2_4900_5100.txt
root -l -q 'higgs_scan.C("higgs_v08770.dat", 2, 4000e9, 4900e9)' > higgs_scan_v08770_2_4000_4900.txt
root -l -q 'higgs_scan.C("higgs_v08770.dat", 2, 100e9, 4000e9)' > higgs_scan_v08770_2_100_4000.txt
root -l -q 'higgs_scan.C("higgs_v08770.dat", 2, 0e9, 100e9)' > higgs_scan_v08770_2_0_100.txt

root -l -q 'higgs_scan.C("higgs_v08770.dat", 3, 4900e9, 5100e9)' > higgs_scan_v08770_3_4900_5100.txt
root -l -q 'higgs_scan.C("higgs_v08770.dat", 3, 4000e9, 4900e9)' > higgs_scan_v08770_3_4000_4900.txt
root -l -q 'higgs_scan.C("higgs_v08770.dat", 3, 100e9, 4000e9)' > higgs_scan_v08770_3_100_4000.txt
root -l -q 'higgs_scan.C("higgs_v08770.dat", 3, 0e9, 100e9)' > higgs_scan_v08770_3_0_100.txt


cat higgs_scan_v08770*.txt > cat_higgs_scan_v08770.txt
