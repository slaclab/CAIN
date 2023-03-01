cd cain242-TMTT
cd ./exec
 ./@make all  or  ./@make

! input file name and comment, e.g. nominal for crab crossing with 14mr horizontal crossing
! put incpair=1 for incoherent pair generation
./ex_go_original 250.tdr-waist nominal

cd ../out
 cd ./250.tdr-waist-nominal/
!  You may change  all  0.0000D+60  to 0.0000D+0  in the topdraw file to escape *** ERROR *** Number out of range -1.000E+35 to  1.000E+35.
! to make a pdf file from topdraw file (output)
../ex_pdf cain242-250.tdr-waist-nominal
