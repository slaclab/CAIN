# CAIN
CAIN Beam-Beam Collision code from KEK (K. Yokoya &amp; T. Tauchi)

Version 2.4.2 with modifications by T. Barklow  -- see doc/modified_files.txt

To compile:

cd src
make

To run an example job (gamma gamma  collisions at 280 GeV Ecm):

./scripts/runCain_v97112.sh


Note:  this script uses the command "sed -i ... "  to set input parameter variables, which doesn't
work on macOS with its bsd version of "sed -i" .
