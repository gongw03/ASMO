#!/bin/csh
# A. Wood, 2016
# Run general distributed snow17-sac-uh code, which loops over 
# lumped code version for multiple model areas and writes an
# additional combined output file
#
# To run the snow17/sac model, just execute this script (or command below)

 ../../src_bin/Snow17SacUH.exe namelist.HHWM8

# -------------------------------------
# the control file format is:
# number_of_areas "name of combined output file"
# namelist for first area
# namelist for second area
# namelist for third area
# ...


