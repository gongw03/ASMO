# Snow17SacUH
Standalone Fortran version of snow17 + sacramento + unit hydrograph routing models used in NCAR applications
Version adapted by Andy Wood, 2016, from other codes assembled by Andy Newman at NCAR

The subdirs here contain:

  src_bin/	 source code and compiled executable

  sample_model/  a sample model for testing the code, containing:
    run/ 	 small script to run the model, and namelist (control) file
    input/  	 parameter files and forcings, and obs flows if available
    output/  	 output simulation results from the run
    state/  	 model state files for re-initialization

The sample model is a 2-zone example using the Hungry Horse Reservoir inflow watershed, MT
with the Snow17/Sacramento snow and soil moisture accounting models, and UH routing
