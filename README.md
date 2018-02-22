# ASMO
This is a standalone version of ASMO, a surrogate based single objective optimization algorithm.

Quick start: please run Ackley/Ackley\_ASMO.py to start your first run. For more information about ASMO, please read the paper. And please cite it if you use the code in your own research.
Wang, C., Duan, Q., Gong, W., Ye, A., Di, Z., & Miao, C. (2014). An evaluation of adaptive surrogate modeling based optimization with two benchmark problems. Environmental Modelling & Software, 60(0), 167-179. https://doi.org/10.1016/j.envsoft.2014.05.026

Two test cases with the test function Ackley.
1. Ackley\_SCEUA.py: Optimize with SCE-UA algorithm, a traditional single objective optimization algorithm.
2. Ackley\_ASMO.py: Optimize with ASMO.

Other files in directory Ackley:
1. Ackley.py: the test function Ackley.
2. Ackley.txt: the parameter name, lower bound, upper bound.

A quick example of NWS hydrology model Sno17SacUH to show how to connect your own model with ASMO.
Two test cases.
1. SAC\_SCEUA.py: Optimize with SCE-UA algorithm, a traditional single objective optimization algorithm.
2. SAC\_ASMO.py: Optimize with ASMO.

Other files in directory Snow17SacUH (abbrev. SAC):
1. SAC.py: model driver file that put the parameter value in the parameter files that Snow17SacUH can read, run the model, and compute objective function (Here we use RMSE).
2. SAC.txt: the parameter name, lower bound, upper bound.
3. sac\_params.HHWM8.txt.tmplt: parameter templete file, replace string "UQ\_xxxx" in this file and generate parameter file sac\_params.HHWM8.txt for the model to load.
