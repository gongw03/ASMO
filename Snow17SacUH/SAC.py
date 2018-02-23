import os
import math
import string
import numpy as np
import sys
sys.path.append('../src')
import util

#######################################################
# USER SPECIFIC SECTION
#======================================================
modelpath = "../../../NCAR/NWS_hydro_models/Snow17SacUH/"
controlFileName = "SAC.txt"
appInputFiles = [ modelpath + "sample_model/input/params/sac_params.HHWM8.txt", \
                  modelpath + "sample_model/input/params/snow17_params.HHWM8.txt", \
                  modelpath + "sample_model/input/params/uh_params.HHWM8.txt"]
appInputTmplts = ["sac_params.HHWM8.txt.tmplt", "snow17_params.HHWM8.txt.tmplt", "uh_params.HHWM8.txt.tmplt"]

#######################################################
# FUNCTION: GENERATE MODEL INPUT FILE
#======================================================
def genAppInputFile(inputData, appTmpltFiles, appInputFiles, nInputs, inputNames):
    nfiles = len(appInputFiles)
    for i in range(nfiles):
        infile = open(appTmpltFiles[i], "r")
        outfile = open(appInputFiles[i], "w")
        while 1:
            lineIn  = infile.readline()
            if lineIn == "":
                break
            lineLen = len(lineIn)
            newLine = lineIn
            if nInputs > 0:
                for fInd in range(nInputs):
                    strLen = len(inputNames[fInd])
                    while 1:
                        sInd = newLine.find(inputNames[fInd])
                        if sInd < 0:
                            break
                        sdata = '%7.3f' % inputData[fInd]
                        strdata = str(sdata)
                        ntidx = sInd + strLen
                        lineTemp = newLine[0:sInd] + strdata +  " " + newLine[ntidx:lineLen+1]
                        newLine = lineTemp
                        lineLen = len(newLine)
            outfile.write(newLine)
        infile.close()
        outfile.close()
    return

def allindices(str1, str2, listindex = [], offset=0):
    """
    return all indeces of str2 found in str1
    """
    i = str1.find(str2, offset)
    while i >= 0:
        listindex.append(i)
        i = str1.find(str2, i + len(str2))
    return listindex

#######################################################
# FUNCTION: RUN MODEL
#====================================================== 
def runApplication():
    cwd = os.getcwd()
    os.chdir(modelpath + "sample_model/run/") 
    os.system("csh run_Snow17SacUH.csh")
    os.chdir(cwd)
    return

#######################################################
# FUNCTION: CALCULATE DESIRE OUTPUT
#======================================================
def getOutput():
    ## compute RMSE from 1970-01-01 to 2007-12-31
    #Qsim = np.loadtxt(modelpath + "sample_model/output/wbvars.txt.HHWM8", skiprows=1)[0:13878,18]
    #Qobs = np.loadtxt(modelpath + "sample_model/input/misc/HHWM8.NRNI.dly.mmd.txt", skiprows=1)[15159:29037,3]
    # compute RMSE from 1980-01-01 to 2000-12-31
    Qsim = np.loadtxt(modelpath + "sample_model/output/wbvars.txt.HHWM8", skiprows=1)[3652:11322,18]
    Qobs = np.loadtxt(modelpath + "sample_model/input/misc/HHWM8.NRNI.dly.mmd.txt", skiprows=1)[18811:26481,3]
    RMSE = np.sqrt(((Qsim - Qobs) ** 2).mean())
    return RMSE

#######################################################
# MAIN PROGRAM
#======================================================
def evaluate(x):
    pf = util.read_param_file(controlFileName)
    for n in range(pf['num_vars']):
        pf['names'][n] = 'UQ_' + pf['names'][n]
    genAppInputFile(x, appInputTmplts, appInputFiles, pf['num_vars'], pf['names'])
    runApplication()
    output = getOutput()
    return output
