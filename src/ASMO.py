# -*- coding: utf-8 -*-
# Adaptive Surrogate Modelling-based Optimization
from __future__ import division, print_function, absolute_import
import sampling
import gp
import SCEUA
import numpy as np

def optimization(model, nInput, xlb, xub, niter, Xinit = None, Yinit = None, 
                 ngs = None, maxn = 3000, kstop = 10, 
                 pcento = 0.1, peps = 0.001):
    """ 
    Adaptive Surrogate Modelling-based Optimization
    model:  the evaluated model function
    nInput: number of model input
    xlb:    lower bound of input
    xub:    upper bound of input
    niter:  number of total iteration
    Xinit/Yinit: initial samplers for surrogate model construction
    ngs:    number of complexes (sub-populations)
    kstop:  maximum number of evolution loops before convergency
    pcento: the percentage change allowed in kstop loops before convergency
    peps:   minimum range of parameter
    maxn:   number of maximum model runs - surrogate model
    """
    if (Xinit is None and Yinit is None):
        Ninit = nInput * 10
        Xinit = sampling.glp(Ninit, nInput)
        for i in range(Ninit):
            Xinit[i,:] = Xinit[i,:] * (xub - xlb) + xlb
        Yinit = np.zeros(Ninit)
        for i in range(Ninit):
            Yinit[i] = model.evaluate(Xinit[i,:])
    else:
        Ninit = Xinit.shape[0]
    icall = Ninit
    x = Xinit.copy()
    y = Yinit.copy()

    for i in range(niter):
        print('Surrogate Opt loop: %d' % i)
        sm = gp.GPR_Matern(x, y, nInput, 1, x.shape[0], xlb, xub)
        bestx_sm, bestf_sm, icall_sm, nloop_sm, \
        bestx_list_sm, bestf_list_sm, icall_list_sm = \
            SCEUA.optimization(sm, nInput, xlb, xub, 
                               ngs, maxn, kstop, pcento, peps, verbose = False)
        bestx = bestx_sm.copy()
        bestf = model.evaluate(bestx)
        icall += 1
        x = np.vstack((x, bestx))
        y = np.append(y, bestf)

    return bestx, bestf, x, y

def onestep(nInput, xlb, xub, Xinit, Yinit, 
            ngs = None, maxn = 3000, kstop = 10, pcento = 0.1, peps = 0.001):
    """ 
    Adaptive Surrogate Modelling-based Optimization
    One-step mode for offline optimization
    Do NOT call the model evaluation function
    nInput: number of model input
    xlb:    lower bound of input
    xub:    upper bound of input
    Xinit/Yinit: initial samplers for surrogate model construction
    ngs:    number of complexes (sub-populations)
    kstop:  maximum number of evolution loops before convergency
    pcento: the percentage change allowed in kstop loops before convergency
    peps:   minimum range of parameter
    maxn:   number of maximum model runs - surrogate model
    """
    x = Xinit.copy()
    y = Yinit.copy()

    sm = gp.GPR_Matern(x, y, nInput, 1, x.shape[0], xlb, xub)
    bestx_sm, bestf_sm, icall_sm, nloop_sm, \
    bestx_list_sm, bestf_list_sm, icall_list_sm = \
        SCEUA.optimization(sm, nInput, xlb, xub, 
                           ngs, maxn, kstop, pcento, peps, verbose = False)
    x_resample = bestx_sm.copy()

    return x_resample
