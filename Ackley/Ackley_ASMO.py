from __future__ import division, print_function, absolute_import
import os
import sys
sys.path.append('../src')
import ASMO
import numpy as np
import util
import cPickle

# model name
modelname = 'Ackley'
model = __import__(modelname)

# result path
respath = '../../UQ-res/SOO/%s' % modelname
if not os.path.exists(respath):
    os.makedirs(respath)

# load parameter name and range
pf = util.read_param_file('%s.txt' % modelname)
bd = np.array(pf['bounds'])
nInput = pf['num_vars']
xlb = bd[:,0]
xub = bd[:,1]

# run ASMO
niter = 50
bestx, bestf, x, f = ASMO.optimization(model, nInput, xlb, xub, niter)

print('Optimum found by ASMO:')
print('bestx:')
print(bestx)
print('bestf:')
print(bestf)
print('x:')
print(x)
print('f:')
print(f)

# save results to bin file
with open('%s/Ackley_ASMO.bin' % respath, 'w') as fbin:
    cPickle.dump({'bestx': bestx, 'bestf': bestf, 'x': x, 'f': f}, fbin)
