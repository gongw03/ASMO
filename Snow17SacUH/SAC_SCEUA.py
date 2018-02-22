from __future__ import division, print_function, absolute_import
import os
import sys
sys.path.append('../src')
import SCEUA
import numpy as np
import util
import cPickle

# model name
modelname = 'SAC'
model = __import__(modelname)

# result path
respath = '../../UQ-res/%s' % modelname
if not os.path.exists(respath):
    os.makedirs(respath)

# load parameter name and range
pf = util.read_param_file('%s.txt' % modelname)
bd = np.array(pf['bounds'])
nInput = pf['num_vars']
xlb = bd[:,0]
xub = bd[:,1]

# run SCE-UA
bestx, bestf, icall, nloop, bestx_list, bestf_list, icall_list = \
    SCEUA.optimization(model, nInput, xlb, xub)

print('Optimum found by SCE-UA:')
print('bestx:')
print(bestx)
print('bestf:')
print(bestf)

# save results to bin file
with open('%s/SAC_SCEUA.bin' % respath, 'w') as f:
    cPickle.dump({'bestx': bestx, 'bestf': bestf, 'icall': icall, \
        'bestx_list': bestx_list, 'bestf_list': bestf_list, \
        'icall_list': icall_list}, f)
