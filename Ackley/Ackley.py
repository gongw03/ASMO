# -*- coding: utf-8 -*-
"""
Created on Mon May 04 16:03:16 2015
@author: gongwei
"""
import math

def evaluate(x):
	'''
	%
	%  This is the Ackley's Function
	%  Bound: XUB = [5,5]; XLB = [-5,-5]
	%  Global Optimum: f(0,0) = 0
	%
	'''
	return -20 * math.exp(-0.2*math.sqrt(0.5*(x[0]**2 + x[1]**2))) - \
        math.exp(0.5*(math.cos(2*math.pi*x[0]) + math.cos(2*math.pi*x[1]))) + \
        20 + math.exp(1)


