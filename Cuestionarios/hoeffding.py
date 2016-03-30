#!/usr/bin/env python

import math

epsilon = 0.05

for M in [1, 100, 1000]:
    print("M = ", M, "-> N = ", math.ceil(-math.log(0.015/M)/(2*(epsilon**2))))
