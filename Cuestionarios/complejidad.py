#!/usr/bin/env python

import math

EPSILON = 0.05
DELTA = 0.05
dVC = 10

"""
Aplicamos la formula explicada en teoria
"""
def newN(n):
    result = 8 / (EPSILON**2)
    result *= math.log(4*((2*n)**dVC + 1)/DELTA)
    return result

if __name__ == "__main__":

    N = 10000
    new_N = newN(N)

    # Iteramos hasta que la diferencia entre iteraciones sea baja
    while math.fabs(new_N - N) > 1e-6:
        N = new_N
        new_N = newN(N)

    # Imprimimos el entero por encima del resultado
    print(math.ceil(N))
