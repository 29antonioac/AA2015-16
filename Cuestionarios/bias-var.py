#!/usr/bin/env python

import matplotlib.pyplot as plt
import numpy as np
from random import uniform
from sympy import *
from time import time


if __name__ == "__main__":

    x, y, z, t = symbols('x y z t')
    k, m, n = symbols('k m n', integer=True)
    f, g, h = symbols('f g h', cls=Function)
    a, b = symbols('a b')

    np.random.seed(12345678)

    # Tomamos el tiempo actual
    inicio = time()

    # La funcion objetivo f es x^2
    f = x**2
    def f_f(dato):
        return f.subs(x,dato)
    f_v = np.vectorize(f_f)

    # Determinamos g media como la media de K funciones g^(D)(x) con D aleatorio
    # La funcion g^(D)(x) esta definida en el ejercicio

    g_D = (b + a)*(x - a) + a**2

    # La salida completa con 1000, 10000 y 100000 datos esta abajo
    # Se deja solo 100 porque las operaciones simbolicas son costosas
    num_datos = 100
    D = np.random.uniform(-1,1,(num_datos,2))

    g_media = sympify(0)

    for dato in D:
        g_media += g_D.subs({a:dato[0],b:dato[1]})
    g_media /= num_datos

    def g_media_f(dato):
        return g_media.subs(x,dato)
    g_media_v = np.vectorize(g_media_f)

    print("g_media(x) =",g_media)

    # La salida despues de este punto con 1000,10000 y 100000 es
    # -0.0116637213459665*x - 0.0155484354190228  Tiempo = 1.829831600189209
    # -0.00340066477585505*x - 0.00103483800982056  Tiempo = 18.55463218688965
    # 0.00516305073304809*x - 0.00057119754433822  Tiempo = 182.91639041900635

    # Determinamos ahora el bias(x)
    # No es mas que el cuadrado de la diferencia
    # entre la funcion media y la objetivo al cuadrado

    bias = simplify((g_media - f)**2)

    # Calculamos la esperanza de bias(x) como la media de 1000 valores en [-1,1]

    discretizacion = np.linspace(-1,1,1000)
    def bias_f(dato):
        return bias.subs(x,dato)
    bias_v = np.vectorize(bias_f)
    esp_bias = np.mean(bias_v(discretizacion))

    print("Bias =",esp_bias)
    print("Calculando varianza...")


    # Ahora calculamos la varianza del mismo modo que la g media.
    # Hacemos la media con 1000 datos del cuadrado de la diferencia
    # entre la funcion g muestreada y la g media

    varianza = sympify(0)

    for dato in D:
        varianza += (g_D.subs({a:dato[0],b:dato[1]}) - g_media.subs({a:dato[0],b:dato[1]}))**2
    varianza /= num_datos
    varianza = simplify(varianza)

    # Calculamos la esperanza de varianza(x) igual que bias
    def varianza_f(dato):
        return varianza.subs(x,dato)
    varianza_v = np.vectorize(varianza_f)
    esp_varianza = np.mean(varianza_v(discretizacion))

    print("Varianza =",esp_varianza)
    print("Calculando Eout_D...")

    # Nuestro ultimo paso es calcular la esperanza del Eout

    Eout_D = simplify((g_D - f)**2)
    Eout_media = 0
    for dato in D:
        Eout_media += Eout_D.subs({a:dato[0],b:dato[1]})
    Eout_media /= num_datos
    Eout_media = simplify(Eout_media)

    print("Eout_D calculado, ahora la esperanza...")

    def eout_f(dato):
        return Eout_media.subs(x,dato)
    eout_v = np.vectorize(eout_f)
    eout_evaluado = eout_v(discretizacion)
    esp_eout = np.mean(eout_evaluado)

    print("Eout con esperanza =",esp_eout, " Eout usando bias + var =", esp_bias+esp_varianza)

    fin = time()

    print("Tiempo transcurrido =", fin-inicio, "segundos")

    plt.plot(discretizacion, g_media_v(discretizacion), color="blue",  linewidth=2.5, linestyle="-", label = "g_media")
    plt.plot(discretizacion, f_v(discretizacion), color="red",  linewidth=2.5, linestyle="-", label = "f")
    plt.plot(discretizacion, eout_evaluado, color="green",  linewidth=2.5, linestyle="-", label = "Eout")
    plt.ylabel('Y')
    plt.xlabel('X')
    plt.legend(loc='upper left')
    plt.show()
