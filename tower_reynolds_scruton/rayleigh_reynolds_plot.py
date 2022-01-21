#!/usr/bin/env python
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
import seaborn as sns
import math

def rayleigh(x, sigma):
    return x/(sigma**2)*math.exp(-x**2/(2*sigma**2))

def main():
    # air constants
    nu = 1.48e-5

    # assume uniform inflow, Rayleigh distributed
    meanu = 8.0

    u = np.linspace(1.0, 24, 100)

    pu = []
    for element in u:
        pu.append(rayleigh(element, meanu/(math.sqrt(math.pi/2))))

    # plot
    sns.set_theme()
    sns.set_style("white")

    fig, ax = plt.subplots()
    ax.plot(u*4.0/nu, pu)
    ax.plot(u*5.0/nu, pu)
    ax.plot(u*6.0/nu, pu)
    ax.set_xlabel("Re [-]")
    ax.set_ylabel("P(Re) [-]")
    ax.legend(['D = 4.0 m', 'D = 5.0 m', 'D = 6.0 m'])

    plt.savefig('Rayleigh_Reynolds.pdf')
    plt.show()


if __name__ == '__main__':
    main()