#!/usr/bin/env python
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy as np

class color:
    """ class allowing easy access to terminal output coloring"""
    PURPLE = '\033[95m'
    CYAN = '\033[96m'
    DARKCYAN = '\033[36m'
    BLUE = '\033[94m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'
    END = '\033[0m'

def main():
    S = 0.2       # Strouhal number (constant)
    rho = 1.225     # air density [kg/m^3]
    mu = 1.81e-5    # dynamic viscosity [Pa*s]
    
    # St = f * D / U
    # Re = rho * U * L / mu
    # where D = L (tower diameter and characteristic length, respectively)

    L = np.linspace(1, 10, 100)
    R = np.linspace(800, 20000, 100)

    L, R = np.meshgrid(L, R)
    U = R * mu / (rho * L)
    F = S * U / L

    #fig, ax = plt.subplots(subplot_kw={"projection": "3d"})
    #surf = ax.plot_surface(L, R, F, cmap=cm.coolwarm, linewidth=0, antialiased=False)

    plt.contourf(L, R, F, 100, cmap='RdGy')
    plt.colorbar()

    #ax.set_xlabel('L [m]')
    #ax.set_ylabel('Re [-]')

    #fig.colorbar(surf, shrink=0.5, aspect=5)

    plt.show()


if __name__ == '__main__':
    main()