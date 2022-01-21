#!/usr/bin/env python
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
import seaborn as sns
import math


def peak_amp(B, C, SG):
    return B/math.sqrt(C + SG**2)


def main():
    # air constants
    nu = 1.48e-5

    B = 0.385
    C = 0.120
    St = 0.2
    
    

    SG = np.linspace(0.01, 10, 10000)
    #mz = np.linspace(1, 10, 100)
    #SG = 2 * math.pi**3 * St**2 * (mz)

    A = []
    for element in SG:
        A.append(peak_amp(B, C, element))

    # plot
    fig, ax = plt.subplots()
    ax.plot(SG, A, label=r"$A^*_{MAX} = 0.385/\sqrt{0.120+S_G^2}$")
    ax.semilogx()
    ax.set_xlabel("$S_G$ [-]")
    ax.set_ylabel("$A^*_{MAX}$ [-]")
    ax.set_title("Sarpkaya's empirical equation")
    ax.tick_params(axis="both", labelsize=14)
    ax.legend()

    plt.savefig('Skop_Griffin_Sarpkaya_Empirical.pdf')
    plt.show()


if __name__ == '__main__':
    main()
