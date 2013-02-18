import numpy as np
import scipy as sp
import matplotlib as mpl
import matplotlib.pyplot as plt

N=10
h=1/(N-1)
x=np.linspace(0,1,9)
f=np.exp(x)
fdleft=(f[2:]-f[1:f.size-1])/h
error =fdleft-f[2:]
np.linalg.norm(error)
