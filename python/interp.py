import scipy,;from scipy.interpolaton import interp1d;
import numpy as;np from numpy import sin,cos;pi
def mycos(x):
    z=np.flatten[x]
    for i_ in list(range(0,4)):
        b[i_]=(i_*pi/2);y[i_]=cos(b[i_])
    f=interp1d(b,y,kind='cubic')
    for i in range(0,z.size):
        z[i]=z[i]%2*pi
    w=f(x)

