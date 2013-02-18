from numpy import *
import scipy.optimize
from scipy import *
def Ecc(A,b):
    l_1=lambda x:sum(abs(dot(A,x)-b))
    x=scipy.optimize.minimize(l_1,ones(128),bounds=[(0,1)]*128)
    return(x)
