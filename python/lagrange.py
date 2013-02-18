import numpy as np;
def lagrange(x,fx):
    x=np.matrix(x);n=len(x);P=list(range(0,n));k=-1;L=P.copy()
    for i in x:
        ++k
        for j in range(list(0,n)):
           if k!=j: P[k]=(x[0]-x[k])/(x[j]-x[k])
        s=P[i]*fx[i]
