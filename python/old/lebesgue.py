import numpy as np
def lebesgue(x):
    x=np.array(x);m=x.size
    z=np.linspace(-1,1,10000)
    A=np.empty([m,m]);B=np.empty([z.size,z.size])
    for i in list(range(0,m)):
        A[i]=x**(m-i);B[i]=z**(m-i);
    L=np.linalg.norm(np.outer(B,np.linalg.inv(A)),'inf')
    return(L)
