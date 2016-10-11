import numpy as np;
from numpy import *;
import scipy as sci;
import scipy.linalg as la;
import matplotlib as mpl;
import matplotlib.pyplot as plt;
import sympy as sym;
iota = lambda *args: list(np.arange(*args))
lmap = lambda f, *lists: list(map(f,*lists))
# la.lu_factor does the same thing as this
def lu_decomp(A):
    # doing this natively in python is obviously going to be slow
    A = A.copy()
    (n,m) = A.shape
    for k in range(n):
        a = A[k,k]
        for i in range(k+1,n):
            A[i,k]/=a
            for j in range(k+1,n):
                A[i,j] -= A[i,k]*A[k,j]
            #endfor
        #endfor
    #endfor
    return A
def lup_decomp(A):
    # doing this natively in python is obviously going to be slow
    A = A.copy()
    (n,m) = A.shape
    p = np.arange(n)
    k = 0;
    while(k<n):
#        print(A)
        kmax = np.argmax(lmap(abs, A[k:n,k]))+k
#        print(kmax)
        for i in range(n):
            (A[kmax,i],A[k,i]) = (A[k,i],A[kmax,i])
        (p[kmax],p[k]) = (p[k],p[kmax])
#        print(A)
        i = k+1
        while(i<n):
            A[i,k] = A[i,k]/A[k,k]
            j = k+1
            while(j<n):
                A[i,j] = A[i,j] - A[i,k]*A[k,j]
                j+=1
            i+=1
        k+=1
    return (A,p)
def lup_solve(LU,p,b):
    (n,m) = LU.shape
    y = np.zeros(n)
    x = np.zeros(n)
    for i in range(n):
        y[i] = b[p[i]] - reduce(op.add, map(lambda j: LU[i,j]*y[j], np.arange(i-1)))
    for i in range(n-1,0,-1):
        x[i] = y[i] - reduce(op.add,
                             map(lambda j: LU[i,j]*y[j], np.arange(i-1)))/LU[i,i]
    return x

def mat4_rotation_matrix(axis, angle):
    #normalize the given axis
    u = (axis/la.norm(axis));
    # This uses the matrix form of Rodrigues rotation formula
    # R = cos(angle)*I + sin(angle)*[u]x + (1-cos(angle))u⊗u
    # [u]x is the cross product matrix and u⊗u is the outer product
    # [u]x = [[0,-u[2],u[1]],[u[2],0,-u[0]],[-u[1],u[0],0]]

    # Since this is pure python it's going to be slow, so we may
    # as well use the formula explicitly
    I3 = eye(3)
    #cross product matrix
    ux = column_stack(apply_along_axis(lambda x: cross(u,x), 1, I3))
    uouter = outer(u,u)
    R = cos(angle) * I3 + sin(angle)*ux + (1-cos(angle))*uouter
    ret = eye(4);
    for i in range(3):
        ret[i,0:3] = R[i,:]
    return ret
def mat4_scale_matrix(x,y,z):
    return diag(array([x,y,z,1]))

# Shearing is a bit weird, the generic 3d shear matrix is:
# |1 d g 0|
# |b 1 i 0|
# |c f 1 0|
# |0 0 0 1|
#  Shear X: (x,y,z) -> (x, y + bx,z + cx)
#  Shear Y: (x,y,z) -> (x + dy, y,z + fy)
#  Shear Z: (x,y,z) -> (x + gz, y + iz,z)
