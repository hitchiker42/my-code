import numpy as np;
from numpy import *;
import scipy as sci;
import scipy.linalg as la;
import matplotlib.pyplot as plt;
import operator as op;
from functools import reduce;
lmap = lambda f, *lists: list(map(f,*lists))
# 1.2 33,34
def find_interpolating_polynomial(points):
    degree = len(points)
    x,y = zip(*points)
    A = np.array(lmap(lambda x: lmap(lambda t: x**t, range(degree)), x))
    b = np.array(y)
    x = la.solve(A,b)
    return (A,b,x)
def plot_interpolating_polynomial(points, coefficents, filename):
    x = coefficents
    t = np.linspace(0,10,100)
    f = np.vectorize(lambda t: reduce(op.add,
                                      lmap(lambda i: x[i]*t**i,
                                           range(len(points)))))
    plt.plot(t,f(t));
    plt.plot(*list(zip(*points)), '.');
    plt.savefig(filename);
    plt.clf()
def problem_33():
    points = ((1,12),(2,15),(3,16))
    (A,b,x) = find_interpolating_polynomial(points)
    plot_interpolating_polynomial(points, x, "problem_33.png")
def problem_34():
    points = ((0,0),(2,2.9),(4,14.8),(6,39.6),(8,74.3),(10,119))
    (A,b,x) = find_interpolating_polynomial(points)
    plot_interpolating_polynomial(points, x, "problem_34.png")
def problem_28():
    A = lambda t: t*np.array([27.6,3100,250])
    B = lambda t: t*np.array([30.2,6400,360])
                
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
        A[[kmax,k]] = A[[k,kmax]]
        p[[kmax,k]] = p[[k,kmax]]
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
    i = j = 0
    while(i<n):
        sum = 0
        j = 0
        while(j<=(i-1)):
            sum += LU[i,j]*y[j]
            j+=1
        y[i] = b[p[i]] - sum
        i+=1
    i = (n-1)
    while(i>=0):
        sum = 0
        j = i+1
        while(j<n):
            sum += LU[i,j]*x[j]
            j+=1
        x[i] = (y[i] - sum)/LU[i,i]
        i-=1
    return x
def do_problem_5(datafile):
    print_arr = lambda x,y: \
                print("{} =\n{}".format(y,
                                        np.array2string(x,precision = 6,
                                                        suppress_small = True,
                                                        separator=',')))
    np.set_printoptions(precision=6)
    A = loadtxt(datafile)
    (n,m) = A.shape
    (LU,p) = lup_decomp(A)
    (LU_control,p_control) = la.lu_factor(A)
    ## Check that my LU is equal to the actual LU, with a small
    ## tolerence for floating point rouding errors
    assert(np.allclose(LU,LU_control));
    
    L = np.tril(LU)
    U = np.triu(LU)
    P = np.zeros((n,n))
    for i in range(n):
        L[i,i] = 1
        P[i,p[i]] = 1
    print("Problem 5:")
    print("LUP decomposition")
    print_arr(L,"L")
    print_arr(U,"U")
    print_arr(P,"P")
    print("Solving Ax = b for various values of b")
    
    b1 = array([2,3,-1,5,7],dtype=float)
    x1 = lup_solve(LU,p,b1)
    x1_control = la.lu_solve((LU_control,p_control),b1)
    assert(np.allclose(x1,x1_control));
    print_arr(b1,"b1")
    print_arr(x1,"x1")
    
    b2 = array([15,29,8,4,-49],dtype=float)
    x2 = lup_solve(LU,p,b2)
    x2_control = la.lu_solve((LU_control,p_control),b2)
    assert(np.allclose(x2,x2_control));
    print_arr(b2,"b2")
    print_arr(x2,"x2")

    b3 = array([8,-11,3,-8,-32],dtype=float)
    x3 = lup_solve(LU,p,b3)
    x3_control = la.lu_solve((LU_control,p_control),b3 )
    assert(np.allclose(x3,x3_control));
    print_arr(b3,"b3")
    print_arr(x3,"x3")

