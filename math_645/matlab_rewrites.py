import numpy as np;
import scipy as sci;
import scipy.linalg as la;
import matplotlib.pyplot as plt;
import sys,time
from numpy import *
iota = lambda *args: list(np.arange(*args))
lmap = lambda f, *lists: list(map(f,*lists))
def interactive_reduce(A):
    menu = \
"""            OPTIONS
 < 1>  Swap two rows: A[[i,j],:] = A[[j,i],:],
 < 2>  Multiply a row by a scalar: A[i,:] *= c,
 < 3>  Replace:  A[i,:] -= c * A(j,:),
 < 4>  Display current matrix,
 <-1>  Undo last operation,
 < 0>  Quit"""

    def swap_rows(A):
        (i,j) = (int(input("First row:")),
                 int(input("Second row:")))
        A[(i,j),:] = A[(j,i), :]
    def scale_row(A):
        (i,c) = (int(input("Row:")),
                 int(input("Multiplier:")));
        A[i] *= c
    def subtract_row(A):
        (i,j,c) = (int(input("First row:")),
                   int(input("Second row:")),
                   int(input("Scond row multiplier:")));
        A[i] -= c*A[j]
    dispatch = [swap_rows, scale_row, subtract_row];
    print(menu)
    while True:
        try:
            print(A)
            oldA = A.copy
            op = int(input("Select operation:"))
            if(op < 0):
                A = oldA
            elif(op == 0):
                break
            elif(op >= 4):
                continue
            else:
                dispatch[op-1](A)
        except Exception as exn:
            print(exn.args)
        #endif
    #endwhile
    return A
#enddef


def plane_transform(A,num_iters):
    nn = 64
    color = ("b.","r.","g.","m.","c.","y.","k.")
    cc1 = ("b *","r *","g *","m *","c *","y *","k *")
    cc2 = ("b o","r o","g o","m o","c o","y o","k o")
    V = np.vstack((np.cos(np.linspace(0,2*np.pi,nn)),
                   np.sin(np.linspace(0,2*np.pi,nn))))
    plt.plot(V[0],V[1],"b."), plt.show()
    plt.ion()
    plt.hold(True)#don't erase figure after display
    plt.plot(V[0,1],V[1,1],"b*")
    plt.plot(V[0,nn/4],V[1,nn/4],"bo")
    for i in range(num_iters):
        #plt.waitforbuttonpress()
        V = np.dot(A,V)
        plt.plot(V[0],V[1],color[i%7])
        plt.plot(V[0,1],V[1,1],color[i%7])
        plt.plot(V[0,nn/4],V[1,nn/4],color[i%7])
    plt.hold(False)
    plt.ioff()
    plt.close()

def mat3_translate(mat,x,y):
    mat[0,2] += mat[0,0]*x + mat[0,1]*y
    mat[1,2] += mat[1,0]*x + mat[1,1]*y
    return mat
def mat3_scale(mat,x,y):
    mat[0,0] *= x
    mat[1,1] *= y
    return mat
#def mat3_shear(mat,x,y):
# rotates around the origin by angle clockwise
def mat3_rot(mat,angle):
    #a b*c -s    ac-cs bc-ds
    #c d s  c -> as+cc bs+dc
    (x,y,z,w) = (mat[0,0],mat[0,1],mat[1,0],mat[1,1])
    (c,s) = (cos(angle),sin(angle))
    mat[0,0] = x*c-z*s
    mat[0,1] = y*c-w*s
    mat[1,0] = x*s+z*c
    mat[1,1] = y*s+w*c
    return mat

def moveN():
    D = array([(0,0),(0,8),(5.5,1.58),(5.5,8),(6,8),(6,0),(0.5,6.42),(0.5,0)])
    D1 = array([*D.T,[1,1,1,1,1,1,1,1]])
    angle = pi/18
    mat = mat3_rot(eye(3),angle)
    color = ['y','m','c','r','g','b','k']
    
    plt.hold(True)
    plt.ion()
    plt.axis([-30,30,-30,30])
    plt.fill(D[0],D[1],'r')
    
    for i in range(5):
        mat = mat3_translate(mat,i,i)
        for j in range(36):
            mat = mat3_rot(mat, angle)
            D = np.dot(mat,D1.copy())
            plt.fill(D[0],D[1],color[j%7])
            plt.pause(0.1)
    plt.hold(False)
    plt.ioff()
    plt.show()
    return
