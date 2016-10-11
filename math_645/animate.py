import numpy as np;
import scipy as sci;
import scipy.linalg as la;
import matplotlib as mpl;
import matplotlib.pyplot as plt;
from mpl_toolkits.mplot3d import Axes3D;
import sys,time;
from numpy import *

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
# Assumes array is an array of 3 tuples, representing a 2d point + a
# homogenous coordinate.
def transform_array(arr, trans):
    return np.apply_along_axis(lambda x: np.dot(trans,x), 1, arr)
def rotate_array(arr, angle):
    trans = mat3_rot(np.eye(3),angle)
    return transform_array(arr, trans)
def scale_array(arr, x, y):
    trans = mat3_scale(np.eye(3), x, y)
    return transform_array(arr, trans)
def translate_array(arr, x, y):
    trans = mat3_translate(np.eye(3), x, y)
    return transform_array(arr, trans)
def read_jpg(file):
    img = plt.imread(file);
    (M,N) = shape(img)
    tmp = np.stack(np.meshgrid(np.linspace(0,1,),
                               np.linspace(0,1,2240)),axis=-1)
    tmp = reshape(tmp,(tmp.shape[0]*tmp.shape[1],2))
    #need to add an extra column of 1s to tmp 
# Draw a letter N and move it around the screen
def moveN():
    D = array([(0,0,1),(0,8,1),(5.5,1.58,1),(5.5,8,1),
               (6,8,1),(6,0,1),(0.5,6.42,1),(0.5,0,1)])
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
            D = transform_array(mat, D)
            plt.fill(D[:,0],D[:,1],color[j%7])
            plt.pause(0.1)
    plt.hold(False)
    plt.ioff()
    plt.show()
    return
