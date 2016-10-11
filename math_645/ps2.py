import numpy as np;
import scipy as sci;
import scipy.linalg as la;
import matplotlib as mpl;
import matplotlib.pyplot as plt;
import sympy as sym;
from sympy.abc import x,y
iota = lambda *args: list(np.arange(*args))
lmap = lambda f, *lists: list(map(f,*lists))
# 3, section 1.7 9,12,19
def problem_3():
    mat_9 = sym.Matrix([[1,-3,2],[-3,9,-6],[5,-7,x]]).T
    print(mat9.det()) #prints 0, obviously mat_9[1] == -3*mat_9[0]
    mat_12 = sym.Matrix([[2,-4,1],[-6,7,-3],[8,h,4]]).T
    mat_19 = sym.Matrix([[-8,12,-4],[2,-3,-1]]).T
# 4, section 1.8 29,30,26
def problem_4():
