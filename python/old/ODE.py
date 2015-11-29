import numpy as np
from numpy import*
import scipy as sci
from scipy.special import legendre
import functools,itertools
#Integral functions
left=lambda values,interval:sum(values[:len(values)-2])*((interval/(len(values)-2)))
right=lambda values,interval:sum(values[1:])*((interval/(len(values)-2)))
midpt=lambda values,interval:(sum(values[1:len(values)-2])+(.5*values[0]+.5*values[len(values)-1]))*((interval/(len(values)-2)))
trap=lambda values,interval:(left(values,interval)+right(values,interval))/2
simpson=lambda values,interval:(2*midpt(values,interval)+trap(values,interval))/3
def Gaussian_quad(fx,start,end,sample=0.1):
    x=list(range(start,end,sample))
    i=end-start
    quad=0
    for y in enumerate(x):
        quad+=(fx(y[1])*legendre(y[0])*i)
    return(quad)
#Ode Solvers
def Rk4(t,dt,y,f):
    s1=f(t,f(t,y))
    s2=f(t + dt/2,y+ dt/2 *s1)
    s3=f(t + dt/2,y+ dt/2 *s2)
    s4=f(t + dt,y + dt *s3)
    y_n1=(y+dt/6*(s1 +2s2 +2s3 +s4))
    return(y_n1)
def Rk4_list=(n,t,dt,y,f):
    y=list(y)
    i=-1
    while n>0:
        i+=1
        y+=Rk4(t+(i*dt),dt,y[len(y)-1],f)
        n-=1
    return(y)

   
