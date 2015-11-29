import numpy as np
from numpy import*
import scipy as sci
from scipy.constants import *
from scipy.special import legendre
#int, finite pts
left=lambda y,h:sum(y[:len(y)-1])*h/len(y)
right=lambda y,h:sum(y[1:len(y)])*h/len(y)
midpt=lambda y,h:sum(y)*h/len(y)
trap=lambda y,h:0.5*(sum(y)+y[0]+y[len(y-1)])*h/len(y)-1
simpson=lambda y,h:(2*midpt(y,h)+trap(y,h))/3
def Gaussian_quad(fx,start,end,sample=0.1):
    x=list(range(start,end,sample))
    i=end-start
    quad=0
    for y in enumerate(x):
        quad+=(fx(y[1])*legendre(y[0])*i)
    return(quad)
'''def legendre(x,i):
    l=(1/(2**i)*factorial(i))
    p=lambda x,i:(x**2-1)**i
    y=p(x,i);j=i
    while j>0:
        y=df5()'''
#deritives
df2=lambda f,x,h:(f(x+h)-f(x-h))/(2*h)
df5=lambda f,x,h:(-f(x+2*h)+8*f(x+h)-8*f(x-h)+f(x-2*h))/(12*h)
#functional integrals
fleft=lambda f,x,h:sum(list(map(f,x[0:len(x)-1])))*(h/(len(x)-1))
fright=lambda f,x,h:sum(list(map(f,x[1:len(x)])))*(h/(len(x)-1))
ftrap=lambda f,x,h:(fright(f,x,h)+fleft(f,x,h)/2)
def mid(x):
    y=[]
    n=0
    for i in x[1:]:
        n+=1
        y.append((x[n]+x[n-1])/2)
    return(y)
fmid=lambda f,x,h:sum(list(map(f,mid(x))))*(h/(len(x)-1))
fsimp=lambda f,x,h:(2*fmid(f,x,h)+ftrap(f,x,h))/3
#ODE
def euler(y,t,dt,f,n=1):#leftpt
    """Forward euler method, requires a function f with 2 arguments, n determines the number of iterations"""
    if not hasattr(y,'__iter__'):
        y=[y]
    yf=[y]
    tt=t-dt
    for i in list(range(0,n)):
        yf.append([])
        tt=tt+dt
        for j,q in enumerate(yf[i]):
            yf[i+1].append(yf[i][j]+dt*f(yf[i][j],tt))
    return(yf)
def back_euler(y,t,dt,f,n=1):#rightpt
    if not hasattr(y,'__iter__'):
        y=[y]
    yf=[y]
    tt=t-dt
    for i in list(range(0,n)):
        yf.append([])
        tt=tt+dt
        for j,q in enumerate(yf[i]):
            yf[i+1].append(yf[i]+1/(dt*f(y[i],tt)))
    return(yf)
def imp_trap():
    pass
def exp_trap():
    pass
def Rk4(t,dt,y,f,n=1):
    yf=[]
    tt=t-dt
    for j in list(range(0,n)):
        yf.append([])
        tt=tt+dt
        for i in list(range(0,len(y))):
            s1=dt*f(tt,yf[j][i])
            s2=dt*f(tt+(dt/2),yf[j][i]+(s1/2))
            s3=dt*f(tt+(dt/2),yf[j][i]+(s2/2))
            s4=dt*f(tt+dt,yf[j][i]+s3)
            yf[j+1].append((yf[j][i]+((dt/6)*(s1 +2*s2 +2*s3 +s4))))
    return(yf)
#def bk_eulen(y,t,dt,f,n):#rightpt

            
