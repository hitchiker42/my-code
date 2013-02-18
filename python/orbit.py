import numpy as np
from numpy import *
#def euler(y,dt,f):#leftpt
#    yf=[y]#find a way to allocate yf space y*n
#    yf=y+f(y)*dt
#    return(yf)
#def earth_orbit (y,dt,n):
#    yf=[[y]]
#    for i in list(range(1,n)):
#        yf.append([])
#        yf[i+1].append(yf[i][0]+y[i][2]*dt)
#        yf[i+1].append(yf[i][1]+y[i][3]*dt)
#        yf[i+1].append(yf[i][2]+dt*f(r)/Me*Ms)
#        yf[i+1].append(yf[i][3]+dt*f(r)/Me*Ms)
def weird(y,dt):
    yy=y+dt*step(y,dt)
    y=y+list(map(lambda x:x*dt/2,(step(y,dt)+step(yy,dt))))
    return(y)
def step(y,dt):
    me=5.98e+24
    ms=1.991e+30
    G=6.67384e-11
    r=sqrt(y[0]**2+y[1]**2)
    f=lambda r:G*((me*ms)/r**2)
    yn=y
    yn[0]=y[0]+y[2]*dt
    yn[1]=y[1]+y[3]*dt
    yn[2]=y[2]+dt*f(r)/(2*me*ms)
    yn[3]=y[3]+dt*f(r)/(2*me*ms)
    return(yn)
def Rk4(t,dt,y,f):
    s1=dt*f(t,y)
    s2=dt*f(t+(dt/2),y+(s1/2))
    s3=dt*f(t+(dt/2),y+(s2/2))
    s4=dt*f(t+dt,y+s3)
    y_n1=(y+((dt/6)*(s1 +2*s2 +2*s3 +s4)))
    return(y_n1)
