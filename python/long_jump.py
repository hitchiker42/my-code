#!/usr/bin/python3
"""Written by Tucker DiNapoli, created xx/11/12, last edited 4/12/12"""
from numpy import *
from math import *
import matplotlib as mpl
import matplotlib.pyplot as plt
from scipy.interpolate import interp1d,InterpolatedUnivariateSpline,sproot,UnivariateSpline
from sys import argv,stdout

#Rk4 w/o time dependence
def Rk4(f,y,dt): #equations for position in comments
    k1=f(y) #x1=y+(dt*k1)
    k2=f(y+dt/2*k1) #x2=y+(dt/2*k2)
    k3=f(y+dt/2*k2) #x3=y+(dt/2*k3)
    k4=f(y+dt*k3) #x4=y+(dt*k4)
    return(y+dt/6*(k1+2*k2+2*k3+k4)) #x=x+dt/6*(x1+2*x2+2*x3+x4)

#The length of each jump is x(tf), where the air time, tf ,is determined by the condition y(tf) = 0.

def long_jump(V0,p,x=0,dt=0.1,f=stdout):
    """calculate parabolic trajectory w/drag via iteration using a fixed step size Rk4 method"""
    Dc=-0.000225*p
    x=[x]
    y=[0]
    v=[(V0*cos(pi/8),V0*sin(pi/8))]
    dvx=lambda x:Dc*x**2
    dvy=lambda y:Dc*y**2-9.81
    i=0
    while y[i]>-0.1:
        v.append((Rk4(dvx,v[i][0],dt),Rk4(dvy,v[i][1],dt)))
        x.append(x[i]+dt/2*(v[i][0]+v[i+1][0]))
        y.append(y[i]+dt/2*(v[i][1]+v[i+1][1]))
        i+=1
        #position step is simplified but it should be accurate enough, if not the rk4 version is outlined in that function
    fxn=UnivariateSpline(x,y)
    dist=fxn.roots()
    print("The distance jumped was {:6g} m".format(dist[len(dist)-1]),file=f)
    #elif V0==0:
    #    v_i=-v[len(v)-1][1]/sin(pi/2)
    #    v_init=(v_i*cos(pi/8),v_i*sin(pi/8))
    #    print("The inital velocity was {} m/s or \nThe vector {}".format(v_i,v_init),file=f)
    return
def jump_speed(xf,p):
    Dc=-0.000225*p
    theta=22.5
    y=[0]#y starts at 0 and ends at 0, goes up then down
    x=[xf]#x starts at xmax and decreases untill x=0
    dvy=lambda y:Dc*y**2-9.81
    dvx=lambda x:-Dc*x**2
    #x intial= y inital, at y max dy=0 so Dc*dy*82=9.81, but x=???
    #v inital= [(V0*cos(pi/8),V0*sin(pi/8))]
    #idea, use shooting method, no time to impliment

if __name__=="__main__":
    i=1
    V0=10;p=1;dt=0.1;x=0;xf=None
    f=open("ans.txt",'a')
    print(argv,file=f)
    while i<len(argv):
        if argv[i]=="V0":
            V0=eval(argv[i+1])
            i+=2
        elif argv[i]=="p":
            p=eval(argv[i+1])
            i+=2
        elif argv[i]=="dt":
            dt=eval(argv[i+1])
            i+=2
        elif argv[i]=="x":
            x=eval(argv[i+1])
            i+=2
        elif argv[i]=="xf":
            xf=eval(argv[i+1])
            i+=2
        else: i+=2
    if xf is not None:
        jump_speed(xf,p)
    else:long_jump(V0,p,x=x,dt=dt,f=f)
