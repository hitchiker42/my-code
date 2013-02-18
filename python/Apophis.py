#!/usr/bin/python3
#Note, Does not work and I can't figure out why.I have spent quite a while looking over this but found nothing
"""Tucker DiNapoli, last modified 25/11/12, original date created unkn, date this version created 25/11/12
program to calculate the position of the asteroid apophis relative to the earth and venus over time
original program was a matlab script from Mark Lyon, I have converted it into python and added quite a bit
the original was delebrately a skeleton of a program to be filled in, i convered this skeleton into python then filled it in"""
#Original discription:Incomplete, simplified, and inaccurate code to calculate the orbit of the astroid apophis\
#using Runge-Kutta fourth order nonadpative time stepping.
from numpy import *
#from scipy.constants import G
import matplotlib as mpl
import  matplotlib.pyplot as plt
import datetime
from itertools import takewhile
from sys import argv
from math import atan2,acos

#Rk4 w/o time dependence
#idea is to update d2x,dx,x for each planet w/first step of rk4 then use new values for step 2, step 3, step 4 then append
#1/6 * (step 1 * 2*step 2 + 2*step 3 + step 4) to all values
#d2n needs to use a fxn to get each new value, dn & n are of the forn n1=n0+dn*dt

G=6.674e-11
m=[5.98e24,1.991e30,2.7e10,4.88e24];me=m[0];ms=m[1];ma=m[2];mv=m[3]
re=1.496e11
dist=lambda y,x:(y[0]-x[0],y[1]-x[1])
mag=lambda x:(x[1]**2+x[0]**2)**0.5
angle=lambda x,y:atan2(dist(x,y)[1]/mag(dist(x,y)),dist(x,y)[0]/mag(dist(x,y)))
#angle=lambda y,x:acos((y[0]-x[0])/mag(x,y))
d2x=lambda y,x,mx:(((G*mx)/(mag(dist(y,x))**2))*mag(dist(y,x))*cos(angle(y,x)))
d2y=lambda y,x,mx:(((G*mx)/(mag(dist(y,x))**2))*mag(dist(y,x))*sin(angle(y,x)))
def apophis(plot,t0,dt,tf,dp):
    """main part of the program, mostly a loop that calculates position and velocity of the objects using simple numerical ode methons
    Takes 5 arguments, a boolean value to plot, inital and final time with a time step and a plot step, give on command line in form of parameter_name value"""
    #e represents earth, s the sun, v venus and a apophis, time is in seconds and position is in meters
    print(t0,tf,dt)
    cnt=0
    date_time=datetime.datetime.today() #not sure day to start
    e=[(1.4960e11,0.0)]
    de=[(0.0,2.9783e4)]
    v=[(-4.128199e10,9.980061e10)]
    dv=[(-3.232230e4,-1.336962e4)]
    a=[(-1.468739e11,-2.230613e10)]
    da=[(9.127491e3,-2.724475e4)]
    s=[(0,0)]
    d2e=[((d2x(de[0],s[0],ms)+d2x(de[0],da[0],ma)),(d2y(de[0],s[0],ms)+d2y(de[0],da[0],ma)))]
    d2a=[((d2x(da[0],s[0],ms)+d2x(da[0],de[0],me)+d2x(da[0],dv[0],mv)),d2y(da[0],s[0],ms)+d2y(da[0],de[0],me)+d2y(da[0],dv[0],mv))]
    d2v=[(d2x(dv[0],s[0],ms)+d2x(dv[0],da[0],ma),d2y(dv[0],s[0],ms)+d2y(dv[0],da[0],ma))]
    de_a=[mag(dist(e[0],a[0]))]
    time=[0]
    for i in range(0,int((tf-t0)/dt)):
        """this is the loop that does work. we have 12 variables, the x and y position and velocity of earth,venus and apophis.
        we need to update each of these values in each iteration of the loop. the positions are going to be posN+1=posN+dt*vN
        while the velocites are going to be updated via differential eq's using the gravitational forces
        for now lets just use velN+1=velN+dt*aN
        """
        s.append((0,0))
        #date_time+=datetime.timedelta(seconds=dt)
        d2e.append(((d2x(e[i],s[i],ms)+d2x(e[i],a[i],ma)),(d2y(e[i],s[i],ms)+d2y(e[i],a[i],ma))))
        de.append((de[i][0]+dt*d2e[i+1][0],de[i][1]+dt*d2e[i+1][1]))
        e.append((e[i][0]+dt*de[i+1][0],e[i][1]+dt*de[i+1][1]))
        d2v.append((d2x(v[i],s[i],ms)+d2x(v[i],a[i],ma),d2y(v[i],s[i],ms)+d2y(v[i],a[i],ma)))
        dv.append((dv[i][0]+dt*d2v[i+1][0],dv[i][1]+dt*d2v[i+1][1]))
        v.append((v[i][0]+dt*dv[i+1][0],v[i][1]+dt*dv[i+1][1]))
        d2a.append(((d2x(a[i],s[i],ms)+d2x(a[i],e[i],me)+d2x(a[i],v[i],mv)),d2y(a[i],s[i],ms)+d2y(a[i],e[i],me)+d2y(a[i],v[i],mv)))
        da.append((da[i][0]+dt*d2a[i+1][0],da[i][1]+dt*d2a[i+1][1]))
        a.append((a[i][0]+dt*da[i+1][0],a[i][1]+dt*da[i+1][1]))
        #above updates everything using forward euler
        time.append(time[i]+dt)
        cnt+=1
        de_a.append(mag(dist(e[i+1],a[i+1])))
    if plot==True:
        """this plots the distance between earth and apophis vs time"""
        t=time[::dp]
        distance=de_a[::dp]
        print("time is of length {} and dist is of length {}".format(len(t),len(distance)))
        plt.plot(t,distance,'*k')
        plt.ylabel("distance")
        plt.xlabel("time")
        plt.show()
        print(de_a[0],de_a[len(de_a)-1])
    #prints inital and final values for position velocity and acceleration for all three moving bodies
    print(e[0],e[len(e)-1],de[0],de[len(de)-1],d2e[0],d2e[len(d2e)-1],v[0],v[len(v)-1],dv[0],dv[len(dv)-1],\
              d2v[0],d2v[len(d2v)-1],a[0],a[len(a)-1],da[0],da[len(da)-1],d2a[0],d2a[len(d2a)-1],\
              "closest distance b/t earth and apophis was {} km".format(min(de_a)),"count = {}".format(cnt),sep='\n')
    return

#def aph_plot():
#    """y(1,2)=earth(x,y) y(3,4)=earth(dx,dy) y(5,6)=venus(x,y) y(7,8)=venus(dx,dy) y(9,10)=ap(x,y) y(11,12)=ap(dx,dy)
#    """
   # norm=lamda x:list(map(lambda i:i/re,x))
#    if dp%i==0:
        # 3rd arg is tick mark&color &  for old args 4th and 5th arguments were both linewidth args b/c matlab is weird
        #what do hold on and off do, can i emulate them with figures?(replaced with mpl syntax, still don't know how they work
        #It seems all the values are normalized wrt the earth's radius
#        plt.plot(e[i][0]/re,e[i][1]/re,'k*',linewidth=3) #earth
#        plt.hold('on')
#        plt.plot(a[i][0]/re,a[i][1]/re,'k*',linewidth=3) #apophis
#        plt.plot(v[i][0]/re,v[i][1]/re,'k*',linewidth=3) #venus
#        plt.hold('off')
        #axis equal #what is this???
#        plt.title(date_time.ctime()) #not sure if this is the right format
#        plt.xlabel("x position (m)")
#        plt.ylabel("y position (m)")
        #lets try this for g
#        g=[(ex,ey),(ax,ay),(vx,vy)]
#        if i*dt<31536000+2*dp*dt: #not sure what this conditional does, number is a year in seconds,
            #what is g, where is it defined trying my intrepretation
            #g might only need to be 1 set of tuples and not an array, if so cut out the [i]
#            g.append([('ex','ey'),('ax','ay'),('vx','vy')])
#            g[i][0]=(e[i][0],e[i][1])
#            g[i][1]=(a[i][0],a[i][1])
#            g[i][2]=(v[i][0],v[i][1])
#            k+=1 
#            plt.hold('on')
#            plt.plot(0,0,'yo',linewidth=5) #Sun???
#            plt.plot(e[i][0]/re,e[i][1]/re,'bo',linewidth=3) #earth
#            plt.plot(a[i][0]/re,a[i][1]/re,'go',linewidth=3) #apophis
#            plt.plot(v[i][0]/re,v[i][1]/re,'mo',linewidth=3) #venus
#            plt.hold('off')
#        else:
            #there were a bunch of g(#,:) i assumed this meant g[#]
            #might need to replace some opperations with maps....
#            plt.hold('on')
#            plt.plot(0,0,'yo',linewidth=5)
            #these three seem to be just lines
#            plt.plot(g[i][0][0]/re,g[i][0][1]/re,'b',linewidth=2)
#            plt.plot(g[i][1][0]/re,g[i][1][1]/re,'g',linewidth=2)
#            plt.plot(g[i][2][0]/re,g[i][2][1]/re,'m',linewidth=2)
            #while these have points again
#            plt.plot(e[i][0]/re,e[i][1]/re,'k*',linewidth=3)
#            plt.plot(e[i][0]/re,e[i][1]/re,'bo',linewidth=3)
#            plt.plot(a[i][0]/re,a[i][1]/re,'go',linewidth=3)
#            plt.plot(v[i][0]/re,v[i][1]/re,'mo',linewidth=3)
#            plt.hold('off')
#        plt.axis([-1.5,1.5,-1.5,1.5])
#        plt.show()

if __name__=="__main__":
    i=1
    plot=False;t0=700000;dt=1000;tf=750000;dp=20
    while i<len(argv):
        if argv[i]=='t0':
            t0=eval(argv[i+1])
            i+=2
        elif argv[i]=='plot':
            plot=eval(argv[i+1])
            i+=2
        elif argv[i]=='tf':
            tf=eval(argv[i+1])
            i+=2
        elif argv[i]=='dt':
            dt=eval(argv[i+1])
            i+=2
        elif argv[i]=='dp':
            dp=eval(argv[i+1])
            i+=2
        else: i+=2
    apophis(plot=plot,t0=t0,dt=dt,tf=tf,dp=dp)
