#!/usr/bin/python3
"""Tucker DiNapoli, numerical analysis extra credit problem, written 25/11/12, last modified 25/11/12
Crank Nicolson solver for the equation dy/dy=y**2-y**3, with y(0)=δ and 0<=t<=2/δ"""
from sys import argv
#assuming this is 1-D,because it is...so the formula to use is
#∂y/∂t=F(y,x,t,∂y/∂x,∂**2y/∂x**2) and y(iΔx,nΔt)=y_{i}^{n}
#the iteration step will be
#(y_{i}^{n+1}-y_{i}^{n})/Δt=0.5*[F_{i}^{n+1}(...)+F_{i}^{n+1}]
#above is for partial ode's, but it took me a little while to write it so i'm leaving it in
dy=lambda dt,y:dt*(y**2-y**3) #deritive of y wrt t
dy2=lambda dt,y:dt*(2*y-3*y**2) #2nd deritive of y wrt t
yi1=lambda dt,y:y+dy(dt,y)/dy2(dt,y) #one step of a newton method to find yn+1 from yn
yf1=lambda dt,y:y+(dy(dt,y)) #yn+1 given as  yn +dy/dt
yn1=lambda dt,y:(1/2)*(yi1(dt,y)+yf1(dt,y)) #simple crank-nicolson step
yn1x=lambda dt,y,yn:(1/2)*(yi1(dt,yn)+yf1(dt,y)) #crank nicolson allowing for yn+1 to be calculated with more than one step of precision

def c_n_solver(dt,y,n,t_init=0,f=dy,tol=0.1):
    """ode solver which impliments a crank nicolson method for n steps starting at y and specifically solves the equation dy/dt=y**2-y**3"""
    if t_init<0 or n<=0 or dt<=0:
        print("for this solver time must always be positive, and increasing")
        return
    elif not n==int(n):
        print("the number of steps n must be an integer")
        return
    elif (t_init+dt*n)>2/y:
        print("t can not go above 2/y(0)")
        return
    d=y
    y=[d]
    t=map(lambda x:t_init+x*dt,range(0,n+1))
    for i in range(0,n+1):
        cnt=0
        y.append(yn1(dt,y[i]))
        while abs(abs(y[i+1])-abs(yn1x(dt,y[i],y[i+1])))>=tol and cnt<10:
            y[i+1]=yn1x(dt,y[i],y[i+1])
            cnt+=1
        y[i+1]=yn1x(dt,y[i],y[i+1])
    return (y)

if __name__=="__main__":
    if len(argv)<4:
        print("not enough arguements")
    elif len(argv)==4:
        print(c_n_solver(eval(argv[1]),eval(argv[2]),eval(argv[3])))
