'''def adp_trap(f,x,h,tol):
    a0=x[0]
    b0=x[len(x)-1]
    c0=a+b/2
    n=len(x)
    sac0=ftrap(f,linspace(a,c,n),h)
    scb0=ftrap(f,linspace(c,b,n),h)
    sab0=ftrap(f,x,h)
    sac=sac0;sab=sab0;scb=scb0;a=a0;b=b0;c=c0;sabf=0
    if abs(sab-(sac+sab))<3*tol:
        return(sab)
    while():
        while abs(sab-(sac+sbc))<3*tol*((b0-a0)/(b-a)):
            sab=sac;b=c;c=(a+b)/2
            sac=ftrap(f,linspace(a,c,n),h)
            scb=ftrap(f,linspace(c,b,n),h)
        sabf+=(sac+sab)
        if(b=c):
            break
        b=c0;
        a=b
        c=(a+c0)/2
        sab=ftrap(f,linspace(a,b,n))
        sac=ftrap(f,linspace(a,c,n))
        scb=ftrap(f,linspace(c,b,n))'''
def adp_trap(f,x,h,tol):
    """ """
    n=len(x)
    mid=lambda x:(x[0]+x[n-1])/2
    c=mid(x)
    def trapz(f,h,x):
        ans=ftrap(f,h,x)
        a=linspace(x[0],mid(x),n)
        b=linspace(x[0],mid(x),n)
        return((ans,a,b))
    while(ans>real+tol):
        for i in total.copy():
            total.append(trapz(f,h,i[1]))
            total.append(trapz(f,h,i[2]))
