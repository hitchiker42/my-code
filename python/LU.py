import numpy as np;from numpy import array,copy,indices,zeros_like;
def Lu(a):
    a=array(a);l=copy(a);u=copy(a);#arrays
    n=a.shape[0];m=a.shape[1];err=1;#dimensions
    z=indices([n,m])[1];i=-1;#indices
    g=lambda x_,y_,i_,n_:x_[i_]-n_*y_[i_]
    for p in z:#LU decomposition
        i+=1;lp=l[i][i];up=u[m-1-i][m-1-i];r=m-1-i;
        if a[i][i]==0:err=0;break
        for j in p:
            q=n-1-j
            if j>i:
                lmult=l[j][i]/lp
                for k in list(range(0,m)):
                    l[j][k]=g(l[j],l[i],k,lmult)
            if q<r:
                umult=u[q][r]/up
                for k in list(reversed(range(0,m))):
                    u[q][k]=g(u[q],u[r],k,umult)
    for k in list(range(0,m)):u[k][k]=1
    if err!=True:Print("zero pivot encountered");return
    else: return l,u

def back_sub(A,b):
    A=array(A);LU=Lu(A);L=LU[0];U=LU[1];
    c=zeros_like(b);x=zeros_like(b);m=A.shape[0]
    for i in list(range(0,m)):
        j=0;sumc=b[i]
        while j<i:
            sumc-=c[j]*L[i][j];j+=1;
        c[i]=sumc/L[i][i]
    for i in list(range(0,m)):
       j=0;sumx=c[i]
       while j<i:
           sumx-=x[j]*U[i][j];j+=1;
       x[i]=sumx/U[i][i]
    return(c,x)
