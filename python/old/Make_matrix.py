from numpy import zeros;
def matrix_make(n,f):
    x=zeros([n,n]);
    for i in list(range(0,n)):
        for j in list(range(0,n)):
            x[i][j]=f(i,j)
    return(x)
