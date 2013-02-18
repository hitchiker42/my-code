#!/usr/bin/python3
"""give the position, velocity and acceleration of 4 bodies under the infulence of the gravitational force"""
#Incomplete for now, but most of the theory is here, this was written to calculate the apophsis problem using all gravitational interactions
dist=lambda x,y:((x[0]-y[0])**2+(x[1]-y[1])**2)**0.5
angle=#formula for the angle b/t 2 planets
idef f(x,y,m):#mass is mass of y
    return(G*m/dist(x,y)**2)*angle(x,y)
#shortcut for last step of rk4
rk4_add=lambda w,n:dt/6*(w[0][n][0]+2*w[1][n][0]+2*w[2][n][0]+w[3][n][0]),dt/6*(w[0][n][1]+2*w[1][n][1]+2*w[2][n][1]+w[3][n][1])

def main(x0,dx0,mass,dt,t0=0,tf):#x0 &dx0 are lists of 4 tuples, and mass is a list of 4 values
    if tf<t0:
        return("error, tf less than t0")
    i=0
    x=[x0]#x0 should be a list so x is a list of lists of tuples, same with the next 2
    dx=[dx0]
    d2x=[[]]#except we don'n know this yet so we wait till after the first iteration
    while t<tf:
        n=[[x[i]],[(0,0),(0,0),(0,0),(0,0)],[(0,0),(0,0),(0,0),(0,0)],[(0,0),(0,0),(0,0),(0,0)]]#temp lists to hold intermediate rk4 values
        dn=[[dx[i]],[(0,0),(0,0),(0,0),(0,0)],[(0,0),(0,0),(0,0),(0,0)],[(0,0),(0,0),(0,0),(0,0)]]
        d2n=[[(0,0),(0,0),(0,0),(0,0)],[(0,0),(0,0),(0,0),(0,0)],[(0,0),(0,0),(0,0),(0,0)],[(0,0),(0,0),(0,0),(0,0)]]
        #some better way to do this then redefine them every time? (answer might be no)
        for m in range(0,4):#for the 4 lists in n,dn&d2n
            if m==2 or m==3:
                d_t=dt/2
            else:d_t=dt
            for n in range(0,4):#for the four tuples in each of those lists
                for l in range(0,4):#calculate the gravitational acceleration
                    if l!=n:
                        d2n[m][n][0]+=f(n[m][n][0],n[m][l][0])
                        d2n[m][n][1]+=f(n[m][n][1],n[m][l][1])
                #calculate velocity and position, each is only an intermedaite value in rk4
                dn[m][n+1][0]=dx[m][n+1][0]+d_t*d2n[m][n][0]
                dn[m][n+1][1]=dx[m][n+1][1]+d_t*d2n[m][n][1]
                n[m][n+1][0]=x[m][n+1][0]+d_t*dn[m][n][0]
                n[m][n+1][1]=x[m][n+1][1]+d_t*dn[m][n][1]
        #sum the rk4 values and append to the actual x,dx&d2x
        x.append([rk4_add(n,0),rk4_add(n,1),rk4_add(n,2),rk4_add(n,3)])
        dx.append([rk4_add(dn,0),rk4_add(dn,1),rk4_add(dn,2),rk4_add(dn,3)])
        d2x.append([rk4_add(d2n,0),rk4_add(d2n,1),rk4_add(d2n,2),rk4_add(d2n,3)])
        if i==0:
            #retroactivly set inital acceleration
            d2x[0]=[(d2n[0][0][0],d2n[0][0][1]),(d2n[0][1][0],d2n[0][1][1]),(d2n[0][2][0],d2n[0][2][1]),(d2n[0][3][0],d2n[0][3][1])]
        i+=1
        t+=dt#Important step, loop will never terminate otherwise
#plot
#if we need lists of x&y componants it'll be messy but
#list(zip(x[0][0],x[0][1],x[0][2]....x[0][n])), then replace 0 with 1 then 2 then 3 and we'll have our lists
#argparse if needed

if __name__=="__main__":
    #parse arguments & run fxn
