#include "Misc.h"
//let pd = partial deritive
/*Rk4 let dx/dt=f(t,x) && x(t_0)=x_0, h=step size
 x_n+1 = x_n + h*1/6 (k_1+2k_2+2k_3+k_4)
 t_n+1 = t_n + h //duh
 k_1 = f(t_n,x_n)
 k_2 = f(t_n + 0.5h,x_n + h*0.5k1)
 k_3 = f(t_n + 0.5h,x_n + h*0.5k2)
 k_4 = f(t_n+h,x_n + h*k_3)*/
//might switch to taking an array && an index as parameters
double rk4 (double (*fp)(double,double),double x,double t,double h);

/*Crank-Nicolson, used for actually solving stuff
  let pdu/pdt = F(u,x,t,pdu/pdx,pd^2u/pdt^2,pd^3u/pdt^3)
  let u_i^n = u(i\delx,n\delt), we want u_i^n+1(i.e next time step)
  (u_i^n+1 - u_i^n)/\delt=0.5*(F_i^n+1+F_i^N) where any deritive in F
  is replaced by an equivlant 5pt stencil.
  Need to solve a linear eq to find u_i^n+1 for each step.(ie implict method)*/
//no idea what this should be 

/*5_pt stencil,built in upto 4th deritive, if given higher for now just raise
 *some kind of error*/
double Stencil_5pt (double arr[],int i,int order,double (*fp)(double),double h);
/* typedef struct {
   int x_init;
   int range;
   double del_t;
   double del_x;
   double* x;
   double (*fn)(double,double)
} kdv_info;
Idea for a struct to store info for a given set of conditions*/
