#include "Misc.h"
double rk4 (double (*fp)(double,double),double x,double t,double h){
  double k_1,k_2,k_3,k_4;
  k_1 = (*fp)(t,x);
  k_2 = (*fp)(t+0.5*h,x+0.5*h*k_1);
  k_3 = (*fp)(t+0.5*h,x+0.5*h*k_2);
  k_4 = (*fp)(t+h,x+h*k_3);
  return x + h*(1.0/6)*(k_1+2*k_2+2*k_3+k_4);
}
double Stencil_5pt (double *x,int i,int len,int order,double h){
  unless(0<order<5,{return NAN;});
  double inf=INFINITY;
  double x_i_2=inf,x_i_1=inf,x_i=inf,x_i1=inf,x_i2=inf;
  switch (i) {
  case 0:
    x_i_2=x[i+2];
  case 1:
    x_i_1=x[i+1];
    break;
  }
  if (i>=len-1){
    x_i1=x[i-1];
    if (i=len){
      x_i2=x[i-2];
    }
  }
  x_i=x[i];
  if(x_i2==inf){x_i2=x[i+2];}
  if(x_i1==inf){x_i1=x[i];}
  if(x_i_1==inf){x_i_1=x[i-1];}
  if(x_i_2==inf){x_i_2=x[i-2];}
  
  switch (order) {
  case 0:
    return x_i;
  case 1:
    return (-(x_i2)+8*(x_i1)-8*(x_i_1)+
            (x_i_2))/12*h;
  case 2:
    return (-(x_i2)+16*(x_i1)-30*(x_i)+
            16*(x_i_1)-(x_i_2))/12*pow(h,2);
  case 3:
    return ((x_i2)-2*(x_i1)+2*(x_i_1)-
            (x_i_2))/2*pow(h,3);
  case 4:
    return ((x_i2)-4*(x_i1)+6*(x_i)-4*(x_i_1)+
            (x_i_2))/pow(h,4);
  }
}
double secant_meth(double x1,double x2,double (*fp)(double),double err){
  double f_x1=(*fp)(x1);
  double f_x2=(*fp)(x2);
  double x_i =(x1-f_x1*((x1-x2)/(f_x1-f_x2)));
  if(x_i-x1>err){
    return secant_meth(x_i,x1,fp,err);
  } else {
    return x_i;
  }
}
double* map(double* x,int l,double(*fp)(double)){
  int i;
  double* result=malloc(l*sizeof(double)); 
#pragma opm parallel for 
  for (i=0;i<l;i++){
    result[i]=(*fp)(x[i]);
  }
  return result;
}
/*  
  Periodic boundry conditions check
  double x_i_2=inf,x_i_1=inf,x_i=inf,x_i1=inf,x_i2=inf;
  switch (i) {
  case 0:
    x_i_2=x[i+2];
  case 1:
    x_i_1=x[i+1];
    break;
  case i:
    x_i2=x[i-2];
  case i-1:
    x_i1=x[i-1];
    break;
  }
  x_i=x[i];
  if(x_i2==inf){x_i2=x[i+2];}
  if(x_i1==inf){x_i1=x[i];}
  if(x_i_1==inf){x_i_1=x[i-1];}
  if(x_i_2==inf){x_i_2=x[i-2];}*/
//crank-Nicolson is (u[i]@t_n-u[i]@t_n-1)/dt + 
//avg of all d^nu/dx^n@t&&t_n-1 = 0
