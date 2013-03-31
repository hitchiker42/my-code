#include "calculus.h"
double rk4 (double (*fp)(double,double),double x,double t,double h){
  double k_1,k_2,k_3,k_4;
  k_1 = (*fp)(t,x);
  k_2 = (*fp)(t+0.5*h,x+0.5*h*k_1);
  k_3 = (*fp)(t+0.5*h,x+0.5*h*k_2);
  k_4 = (*fp)(t+h,x+h*k_3);
  return x + h*(1.0/6)*(k_1+2*k_2+2*k_3+k_4);
}
double Stencil_5pt (double arr[],int i,int order,double (*fp)(double),double h){
  unless(0<order<5,{return NAN;});
  switch (order) {
  case 0:
    return arr[i];
  case 1:
    return (-(*fp)(arr[i+2])+8*(*fp)(arr[i+1])-8*(*fp)(arr[i-1])+
            (*fp)(arr[i-2]))/12*h;
  case 2:
    return (-(*fp)(arr[i+2])+16*(*fp)(arr[i+1])-30*(*fp)(arr[i])+
            16*(*fp)(arr[i-1])-(*fp)(arr[i-2]))/12*pow(h,2);
  case 3:
    return ((*fp)(arr[i+2])-2*(*fp)(arr[i+1])+2*(*fp)(arr[i-1])-
            (*fp)(arr[i-2]))/2*pow(h,3);
  case 4:
    return ((*fp)(arr[i+2])-4*(*fp)(arr[i+1])+6*(*fp)(arr[i])-4*(*fp)(arr[i-1])+
            (*fp)(arr[i-2]))/pow(h,4);
  }
}
