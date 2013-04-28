#ifndef KDV_SIMPLE_H
#define KDV_SIMPLE_H
double*
seq(double init,double final,double step);
double 
inital_u(double y);
double*
u_discrete(double* u,int len,double h);
double* vec_add_kdv (double* x,double* y,int len,double scale);
double*
rk4_array_kdv (double* u,int len,double dt,double dx);
#endif
