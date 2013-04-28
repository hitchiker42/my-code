#include "kdv.h"
#include <ctype.h>
double*
seq(double init,double final,double step){
  int len=ceil((final-init)/step);
  double *val=malloc(len*sizeof(double));
  int i;
  //#pragma omp parallel for 
  for(i=0;i<len;i++){
  val[i]=i*step+init;
  }
  return val;
}
double 
 inital_u(double y){
  return -12*pow((1/cosh(y)),2);
}
double 
  u_tstep(double t,double u){
  return t*u;
  //=sinx*cosut-cosxsin
}
/*getting derivites w.r.t x, 0 =du/dt+ d^3u/du^3 -6*u*du/dx*/
double*
u_discrete(double* u,int len,double h){
  int i;
  double *u_x=malloc(len*sizeof(double));
  #pragma opm parallel for
  for(i=0;i<len;i++){
  u_x[i]=Stencil_5pt(u,i,len,3,h);
  u_x[i]+= -6*u[i]*Stencil_5pt(u,i,len,1,h);
}
  return u_x;
}
double*
u_discrete_i(double* u,int i,int len,double h){
  //#pragma opm parallel for
 return 
   Stencil_5pt(u,i,len,3,h)-6*u[i]*Stencil_5pt(u,i,len,1,h);
}
double* vec_add_kdv (double* x,double* y,int len,double scale){
  int i;
  double *xy=malloc(len*sizeof(double));
  for(i=0;i<len;i++){
    ux[i]=x[i]+(y[i]*scale);
  }
  return xy;
}
double*
rk4_array_kdv (double* u,int len,double dt,double dx){
  int i;
  double result = malloc(len*sizeof(double));
  double *k_1=u_discrete(u,len,dx);
  double *k_2=u_discrete(vec_add_kdv(u,k_1,len,(dt*0.5)),len,dx);
  double *k_3=u_discrete(vec_add_kdv(u,k_2,len,(dt*0.5)),len,dx);
  double *k_4=u_discrete(vec_add_kdv(u,k_4,len,(dt)),len,dx);
  for(i=0;i<len;i++){
    result[i]=(dt*(1.0/6))*(k_1[i]+2*k_2[i]+2*k_3[i]+k_4[i]);
  }
  free(k_1);free(k_2);free(k_3);free(k_4);
  return result;
}
  //assume some function f_ut to get next u
  //u_i_t+i=u_i+dt*(u_discrete)
double*
u_step(double* u,double* x,double t_n,int len,double h,
    double (*u_tstep)(double,double)){
  int i;
  //double x_i;
  double *u_t=malloc(len*sizeof(double));
  //#pragma omp parallel for
  for(i=0;i<len;i++){
    // x_i=x[i];
    u_t[i]=rk4(u_tstep,u[i],0,h);
  }
  return u_t;
}
       
void
update(double* u,double* x,double t_n,double h_x,double h_t,int len){
  int i;
  double* u_x=u_discrete(u,len,h_x);
  double* u_t=u_step(u_x,x,t_n,len,h_t,&u_tstep);
  //#pragma omp parallel for 
  for(i=0;i<len;i++){
    //printf("u_t[i]:%f\t u_x[i]:%f\n",u_t[i],u_x[i]);
    //not sure what this expression should be exactally
    u[i]+=u_t[i]+u_x[i];
  }
  free(u_x);free(u_t);
}
void 
help (){
  const char * help = "kdv -h [-d(0.1)] [-f(0)] [-i(0)] [-s(0.1)] [-r(40)]"
"Command line interface to a Korteweg de Vires equation"
"solver\nWhen run with no options runs the solver with:\n\tinital condition"
" u=sin(pi*x)\n\tx inital value 0, final value 40 and step 0.1\n\t time step 0.1"
"\n\tand using the gsl qgas interation method.\n"
"Options:"
"\n\t-h (--help,to be added) display this help and exit"
"\n\t-d (--delta_x,tba) set the difference between successive x values"
"\n\t-f (--function,tba) set the function to use for numerical integration"
"\n\t-i (--xinit,tba) set the inital x value"
"\n\t-s (--delta_t,tba) set the time step"
"\n\t-r (--range,tba) set the range of x values (ie x max = x inital + range)\n";
  printf(help);
}
