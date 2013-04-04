#include "kdv.h"
#include <ctype.h>
double*
seq(double init,double final,double step){
  int len=ceil((final-init)/step);
  double* ret=malloc(len*sizeof(int));
  int i;
#pragma omp parallel for 
  for(i=0;i<len;i++){
  ret[i]=i*step+init;
}
  return ret;
}
double 
 inital_u(double y){
  return cos(M_PI*y);
}
double 
  u_tstep(double u,double x,double t){
  return cos(M_PI*(x-u*t));
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
  //assume some function f_ut to get next u
double
u_step(double* u,double t_n,int len,double h,
    double (*u_tstep)(double,double,double)){
  int i;
  double *u_t=malloc(len*sizeof(double));
#pragma omp parallel for
  for(i=0;i<len;i++){
  double u_step (double u_i,double t){
  return sin(0-u_i*t);
}
    u_t[i]=rk4(&u_step,u[i],t_n,h);
  }
}
       
void
update(double* u,double t_n,double h){}
  void 
  help (){
  const char * help = "kdv -h [-d(0.1)] [-f(0)] [-i(0)] [-s(0.1)] [-r(40)]"
"Command line interface to a Korteweg de Vires equation"
"solver\nWhen run with no options runs the solver with:\n\tinital condition"
" u=sin(pi*x)\n\tx inital value 0, final value 40 and step 0.1\n\t time step 0.1"
"\n\tand using the gsl qgas interation method."
"Options:"
"\t-h (--help,to be added) display this help and exit"
"\t-d (--delta_x,tba) set the difference between successive x values"
"\t-f (--function,tba) set the function to use for numerical integration"
"\t-i (--xinit,tba) set the inital x value"
"\t-s (--delta_t,tba) set the time step"
"\t-r (--range,tba) set the range of x values (ie x max = x inital + range)";
  printf(help);
}
