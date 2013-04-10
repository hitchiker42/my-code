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
  return -12*pow(acosh(y),2);
}
udis*
  u_tstep(udis* u){
  return u_discrete(u);
  //=sinx*cosut-cosxsin
}
/*getting derivites w.r.t x, 0 =du/dt+ d^3u/du^3 -6*u*du/dx*/
udis*
u_discrete(udis* u){
  int i;
  double *u_x=malloc(u->len*sizeof(double));
  //#pragma opm parallel for
  for(i=0;i<u->len;i++){
    u_x[i]=Stencil_5pt_udis(u,i,3);
    u_x[i]+= -6*(u->u)[i]*Stencil_5pt_udis(u,i,1);
}
  return u_x;
}
  //assume some function f_ut to get next u
  //u_i_t+i=u_i+dt*(u_discrete)
double*
u_step(udis* u,double* x,double t_n,double (*u_tstep)(double,double)){
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
update(udis* u,double* x,double t_n,double h_t){
  int i;
  double* u_x=u_discrete(u,len,h_x);
  double* u_t=u_step(u_x,x,t_n,len,h_t,&u_tstep);
  //#pragma omp parallel for 
  for(i=0;i<len;i++){
    printf("u_t[i]:%f\t u_x[i]:%f\n",u_t[i],u_x[i]);
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
