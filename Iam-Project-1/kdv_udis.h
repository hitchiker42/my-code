#ifndef KDV_UDIS_H
#define KDV_UDIS_H
double* u_step(udis* u,double* x,double t_n,
               double (*u_tstep)(double,double));
udis* u_discrete(udis* u);
double inital_u(double y);
udis* u_tstep(udis* u);
double* seq(double init,double final,double step);
void update(udis* u,double* x,double t_n,double h_t);
void help ();
#endif
