#ifndef CALCULUS_UDIS_H
#define CALCULUS_UDIS_H
double rk4_udis (double (*fp)(double,double),double x,double t,double h);
double Stencil_5pt_udis (udis *u,int i,int order);
#endif
