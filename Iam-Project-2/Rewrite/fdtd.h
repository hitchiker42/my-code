#ifndef _FDTD_H_
#define _FDTD_H_
/* Preprocessor includes/defines */
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <x86intrin.h>
#include <mpi.h>
#include "xmalloc.h"
#define dy dx
#define dz dx
#ifdef __SSE2__
typedef __m128d m128d;
#ifdef __AVX__
typedef __m256d m256d;
#endif
#endif
/* Types */
typedef struct field field;
typedef struct cell cell;
typedef struct point point;
struct field {
  double* x;
  double* y;
  double* z;
};
struct point{
  int x;
  int y;
  int z;
};
struct cell {
  double H[3];//H_x,H_y,H_z
  double E[3];//E_x,E_y,E_z
  double time;
  point loc;
};
/* Global Values */
//global variables, I could do things w/o them but
//using them makes things more efficent.
extern field H_n;
extern field H_n1;//H at time n and time n+1
extern field E_n;
extern field E_n1;//E at time n and time n+1
//global constants
extern const double sigma;
extern const double episilon;
extern const double mu;
extern const double dt;
extern const double dx;
extern const int x_min;
extern const int x_max;
extern const int y_min;
extern const int y_max;
extern const int z_min;
extern const int z_max;

/* Functions */
double get_value(double* field,point loc);
double* get_ptr(double* field,point loc);
double get_value_xyz(double* field,int x,int y,int z);
double* get_ptr_xyz(double* field,int x,int y,int z);
double updateHx(field H,field E,point loc);
double updateHy(field H,field E,point loc);
double updateHz(field H,field E,point loc);
double updateEx(field H,field E,point loc);
double updateEy(field H,field E,point loc);
double updateEz(field H,field E,point loc);
/* Function Definitions */
static inline point incx(point loc){
  return (point){loc.x+1,loc.y,loc.z};
}
static inline point incy(point loc){
  return (point){loc.x,loc.y+1,loc.z};
}
static inline point incz(point loc){
  return (point){loc.x,loc.y,loc.z+1};
}
#endif
/*flow,
  know H&E @ time n/n-0.5
  find H @ time n+0.5
  fine E @ time n+1
  goto start
//material constants, assume homogenious material
/*
syntax Field_{componant}^{time}
\del t <=(1/\del x^2 + 1/\del y^2 + 1/\del z^2)^1/2

H_{x}^{n+1/2}(i,j+1/2,k+1/2)=H_x^{n-1/2}(..) + \frac{\del t}
{\mu(i+j+1/2,k+1/2)*\del}*(E_y^n(i,j+1/2,k+1)-E_y^n(...,k)+
E_z^n(i,j,k+1/2)-E_z^n(..j+1..))
similar for H_z&H_y, look at paper for formula
E_{x}^(n+1)(i+1/2,j,k) = [1 - \frac{\sigma(i+1/2,j,k)\del t}
{\episilon(i+1/2,j,k)}]E_x^n(..)+\frac{\del t}{{\episilon(i+1/2,j,k)\sigma}*
[H_z^{n+1/2}(i+1/2,j+1/2,k)-H_z^{n-1/2}(..j-1/2..)
+H_y^{n+1/2}(i+1/2,j,k-1/2)-H_y^{n+1/2}(..k+1/2..)
similar for E_z&E_y*/
//for now E(i_max,_,_) = 0.0, H(i_max,_,_) = 0;
//or from taflove
//E_?(i_max,_,_)@t_n=E_?(i_max-1,_-1,_-1)+(i_max-1,_,_)+(i_max-1,_+1,_+1)+)i_max(-1,_,_-1)...etc/9@t_n-2

/* a + 1 = a + 1 % 3,a=0=x,a=1=y,a=2=z
  H_a@n+1/2 = H_a@n-1/2 + \mu\del{t}*E_a+1((a+2)+1)-E_a+1((a+2))
  +E_a+2(a+1)-E_a+2((a+1)+1)*/
/*
    location of fields in a cell at i,j,k
    (i+1/2,j,k)=E_x
    (i,j,k+1/2)=E_z
    (i,j+1/2,k)=E_y
    (i+1/2,j,k+1/2)=H_y
    (i+1/2,j+1/2,k)=H_z
    (i,j+1/2,k)=H_x
    but really for a point i,j,k
    fields are at that point, for programming purposes
   where n+1 is on the next cell in the n direction*/
/* loop structure*/
int i,j,k;
for(k=z_min+1;k<z_max;k++){
  for(j=y_min+1;j<y_max;j++){
    /*#ifndef __AVX__
      for(k=x_min+1;k<x_max;k++){
      #endif #ifdef __AVX__*/
    for(k=x_min+1;k<x_max;k+=4){
    updateHx(H_n.x,E_n,(point){i,j,k});
  }
  }
  }
//etc...
//for E fields
for(k=z_min+1;k<z_max;k++){
    for(j=y_min+1;j<y_max;j++){
    /*#ifndef __AVX__
      for(k=x_min+1;k<x_max;k++){
      #endif #ifdef __AVX__*/
    for(k=x_min+1;k<x_max;k+=4){
    updateEx(H_n.x,E_n,(point){i,j,k});
  }
  }
  }
