#include "vector.h"
double
trap(double (*fp)(double),double a,double b,double n){
  double i;
  double h=(a-b)/n;
  double result=((*fp)(a)+(*fp)(b))/2;
  for(i=a+h;i<b;i+=n){
    result+=(*fp)(i);
  }
  return result;
}
