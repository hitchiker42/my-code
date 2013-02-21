#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "vec_math.h"
vector mat_vec_mult(matrix A,vector x){
  //y=Ax, given A&x solve for y
  //double y[n];
  if (A.columns == x.len){
    double* size=malloc(x.len*sizeof(double));
    malloc_test(size==NULL);
    vector y={size,x.len};
    double temp;
    for (int i=0;i<A.rows;i++){
      //printf("%d\n",i);
      y.vals[i]=0;
      temp=0;
      for (int j=0;j<x.len;j++){
        //printf("\t%d\n",j);
        temp+=x.vals[j]*A.vals[i][j];
      }
      y.vals[i]=temp;
      //printf("%f\n",y[i]);
    }
    return y;
  }
  else{
    printf("Bad Args");
   exit(-1);
  }
}
