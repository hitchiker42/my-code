#include <stdio.h>
#include "vec_math.h"
//#define N 3 //wrong syntax
int scalar_test(int N,FILE* file) {
  vector x={malloc(N*sizeof(double)),N};
  vector y={malloc(N*sizeof(double)),N};
  malloc_test(x.vals==NULL || y.vals==NULL);
  for(int i=0;i<N;i++){
    x.vals[i] = i;
    y.vals[i] = i+N;
  }
  double z=scalar_product(x,y);
  //printf("scalar product is %g\n", scalar_product(x, y,N));
  for (int i=0;i<N;i++){
    if(i==N/2){
      fprintf(file,"|%6.3f| . |%6.3f| =  %6.3f \n",x.vals[i],y.vals[i],z);
    } else{
      fprintf(file,"|%6.3f|   |%6.3f|             \n",x.vals[i],y.vals[i]);
    }
  }
  return 0;
}
