#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "vec_math.h"
int add_test(int N,FILE* file){
  vector x={malloc(N*sizeof(double)),N};
  vector y={malloc(N*sizeof(double)),N};
  malloc_test(x.vals==NULL || y.vals==NULL);
  for (int i=0; i<N; i++){
    x.vals[i]=(double)i;
    y.vals[i]=(double)(N+i);
  }
  vector z=vec_add(x,y);
  for (int i=0;i<N;i++){
    if(i==N/2){
      fprintf(file,
              "|%6.3f| + |%6.3f| = |%6.3f|\n",x.vals[i],y.vals[i],z.vals[i]);
    } else{
      fprintf(file,
              "|%6.3f|   |%6.3f|   |%6.3f|\n",x.vals[i],y.vals[i],z.vals[i]);
    }
  }
  if (N==3){
    assert(z.vals[0]==3 && z.vals[1]==5 && z.vals[2]==7);
  }
  return 0;
}

