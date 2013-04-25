#include <stdio.h>
#include "vec_math.h"
#include <stdlib.h>
#include <assert.h>
int vec_mult_test(int N,int M,FILE* file){
  matrix A={malloc(M * sizeof(double*)),N,M};
  malloc_test(A.vals==NULL);
  vector x={malloc(N*sizeof(double)),N};
  malloc_test(x.vals == NULL);
  for (int i=0;i<M;i++){
    A.vals[i]=malloc(N * sizeof(double));
    malloc_test(A.vals[i]==NULL);
    for (int j=0;j<N;j++){
      A.vals[i][j]=((i*N)+(j+1));
      x.vals[j]=j+1;
    }
  }
  vector y=mat_vec_mult(A,x);
  for (int i=0;i<N;i++){
    if (i >= M){
      for (int k=0;k<M;k++){
        fprintf(file,"        ");
      }
      fprintf(file,"  |%6.3f|",x.vals[i]);
      fprintf(file,"           \n");
    } else {
      fprintf(file,"|%6.3f ",A.vals[i][0]);
      for (int j=1;j<M-1;j++){
        fprintf(file," %6.3f ",A.vals[i][j]);
      }
      fprintf(file,"%6.3f| ",A.vals[i][-1]);
      if (i==N/2){
        fprintf(file,"* |%6.3f| = |%6.3f|\n",x.vals[i],y.vals[i]);
      } else {
        fprintf(file,"  |%6.3f|   |%6.3f|\n",x.vals[i],y.vals[i]);
      }
    }
  }
  if (N<M){
    for (int i=N;i<M;i++){
      for (int j=0;j<M;j++){
        printf("        ");
      }
      printf("             |%6.3f|\n",y.vals[i]);
    }
  }
  if (N==M==3){
    assert(y.vals[0] == 10 && y.vals[1] == 28 && y.vals[2] == 46);
  }
  /*for (int i=0;i<N;i++){
    printf("|%6.3f ",A[i][0]);
    for (int j=1;j<M-1;j++){
      printf(" %6.3f ",A[i][j]);
    }
    printf("%6.3f| ",A[i][M-1]);
    if (i==N/2){
      printf("* |%6.3f| = |%6.3f|\n",x[i],y[i]);
    } else {
      printf("  |%6.3f|   |%6.3f|\n",x[i],y[i]);
      }}*/
return 0;
}
