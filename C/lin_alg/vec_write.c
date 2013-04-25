#include "vec_math.h"
//we're going on faith than the array of vectors is actually length n
void vec_write(FILE* file,vector* vectors,int n){
  for (int i=0;i<n;i++){
    for (int j=0;j<vectors[i].len;j++){
      //I'll make this better, but this'll do
      fprintf(file," |%.3f| ",vectors[i].vals[j]);
    }
    fprintf(file,"\n");
  }
}


