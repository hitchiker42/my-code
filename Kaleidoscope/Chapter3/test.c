#include <stdio.h>
#include "dynarray.c"
int main(){
  int i;
  mkdynarray_type(double,double_array);
  double_array* test2,test;
  test2=(double_array*)init_dynarray(20,0,10,8);
  test=*test2;
  for(i=0;i<100;i++){
    dynarray_ref(test,i)=i;
  }
  printf("testing dynarray ref %f\n",test.array[56]);
  return;
}
  
