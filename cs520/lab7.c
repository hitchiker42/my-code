#define get_rbp()                               \
  ({long temp;                                  \
  __asm__("movq %%rbp,%0"                       \
          : "=g" (temp));                       \
  temp;})
#define break_program()                         \
  __asm__ volatile("pop %rbp")
#include <stdio.h>
long count=0;
int loop(int x){
  printf("loop number %d\n",x);
  __asm__ volatile("movq %rbp,8(%rbp)");
  return x+1;
}
int recur(int x){
  count++;
  if(count %500){
    register long rbp __asm__ ("%rbp");
    printf("%rbp is %ld, using register local var it's %ld\n",get_rbp(),rbp);
    break_program();
  }
  if(x<0){
    return -1;
  } else {
    return recur(recur(x+x));
  }
}
int main(){
  loop(1);
}
