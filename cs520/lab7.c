#include <stdio.h>
#include <stdint.h>
#include <execinfo.h>
static uint64_t rbp_init;
static uint64_t rsp_init;
extern struct {uint64_t rbp;uint64_t rsp;} getFP();
void print_stack_frame(){
  static int rec=0;
  if(!rec){
    rec=1;
    print_stack_frame();
  }
  uint64_t fp=getFP().rsp;
  int64_t i;
  for(i=-6;i<=6;i++){
    printf("%%rsp+%ld=%ld\n",i*8,fp+(i*8));
    printf("%ld(%%rsp)==%ld\n",i*8,*(uint64_t*)(fp+(i*8)));
  }
}

#define get_rbp()                               \
  ({uint64_t temp;                              \
  __asm__("movq %%rbp,%0"                       \
          : "=g" (temp));                       \
  temp;})
#define get_rbp_2                               \
  register uint64_t rbp __asm__ ("%rbp")
#define get_rsp()                               \
  ({uint64_t temp;                              \
  __asm__("movq %%rsp,%0"                       \
          : "=g" (temp));                       \
  temp;})
#define get_rsp_2                               \
  register uint64_t rsp __asm__ ("%rsp")
//emits a warning about returning without a value but
//this can be ignored since we know that the return value
//is already in the return registers
struct {uint64_t rbp;uint64_t rsp;} get_fp_fun(){
  __asm__ volatile ("movq %rbp,%rax\n"
                    "movq %rsp,%rdx");
  return;
}
#define get_fp()                                        \
  ({struct {uint64_t rbp;uint64_t rsp;} fp;             \
  __asm__("movq %%rpp,%0\n"                             \
          "movq %%rsp,%1"                               \
          : "=g" (fp.rbp), "=g" (fp.rsp));              \
  fp;})
#define get_fp_2()                                      \
  struct {uint64_t rbp;uint64_t rsp;} fp;               \
  __asm__("movq %%rbp,%0\n"                             \
          "movq %%rsp,%1"                               \
          : "=g" (fp.rbp), "=g" (fp.rsp));
uint64_t get_main_frame(){
  uint64_t *fp;
  fp=(uint64_t*)getFP().rbp;
  while(*(uint64_t*)fp){fp=*(uint64_t*)fp;}
  return fp;
}
uint64_t frameCount(){
  uint64_t i,fp=getFP().rbp;
  for(i=0;*(uint64_t*)fp;i++){fp=*(uint64_t*)fp;}
  return i;
}
volatile uint64_t count=0;
int recur(int x){
  count++;
  get_rbp_2;
  get_rsp_2;
  printf("%rbp is %ld, using register local var it's %ld\n",get_rbp(),rbp);
  printf("%rsp is %ld, using register local var it's %ld\n",get_rsp(),rsp);
  printf("rbp difference is %ld\n",rbp_init-rbp);
  printf("rsp difference is %ld\n",rsp_init-rsp);
  printf("recursion depth is %ld\n",((rsp_init-rsp)/48));
  printf("frame number (using rsp) %ld\n",frameCount());
  printf("frame number (using rbp) %ld\n",(rbp_init-getFP().rbp)/48);
  if(x<0){
    return -1;
  } else {
    return recur(recur(x+x));
  }
}
uint64_t ackerman(uint64_t m,uint64_t n){
  if(m==0){
    return n+1;
  } else if(n == 0){
    return ackerman(m-1,1);
  } else {
    return ackerman(m-1,ackerman(m,n-1));
  }
}
int main(){
  rbp_init=get_rbp();
  rsp_init=get_rsp();
  printf("stack pointer of main = %ld\n",rsp_init);
  printf("frame pointer of main = %ld\n",rbp_init);
  print_stack_frame();

  //  recur(1);
}
