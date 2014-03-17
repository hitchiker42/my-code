#include <stdio.h>
#include <stdint.h>
#define get_rbp()                               \
  ({uint64_t temp;                              \
  __asm__("movq %%rbp,%0"                       \
          : "=g" (temp));                       \
  temp;})
#define get_rbp_2()                             \
  register uint64_t rbp __asm__ ("%rbp")
#define get_rsp()                               \
  ({uint64_t temp;                              \
  __asm__("movq %%rsp,%0"                       \
          : "=g" (temp));                       \
  temp;})
#define get_rsp_2()                             \
  register uint64_t rsp __asm__ ("%rsp")

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
extern struct {uint64_t rbp;uint64_t rsp;} getFP();
static uint64_t rbp_init;
static uint64_t rsp_init;
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
