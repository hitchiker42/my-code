#include <stdio.h>
#include <gmp.h>
/*long ack(long m,long n){
  if (m == 0){
    return n+1;
  } else if (n == 0){
    return ack(m-1,1);
  } else {
    return ack(m-1,ack(m,n-1));
  }
  }*/
mpz_t cnt,zero,one,ans;
void ack(mpz_t m, mpz_t n){
  if (!(mpz_cmp_ui(m,0))){
    mpz_add_ui(cnt,cnt,1);
    mpz_add_ui(ans,n,1);
    return;
  } else if (!(mpz_cmp_ui(n,0))){
    mpz_sub_ui(m,m,1);
    return ack(m,one);
  } else {
    mpz_sub_ui(n,n,1);
    ack(m,n);
    mpz_set(n,ans);
    mpz_sub_ui(m,m,1);
    return ack(m,n);
  }
}
int main(){
  mpz_init_set_ui(one,1);
  mpz_init2(cnt,26553600);
  mpz_init(zero);
  mpz_t n,m;
  mpz_init2(n,265536);
  mpz_set_ui(n,2),mpz_init_set_ui(m,4);
  mpz_init2(ans,265536);
  ack(m,n);
  char* count = mpz_get_str(NULL,10,cnt);
  char* answer = mpz_get_str(NULL,10,ans);
  printf("answer = %s\ncount = %s\n",answer,count);
  return 0;
}
