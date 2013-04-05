int factorial(int n){
  int i;
  int retval = 1;

  for (i = 1; i <= n; i++) {
    retval *= i;
  }

  return retval;
}
int fact_acc(int n,int acc){
  if (n==0){
    return acc;
  } else {
    return fact_acc((n-1),n*acc);
  }
}
int tail_fact(int n){
  return fact_acc(n,1);
}
