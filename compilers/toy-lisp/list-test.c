#include "lists.h"
const char* order(int n){
  switch (n){
    case 1: return "st";
    case 2: return "nd";
    case 3: return "rd";
    default: return "th";
  }
}
double fact(double val,int n){
  if (n<1){
    return val;
  } else {
    return fact(val*(double)n,n-1);
  }
}
datatype lisp_fact(datatype val){
  datatype retval={_double,0};
  if (val.tag != _double){
    return retval;
  } else {
    retval.val.real=fact(val.val.real,(int)val.val.real);
    return retval;
  }
}
double fib(int n){
  if(n<=1){
    return 1;
  } else {
    double x=1,y=1,z;
    n-=1;
    //tail recursion sans function calls
    /*in ml
     *fun fib(n) =
     if n<2 then 1 else
        let 
          fun fib_acc (0,x,y) = y
            | fib_acc (n,x,y) = fib_acc(n-1,y,x+y)
        in
          fib_acc(n-2,1,1)
          end*/
  FIB:
    if(n==0){
      return y;
    } else {
      z=x+y;
      x=y;
      y=z;
      n--;
      goto FIB;
    }
  }
}
int main(){
  //Cons* NIL_REF=&NIL;
  HERE();
  datatype* x;
  x=xmalloc(30*sizeof(datatype));
  func_t fact_ptr=lisp_fact;
  Cons* ls,*xs;
  int i,j=0,k=1,l;
  for(i=0;i<30;i++){
    l=j+k,j=k,k=l;
    x[i].tag=_double;
    x[i].val.real=l;
  }
  HERE();
  ls=list(x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9],nil);
  xs=mapcar(lisp_fact,ls);
  HERE();
  printf("length of ls = %d, should be %d\n",length(ls),10);
  printf("2nd in ls = %f should be 2\n",ls->next->val.val.real);
  i=1;
  //ls=nrev(ls);
  printf("length of ls = %d, should be %d\n",length(ls),10);
  while(ls != NULL && i<20){    
    printf("%d%s value of ls = %f, should be %f\n",i,order(i),pop(ls).val.val.real,fib(i));
    printf("%d%s value of xs = %f, should be %f\n",i,order(i),pop(xs).val.val.real,fact((double)fib(i),fib(i)));
    i++;
  }
  return 0;
}
