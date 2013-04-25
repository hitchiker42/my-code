#include "dbl_list.h"
#include <stdio.h>
int main(){
  node* x = blank_node();
  node* y = blank_node();
  x->val = 0;
  y->val = 1;
  printf("x:%.0f  y:%.0f\n",car(x),car(y));
  node* z = cons(x,y);
  node* w = cons(z,blank_node());
  printf("x:%.0f  y:%.0f\n",car(z),car(cdr(z)));
  printf("x:%.0f  y:%.0f\n",car(cdr(w)),car(cdr(cdr(w))));
  return 0;
}
