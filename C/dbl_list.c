#include "dbl_list.h"
#include <stdlib.h>
node* blank_node(){
  return (node*)malloc(sizeof(node));
}
node* new_node(double val){
  node* x = blank_node();
  x->val = val;
  return x;
}
double car(node* cell){
  return cell->val;
}
node* cdr(node* cell){
  return cell->next;
}
node* cons(node* head,node* tail){
  node* x = new_node(head->val);
  node* y = new_node(tail->val);
  x->next=y;
  y->next=NULL;
  return x;
}
