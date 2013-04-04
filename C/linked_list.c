#include "linked_list.h"
node* new_node(){
  return malloc(sizeof(node));
}
void* car(node* cell){
  return cell->val;
}
node* cdr(node* cell){
  node* x=cell->next;
  return x;
}
list cons(node* head,node* tail){
  node* x=new_node();
  x->val=head->val;
  x->next=tail;
  return x;
}
int length(list l){
  node* cell=l;
  int i=0;
  while(cell->next!=NULL){
    i++;
    cell=cell->next;
  }
  return i;
}
