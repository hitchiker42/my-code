#include "common.h"
typedef struct list list;
struct list{
  Cons* head;
  Cons* tail;
  int len;
};
datatype list_car(list* ls){
  return ls->head->car;
}
Cons* list_head(list* ls){
  return ls->head;
}
Cons list_cdr(list* ls){
  return *(ls->head->next);
}
datatype list_pop(list *ls){
  if (!ls){return NIL;}//deal with empty list
  datatype retval = list_car(ls);
  *ls=(list){cdr(ls),ls->tail,ls->len-1};
  return retval;
}
void list_push(list *ls,datatype val){
  Cons* new=xmalloc(sizeof(Cons));
  new->car=val;
  new->cdr=list_head(ls);
  ls->head=new;
  ls->len+=1;
}
list* mapcar(datatype(*map_fn)(datatype),list *ls){
  Cons* mem_block=xmalloc(sizeof(Cons)*(ls->len));
  list* new_ls=xmalloc(sizeof(list));
  new_ls->len=ls->len;
  int i;
  //set new head
  Cons* new_head=mem_block[0];  
  Cons *cur_loc=new_head,*cur_loc_old=ls->head;
  new_ls->head=new_head;
  for(i=1;i<ls->len;i++){
    cur_loc->car=map_fn(cur_loc_old->car);
    cur_loc->cdr=mem_block[i];
    cur_loc=cur_loc->cdr;
    cur_loc_old=cur_loc_old->cdr;
  }
  new_ls->tail=cur_loc;
  return new_ls;
}
  
