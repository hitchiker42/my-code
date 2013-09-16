#include "lists.h"
/*I guess I could do something like this
struct dbl_list{
  Cons* hd;
  Cons* tl;
  int len;
  }*/
datatype car(Cons ls){
  return ls.val;
}
Cons* cdr(Cons* ls){
  return ls->next;
}
//yes its just an idenity function but it helps the abstraction
//of building an actual list from a series of cells;
Cons first(Cons* ls){
  return *ls;
}
Cons last(Cons* ls){
  if (ls->next == NULL){
    return *ls;
  } else {    
    return last(ls->next);
  }
}
Cons butLast(Cons* ls){
  Cons cur_loc=first(ls),last_loc;
  while(cur_loc.next != NULL){
    last_loc=cur_loc;
    cur_loc=*cur_loc.next;    
  }
  return last_loc;
}
Cons cons(Cons l,Cons ls){
  Cons* temp = last(&l).next;
  temp=&ls;
  return l;
}
Cons* list(datatype head,...){
  HERE();
  va_list ap;
  Cons *retval,*cur_loc;
  retval=cur_loc=xmalloc(sizeof(Cons));
  datatype cur_val=head;
  cur_loc->val=cur_val;
  va_start (ap,head);
  int i=0;
  //definately an efficency issue, it calls malloc n times which is not cool
  while ((cur_val = va_arg(ap,datatype)).tag != _nil){
    cur_loc->next=xmalloc(sizeof(Cons));//allocate next Cons cell
    cur_loc=cur_loc->next;//move loc pointer to next Cons cell
    cur_loc->val=cur_val;//set current Cons cell
  } 
  cur_loc->next=NULL;//allocate next Cons cell
    /*cur_loc=cur_loc->next=NIL_REF;//move loc pointer to next Cons cell
  va_end(ap);*/
  return retval;
}
int length_acc(Cons* ls,int i){
  if (ls == NULL){
    HERE();
    return i;
  } else {
    return length_acc(ls->next,++i);
  }
}
int length(Cons* ls){
  return length_acc(ls,0);
  /* if(ls == NIL_REF){//empty list
    return 0;
  }
  int count=0;
  Cons* cur_loc=ls->next;
  do{
    count++;
  } while((cur_loc=cur_loc->next) != NIL_REF);
  return count;*/
}
    
Cons* mapcar(datatype(*map_fn)(datatype),Cons* ls){
  Cons *retval,*cur_loc,*loc_ptr=ls;
  retval=cur_loc=xmalloc(sizeof(Cons));
  cur_loc->val=(*map_fn)(loc_ptr->val);
  while((loc_ptr=loc_ptr->next) != NULL){
    cur_loc->next=xmalloc(sizeof(Cons));
    cur_loc=cur_loc->next;
    cur_loc->val=(*map_fn)(loc_ptr->val);
  }
  cur_loc->next=NULL;
  return retval;
}
Cons nconc(Cons ls,...){
  va_list ap;
  Cons cur_loc=ls;
  va_start(ap,ls);
  //not sure if this works
  while (ap != NULL) {
    *last(&cur_loc).next=va_arg(ap,Cons);
    cur_loc=*cur_loc.next;
  }
  return ls;
}
Cons push(Cons ls,Cons l){
  l.next=&ls;
  return l;
}
Cons pop_fxn(Cons** ls){
  if (ls == NULL){
    return NIL;
    /*  } if ((*ls)->next == NULL){
    Cons retval=first(*ls);
    *ls=NULL;*/
  } else {
    Cons retval=first(*ls);
    *ls=cdr(*ls);
    return retval;
  }
}
Cons* nrev(Cons* ls){
  Cons* cur_loc=ls,*last_loc=0,*temp;
  while(cur_loc != NULL){
    temp=cur_loc->next;
    cur_loc->next=last_loc;    
    last_loc=cur_loc;
    cur_loc=temp;
  }
  return last_loc;
}

List mkList(datatype head,...){
  HERE();
  va_list ap;
  Cons *fst,*cur_loc;
  List retval;
  fst=cur_loc=xmalloc(sizeof(Cons));
  datatype cur_val=head;
  cur_loc->val=cur_val;
  va_start (ap,head);
  int i=1;
  //definately an efficency issue, it calls malloc n times which is not cool
  while ((cur_val = va_arg(ap,datatype)).tag != _nil){
    cur_loc->next=xmalloc(sizeof(Cons));//allocate next Cons cell
    cur_loc=cur_loc->next;//move loc pointer to next Cons cell
    cur_loc->val=cur_val;//set current Cons cell
    i++;
  } 
  cur_loc->next=NULL;//allocate next Cons cell
    /*cur_loc=cur_loc->next=NIL_REF;//move loc pointer to next Cons cell
  va_end(ap);*/
  retval.fst=fst,retval.lst=cur_loc,retval.len=i;
  return retval;
}
