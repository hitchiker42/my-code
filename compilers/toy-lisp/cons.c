#include "lists.h"
/*Think of things like this, a Cons is a dotted pair
 *and a Cons* is a list.*/
//convience function to return car of list
datatype Car(Cons* ls){
  return ls->car;
}
datatype car(Cons ls){
  return ls.car;
}
//convience function to return cdr of list
Cons* cdr(Cons* ls){
  return ls->cdr;
}
//yes its just an idenity function but it helps the abstraction
//of building an actual list from a series of cells;
Cons first(Cons* ls){
  return *ls;
}
//iterate through list, return last element
Cons last(Cons* ls){
  if (ls->cdr == NULL){
    return *ls;
  } else {    
    return last(ls->cdr);
  }
}
//iterate through list, set cdr of second to last element to NULL
Cons butLast(Cons* ls){
  Cons cur_loc=first(ls),last_loc;
  while(cur_loc.next != NULL){
    last_loc=cur_loc;
    cur_loc=*cur_loc.cdr;    
  }
  return last_loc;
}
//set cdr of l to ls
Cons cons(Cons l,Cons ls){
  Cons* temp = last(&l).cdr;
  temp=&ls;
  return l;
}
//create a list from the arguments
//returns the head of the created list
Cons* list(datatype head,...){
  HERE();
  va_list ap;
  Cons *retval,*cur_loc;
  retval=cur_loc=xmalloc(sizeof(Cons));
  datatype cur_val=head;
  cur_loc->cdr=cur_val;
  va_start (ap,head);
  int i=0;
  //definately an efficency issue, it calls malloc n times which is not cool
  while ((cur_val = va_arg(ap,datatype)).tag != _nil){
    cur_loc->cdr=xmalloc(sizeof(Cons));//allocate next Cons cell
    cur_loc=cur_loc->cdr;//move loc pointer to next Cons cell
    cur_loc->val=cur_val;//set current Cons cell
  } 
  cur_loc->cdr=NULL;//allocate next Cons cell
    /*cur_loc=cur_loc->next=NIL_REF;//move loc pointer to next Cons cell
  va_end(ap);*/
  return retval;
}
//find length, tail recursive
int length_acc(Cons* ls,int i){
  if (ls == NULL){
    HERE();
    return i;
  } else {
    return length_acc(ls->cdr,++i);
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
//create a new list by applying map_fn to each element of ls
//ls is NOT modified
Cons* mapcar(datatype(*map_fn)(datatype),Cons* ls){
  Cons *retval,*cur_loc,*loc_ptr=ls;
  retval=cur_loc=xmalloc(sizeof(Cons));
  cur_loc->car=(*map_fn)(loc_ptr->car);
  while((loc_ptr=loc_ptr->cdr) != NULL){
    cur_loc->cdr=xmalloc(sizeof(Cons));
    cur_loc=cur_loc->cdr;
    cur_loc->car=(*map_fn)(loc_ptr->car);
  }
  cur_loc->cdr=NULL;
  return retval;
}
//destructively concatenate arguments, last argument is unmodified
Cons* nconc(Cons* ls,...){
  va_list ap;
  Cons* cur_loc=ls;
  va_start(ap,ls);
  //not sure if this works
  while (ap != NULL) {
    last(cur_loc).cdr=va_arg(ap,Cons*);
    cur_loc=cur_loc->cdr;
  }
  return ls;
}
Cons* push(Cons* ls,datatype l){
  Cons* retval = xmalloc(sizeof(Cons));
  retval->car=l;
  retval->cdr=ls;
  return retval;
}
datatype pop(Cons* ls){
  if(!ls){return NIL;}//deal with empty list
  datatype retval=Car(ls);
  ls=cdr(ls);
  return retval;
}
Cons pop_fxn(Cons** ls){
  if (ls == NULL){
    return NIL;
  } else {
    Cons retval=first(*ls);
    *ls=cdr(*ls);
    return retval;
  }
}
Cons* nrev(Cons* ls){
  Cons* cur_loc=ls,*last_loc=0,*temp;
  while(cur_loc != NULL){
    temp=cur_loc->cdr;
    cur_loc->cdr=last_loc;    
    last_loc=cur_loc;
    cur_loc=temp;
  }
  return last_loc;
}
Cons* reverse(Cons* ls){
  Cons* cur_loc,*prev_loc=0,*loc_ptr=ls;
  while(loc_ptr !=NULL){
    cur_loc=xmalloc(sizeof(Cons));//allocate new cons cell
    cur_loc->car=loc_ptr->car;//set car of new cons cell
    cur_loc->cdr=prev_loc;//set cdr of new cell to previous cell
    prev_loc=cur_loc;//shift current cell to previous cell
    loc_ptr=loc_ptr->cdr;//step to next element of ls
  }
  cur_loc->cdr=prev_loc;
  return cur_loc;
}
List mkList(datatype head,...){
  HERE();
  va_list ap;
  Cons *fst,*cur_loc;
  List retval;
  fst=cur_loc=xmalloc(sizeof(Cons));
  datatype cur_val=head;
  cur_loc->car=cur_val;
  va_start (ap,head);
  int i=1;
  //definately an efficency issue, it calls malloc n times which is not cool
  while ((cur_val = va_arg(ap,datatype)).tag != _nil){
    cur_loc->cdr=xmalloc(sizeof(Cons));//allocate next Cons cell
    cur_loc=cur_loc->cdr;//move loc pointer to next Cons cell
    cur_loc->car=cur_val;//set current Cons cell
    i++;
  } 
  cur_loc->cdr=NULL;//allocate next Cons cell
    /*cur_loc=cur_loc->next=NIL_REF;//move loc pointer to next Cons cell
  va_end(ap);*/
  retval.fst=fst,retval.lst=cur_loc,retval.len=i;
  return retval;
}
