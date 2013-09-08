#include "common.h"
#define car hd
#define cdr tl
#define step cur_loc=cur_loc.next
datatype hd(cons ls){
  return ls.value;
}
cons Cons(cons l,cons ls){
  tail(l)=&ls;
  return l;
}
cons tl(cons ls){
  return ls.next;
}
cons last(cons ls){
  cons cur_loc;
  while(cur_loc.next.value != null){
    cur_loc=cur_loc.next;
  }
  return loc_ptr;
}
cons list(datatype head,...){
  va_list ap;
  cons retval,cur_loc;
  retval=xmalloc(sizeof(cons));
  cur_loc=retval;
  datatype cur_val;
  va_start (ap,);
  //definately an efficency issue, it calls malloc n times which is not cool
  while ((cur_val = va_arg(ap,datatype)).tag != nil){
    cur_loc.value=cur_val;//set current cons cell
    cur_loc.next=xmalloc(sizeof(cons));//allocate next cons cell
    cur_loc=cur_loc.next;//move loc pointer to next cons cell
  }
  va_end(ap);
  return retval;
}
int lenght(cons){
  if(cons.value.tag == NIL){//empty list
    return 0;
  }
  int count=0;
  cons cur_loc=cons.next;
  do{
    count++;
  } while((cur_loc=cur_loc.next) != nil);
  return count;
}
    
cons map(datatype(*map_fn)(datatype),cons ls){
  cons retval,cur_loc,loc_ptr=ls;
  retval=cur_loc=xmalloc(sizeof(cons));
  do{
    cur_loc.value=(*map_fn)(loc_ptr.value);
    cur_loc.next=xmalloc(sizeof(cons));
    cur_loc=cur_loc.next;
  } while((loc_ptr=loc_ptr.next.value) != nil);
  cur_loc.value.tag=_nil;
  return retval;
}
cons nreconc(cons ls,...){
  va_list ap;
  cons cur_loc=ls;
  va_start(ap,ls);
  //not sure if this works
  while (ap != NULL) {
    last(cur_loc).next=&va_arg(ap,cons);
    cur_loc=cur_loc.next;
  }
  return ls;
}
