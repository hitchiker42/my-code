#include "common.h"
//ml access to pointers
datatype mkref(datatype value){
  datatype retval;
  retval.ref = xmalloc(sizeof(datatype));
  retval.tag = _ref;
  *retval.ref=value;
  return retval;
}
void set(datatype ref,datatype value){
  if(ref.tag != _ref){
    fprintf(stderr,"can't assign to something other than a reference\n");
  } else {
    *ref.ref=value;
  }
  return;
}
datatype get(datatype ref){
  if(ref.tag != _ref){
    fprintf(stderr,"can't access memory from something other than a reference\n");
    return nil;
  } else {
    return *ref.ref;
  }
}
