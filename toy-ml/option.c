#include "common.h"
datatype valOf(option opt){
  datatype retval;
  if(opt.state == NONE){
    retval.tag = _NONE;
  } else {
    retval = opt.value;
  }
  return retval;
}
datatype getOpt(option opt,datatype fallback){
  if(opt.state == NONE){
    /*then*/ return fallback;
  } else {   return opt.value;};
}
