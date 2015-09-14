#include "gl770.h"
#define USE_GLUT
#include "common.h"
#include "context.h"

global_contex *init_global_context(int window_width, int window_height){
  //stupid c++ casting
  global_context *ctx = (global_context*)xmalloc(sizeof(global_context));
  //
