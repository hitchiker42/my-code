#ifndef __MY_GL_COMMON_H__
#define __MY_GL_COMMON_H__
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "config.h"
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <signal.h>
#include <math.h>
#include <GL/glew.h>
#define USE_GLFW
#if (defined USE_GLFW) || !(defined USE_GLUT)
#include <GLFW/glfw3.h>
typedef GLFWwindow* window;
#else
#include <GL/freeglut_std.h>
#include <GL/freeglut_ext.h>
typedef GLuint window;
#endif
#include "C_util.h"
#include "gl_types.h"
#include "gl_util.h"
#ifndef OOM_FUN
static void oom_fun(void){
  fputs("Out of memory\n",stderr);
  raise(SIGABRT);
}
#endif
static inline void* xmalloc(size_t sz){
  void *mem = malloc(sz);
  if(!mem && sz != 0){
    oom_fun();
  }
  return mem;
}
static inline void* zmalloc(size_t sz){
  void *mem = calloc(sz, 1);
  if(!mem && sz != 0){
    oom_fun();
  }
  return mem;
}
#endif
