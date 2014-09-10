//Utility functions to be used in testing code
#ifndef _SL_TEST_UTIL_H
#define _SL_TEST_UTIL_H
#include <check.h>
#include "../src/common.h"
#include <alloca.h>
#include <stdarg.h>
static Suite* gen_test_suite(const char *name, int numargs, ...){
  Suite *s = suite_create(name);
  TCase *tc;
  va_list ap;
  va_start(ap, numargs);
  int i;
  for (i=0;i<numargs;i++) {
    tc = va_arg(ap, TCase*);
    suite_add_tcase(s, tc);
  }
  va_end(ap);
  return s;
}
static TCase* gen_test_case(const char* name, int numargs, ...){
  TCase *tc=tcase_create(name);
  TFun f;
  int i;
  va_list ap;
  va_start(ap, numargs);
  for (i=0;i<numargs;i++){
    f = va_arg(ap, TFun);
    tcase_add_test(tc, f);
  }
  va_end(ap);
  return tc;
}
/*
struct sigaction init_sigaction(void(*handler)(int)){
  struct sigaction action;
  action.sa_handler = handler;
  sigemptyset(&handler.sa_mask);
  handler.sa_flags = 0;
  return handler;
}
void catch_signal(int sig){
//ck_assert... macros go here
#define CATCH_SIGNAL(signal)                    \
  { struct sigaction __new_action, __old_action;        \
  __new_action.sighandler=catch_signal;                 \
  sigemptyset(&new_action.mask);
  
*/
#endif
