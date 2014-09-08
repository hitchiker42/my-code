/*Common header file for tests*/
#ifndef _SL_CHECK_H
#define _SL_CHECK_H
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
#define gen_test_suite_fn(name,num_cases,cases...)      \
  Suite* name##_suite(void){                            \
    Suite *s;                                           \
    TCase tc[num_cases]={cases};                        \
    int i;                                              \
    s = suite_create(#name);                            \
    for(i=0;i<num_cases;i++){                           \
      suite_add_tcase(s, tc[i]);                        \
    }                                                   \
    return s;                                           \
  }
#define gen_test_case_fn(name,num_fns,fns...)        \
  TCase* name##_test_case(void){                     \
    TCase *tc;                                       \
    TFun f[num_fns]={fns};                          \
    int i;                                           \
    tc = tcase_create(#name);                        \
    for(i=0;i<num_fns;i++){                          \
      tcase_add_test(tc, f[i]);                      \
    }                                                \
    return tc;                                       \
  }
#endif
