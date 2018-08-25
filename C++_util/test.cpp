#include <functional>
#undef __cplusplus
extern "C" {
#include <string.h>
}
template<template <typename> typename fn_type,
         typename T, typename U, typename V>
fn_type<T(V,U)> flip(const fn_type<T(U,V)> &fn){
  return fn_type<T(V,U)>([&fn](V arg1, U arg2){return fn(arg2, arg1);});
}
int main(){
  std::function f(strchr);
  auto g = flip(f);
  return f("strchr",'c') == g('c',"strchr");
}
#if 0
#include "util_extra.h"
#include <assert.h>
void test_text_functions(){
  util::string_view test("  one two    three\nfour\nfive    \n");
  auto left = util::ltrim(test);
  auto right = util::rtrim(test);
  auto both = util::trim(test);
  auto split = util::split(test);
  printf("Unmodified : \"%.*s\".\n"
         "Left trimmed : \"%.*s\".\n"
         "Right trimmed : \"%.*s\".\n"
         "Trimmed : \"%.*s\".\n",
         test.size(), test.data(),
         left.size(), left.data(),
         right.size(), right.data(),
         split.size(), split.data());
  printf("Split :");
  for(auto &&s : split){
    printf(" \"%.*s\"", s.size(), s.data());
  }
}
int main(){
  test_text_functions();
  return 0;
}
template<typename T>
void print_int_container(const T& C){
  for(auto&& x : C){
    printf("%d ", x);
  }
  printf("\n");
}
void test_mapping_functions(){
  std::vector<int> vec{0,1,2,3,4,5,6,7,8,9};
  std::list<int> ls{0,1,2,3,4,5,6,7,8,9};
  auto sq = [](auto x){return x*x;};
  
  auto vec2 = Fmap(sq, vec);
  auto ls2 = Fmap(sq, ls);

  assert(vec != vec2);
  assert(ls != ls2);
  print_int_container(vec);
  print_int_container(vec2);
  
  Fmap_into(sq, vec);
  Fmap_into(sq, ls);
  
  assert(vec == vec2);
  assert(ls == ls2);
  print_int_container(ls);
  print_int_container(ls2);
}
int main(){
  test_mapping_functions();
}
#endif
