#include "util_extra.h"
#include <assert.h>
#include <list>
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
