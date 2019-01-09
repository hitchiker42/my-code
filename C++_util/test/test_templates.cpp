#include "../util.h"
static constexpr util::array a1(util::va_init_tag, 1, 2, 3, 4, 5);
static constexpr util::array a2(util::va_init_tag, 6.0, 7.0, 8.0, 9.0, 10.0);
static constexpr util::array a1_2(a1,a2);
int main(){
  printf("Array a1:\n");
  for(auto x : a1){
    printf("\t%s\n", std::to_string(x).c_str());
  }
  printf("Array a2:\n");
  for(auto x : a2){
    printf("\t%s\n", std::to_string(x).c_str());
  }    
  printf("Array a1_2:\n");
  for(auto x : a1_2){
    printf("\t%s\n", std::to_string(x).c_str());
  }    
}    
