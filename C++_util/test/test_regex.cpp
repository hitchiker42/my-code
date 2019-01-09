#include "../util.h"
#include "../regex.h"
static std::string test1 = 
R"(1 2 3
   4 5 6
   7 8 9)";
static std::string test2 = 
R"(1 [2 3 4] 3
2 [5] 1
3 [] 0
4 [6 7 8 9 10] 5)";

void do_test1(){
  util::regex_matcher matcher("(\\d+) (\\d+) (\\d+)", test1);
  int results[3] = {6, 15, 24};
  int test_results[3] = {0,0,0};
  int idx = 0;
  while(matcher.next_match()){
    for(int i = 0; i < 3; i++){
      test_results[idx] += matcher.sub_match_as_int(i+1);
    }
    idx++;
  }
  printf("expected: %d %d %d\nactual: %d %d %d\n",
         results[0],results[1],results[2],
         test_results[0],test_results[1],test_results[2]);
  for(int i = 0; i < 3; i++){
   assert(results[i] == test_results[i]);
  }
}
int main(){
  do_test1();
}
    
