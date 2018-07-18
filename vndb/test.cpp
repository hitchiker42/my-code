#include "vndb.h"
#include "xorshift.h"
int main(){
  //We shuffle test so it needs to be a mutable string.
  util::string_view test("123456789abcdefghijklmnopqrstuvwxyz!@#$%^&*(){}[]/=?+");
  util::string_view symbols("!@#$%^&*(){}[]/=?+");
  util::string_view letters("abcdefghijklmnopqrstuvwxyz");
  util::string_view numbers("123456789");
  std::string spaces(test.size(), ' ');
  util::svector<size_t> offsets;
  for(auto &&s : {symbols, letters, numbers}){
    offsets.push_back(test.find_first_of(s));
    offsets.push_back(test.find_first_not_of(s, offsets.back()));
    offsets.push_back(test.find_last_of(s));
    offsets.push_back(test.substr(0, offsets.back()+1).find_last_not_of(s));
  }
  /*

   */
  std::array<const char*, 6> labels = 
    {{"search set   :", "string       :", 
      "first of     :", "first not of :",
      "last of      :", "last not of  :"}};
  for(auto &&s : {numbers, letters, symbols}){
    int last_not_of = offsets.pop();
    int last_of = offsets.pop();
    int first_not_of = offsets.pop();
    int first_of = offsets.pop();
    
    printf("%s%.*s\n%s%.*s\n",
           labels[0], (int)s.size(), s.data(), 
           labels[1], (int)test.size(), test.data());
    auto ptr = labels.data() + 2;
    for(auto offset : {first_of, first_not_of, last_of, last_not_of}){
      if(offset == -1){
        printf("%s\n", *ptr++);
      } else {
        spaces[offset] = '^';
        printf("%s%s\n", *ptr++, spaces.c_str());
        spaces[offset] = ' ';
      }
    }
  }
  std::string tmp(test);
  std::shuffle(tmp.begin(), tmp.end(), util::xorshift_rng(std::random_device()));
  test = tmp;
  for(auto &&s : {symbols, letters, numbers}){
    offsets.push_back(test.find_first_of(s));
    offsets.push_back(test.find_first_not_of(s, offsets.back()));
    offsets.push_back(test.find_last_of(s));
    offsets.push_back(test.substr(0, offsets.back()+1).find_last_not_of(s));
  }
  for(auto &&s : {numbers, letters, symbols}){
    int last_not_of = offsets.pop();
    int last_of = offsets.pop();
    int first_not_of = offsets.pop();
    int first_of = offsets.pop();
    
    printf("%s%.*s\n%s%.*s\n",
           labels[0], (int)s.size(), s.data(), 
           labels[1], (int)test.size(), test.data());
    auto ptr = labels.data() + 2;
    for(auto offset : {first_of, first_not_of, last_of, last_not_of}){
      if(offset == -1){
        printf("%s\n", *ptr++);
      } else {
        spaces[offset] = '^';
        printf("%s%s\n", *ptr++, spaces.c_str());
        spaces[offset] = ' ';
      }
    }
  }
}
