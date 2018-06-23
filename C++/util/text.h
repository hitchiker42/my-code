#ifndef __TEXT_H__
#define __TEXT_H__
#include "util.h"
#include "string_view.h"
#include "string_buf.h"
#include "my_string.h"
namespace util {
using namespace util::literals::string_literals;
static constexpr const char* whitespace_cstr = " \n\t\v\f\r";
static const string_view whitespace_sv = " \n\t\v\f\r"_sv;
//Functions using string_views, these are generally much cleaner
//and eaiser to write than functions using c strings.
static inline string_view ltrim(const string_view sv,
                                const string_view filter = whitespace_sv){
  size_t start = sv.find_first_of_not(filter);
  return sv.substr(start, sv.size() - start);
}
static inline string_view rtrim(const string_view sv, 
                                const string_view filter = whitespace_sv){
  auto it = util::find_first_of_not(sv.rbegin(), sv.rend(),
                                    filter.begin(), filter.end());
  auto len = std::distance(sv.begin(), it.base()); 
  return sv.substr(0, len);
}
static inline string_view trim(const string_view sv, 
                               const string_view filter = whitespace_sv){
  auto l = util::find_first_of_not(sv.begin(), sv.end(),
                                   filter.begin(), filter.end());
  auto r = util::find_first_of_not(sv.rbegin(), sv.rend(),
                                    filter.begin(), filter.end());  
  return sv.substr(std::distance(sv.begin(), l), 
                   std::distance(sv.begin(), r.base()));
}
static inline svector<string_view> split(const string_view sv,
                                         const string_view filter = whitespace_sv){
  svector<string_view> ret;
  auto l = util::find_first_of_not(sv.begin(), sv.end(),
                                   filter.begin(), filter.end());
  while(l != sv.end()){
    auto r = std::find_first_of(l, sv.end(), filter.begin(), filter.end());    
    size_t len = r-l;
    ret.emplace_back(l, len);
    l = util::find_first_of_not(r, sv.end(),
                                filter.begin(), filter.end());
  }
  return ret;
}
//Functions using c strings, ultimately these should be faster.
static inline const char* ltrim(const char *str,
                                const char *filter = whitespace_cstr){
  return str + strspn(str, filter);
}
//returns length of trimmed string
static inline size_t rtrim(const char *str, size_t size_1,
                           const char *filter, size_t size_2){
  ssize_t idx;
  uint8_t table[256] = {0};
  for(idx = 0; idx < size_2; ++idx){
    table[(uint8_t)filter[idx]] = 1;
  }
  for(idx = (size_1-1); idx >= 0; --idx){
    uint8_t ch = (uint8_t)str[idx];
    if(!table[ch]){
      break;
    }
  }
  return idx;
}
static inline void trim(const char *str, size_t size_1,
                        const char *filter, size_t size_2,
                        const char **start, size_t *end){
  //Use the same table for both rtrim and ltrim.
  ssize_t idx;
  uint8_t table[256] = {0};
  for(idx = 0; idx < size_2; ++idx){
    table[(uint8_t)filter[idx]] = 1;
  }
  uint8_t *s = (uint8_t*)str;
  ssize_t idx1 = 0, idx2 = size_1 - 1;
  while(idx1 < size_1 && table[s[idx1]]){
    ++idx1;
  }
  while(idx2 >= 0 && table[s[idx2]]){
    --idx2;
  }
  *start = str + idx1;
  *end = idx2;
  return;
}
static inline void trim(const char *str, const char *filter,
                        const char **start, size_t *end){
  return trim(str, strlen(str), filter, strlen(filter), start, end);
}
static inline std::vector<std::string_view> split(const char *str, size_t size_1,
                                                  const char *filter, size_t size_2){
  ssize_t idx;
  uint8_t table[256] = {0};
  for(idx = 0; idx < size_2; ++idx){
    table[(uint8_t)filter[idx]] = 1;
  }  
  size_t idx1 = 0;
  std::vector<std::string_view> ret;
  //trim left of string.
  while(idx1 < size_1 && table[(uint8_t)str[idx1]]){
    ++idx1;
  }
  size_t idx2 = idx1;
  while(idx1 < size_1){    
    //find end of word.
    while(idx2 < size_1 && !table[(uint8_t)str[idx2]]){
      ++idx2;
    }
    ret.emplace_back(str + idx1, idx2-idx1);
    //skip over delimiters.
    while(idx2 < size_1 && table[(uint8_t)str[idx2]]){
      ++idx2;
    }
    idx1 = idx2;
  }
  return ret;
}
}

#endif /* __TEXT_H__ */
