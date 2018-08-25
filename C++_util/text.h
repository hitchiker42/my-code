#ifndef __TEXT_H__
#define __TEXT_H__
#include <string_view>
#include <vector>
#include <string.h>
using namespace std::literals::string_view_literals;
namespace util {
static constexpr const char* whitespace_cstr = " \n\t\v\f\r";
static const std::string_view whitespace_sv = " \n\t\v\f\r"sv;
//Functions using std::string_views, these are generally much cleaner
//and eaiser to write than functions using c strings.
inline std::string_view ltrim(const std::string_view sv,
                              const std::string_view filter = whitespace_sv){
  size_t start = sv.find_first_not_of(filter);
  return (start == (size_t)-1 ? ""sv : sv.substr(start, sv.size() - start));
}
inline std::string_view rtrim(const std::string_view sv, 
                                const std::string_view filter = whitespace_sv){
  size_t len = sv.find_last_not_of(filter);
  return (len == (size_t)-1 ? ""sv : sv.substr(0, len));
}
inline std::string_view trim(const std::string_view sv, 
                               const std::string_view filter = whitespace_sv){
  return ltrim(rtrim(sv,filter),filter);
}
inline std::vector<std::string_view> split(const std::string_view sv,
                                           const std::string_view filter = whitespace_sv){
  std::vector<std::string_view> ret;
  size_t l = sv.find_first_not_of(filter);
  while(l != (size_t)-1){
    size_t r = std::min(sv.find_first_of(filter, l), sv.size());
    size_t len = r-l;
    ret.emplace_back(sv.substr(l, len));
    l = sv.find_first_not_of(filter, r);
  }
  return ret;
}
//Versions using cstrings

//Functions using c strings, ultimately these should be faster.
//Each function has two versions one which takes explicit lengths
//and will work with arbitrary bytes and one which doesn't
//and requires nul terminated strings.

//returns index to first char in str that isn't in filter, or
//size_1 if all characters are.
inline size_t ltrim(const char *str, ssize_t size_1,
                    const char *filter, ssize_t size_2){
  ssize_t idx;
  uint8_t table[256] = {0};
  for(idx = 0; idx < size_2; ++idx){
    table[(uint8_t)filter[idx]] = 1;
  }
  uint8_t *s = (uint8_t*)str;
  for(idx = 0; idx < size_1 && table[s[idx]]; ++idx){
    ;//do nothing
  }
  return idx;
}
//we can use strspn here since not providing explicit lengths
//precludes embedded nul bytes.
inline size_t ltrim(const char *str,
                    const char *filter = whitespace_cstr){
  return strspn(str, filter);
}
//returns length of trimmed string
inline size_t rtrim(const char *str, ssize_t size_1,
                    const char *filter, ssize_t size_2){
  ssize_t idx;
  uint8_t table[256] = {0};
  for(idx = 0; idx < size_2; ++idx){
    table[(uint8_t)filter[idx]] = 1;
  }
  uint8_t *s = (uint8_t*)str;
  for(idx = (size_1-1); idx >= 0 && table[s[idx]]; --idx){
    ;//do nothing
  }  
  return (idx+1);
}
inline size_t rtrim(const char *str,
                    const char *filter = whitespace_cstr){
  return rtrim(str, strlen(str),
               filter, strlen(filter));
}
inline void trim(const char *str, ssize_t size_1,
                 const char *filter, ssize_t size_2,
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
  *end = (size_t)idx2;
  return;
}
inline void trim(const char *str, const char *filter,
                 const char **start, size_t *end){
  return trim(str, strlen(str), filter, strlen(filter), start, end);
}
inline std::vector<std::string_view> split(const char *str, size_t size_1,
                                           const char *filter, size_t size_2){
  size_t idx;
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
inline std::vector<std::string_view> split(const char *str,
                                           const char *filter = whitespace_cstr){
  return split(str, strlen(str), filter, strlen(filter));
}
}
#endif /* __TEXT_H__ */
