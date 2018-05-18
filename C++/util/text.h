#ifndef __TEXT_H__
#define __TEXT_H__
#include "util.h"
#include "string_view.h"
#inlcude "string_buf.h"
#include "my_string.h"
namespace util {
static inline string_view ltrim(const string_view& str,
                                const string_view& filter = " \n\t\v\f\r"_sv){
  return find_first_of_not(str.begin(), std.end(),
                           filter.begin(), filter.end());
}
static inline const char* ltrim(const char *str,
                                const char *filter = " \n\t\v\f\r"){
  return str + strspn(str, filter);
}
//returns length of trimmed string
static inline size_t rtrim(const char *str, size_t size_1,
                           const char *filter, size_t size_2){
  size_t idx;
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
static inline size_t rtrim(const char *str, const char *filter = " \n\t\v\f\r"){
  //No confident library function here.
  return rtrim(str, strlen(str), filter, strlen(filter));
}
static inline void trim(const char *str, size_t size_1,
                        const char *filter, size_t size_2,
                        const char **start, size_t *end){
  //Use the same table for both rtrim and ltrim.
  size_t idx;
  uint8_t table[256] = {0};
  for(idx = 0; idx < size_2; ++idx){
    table[(uint8_t)filter[idx]] = 1;
  }
  uint8_t *s = (uint8_t)str;
  size_t idx1 = 0, idx2 = size_1 - 1;
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
static inline std::string_view trim(const char *str, size_t size_1,
                                    const char *filter = " \n\t\v\r\f",
                                    size_t size_2 = sizeof("\n\t\v\r\f")-1){
  const char *start = NULL;
  size_t end = 0;
  trim(str, size_1, filter, size_2, &start, &end);
  size_t len = end - (start - str);
  return std::string_view(start, len);
}
static inline std::string_view trim(const char *str, 
                                    const char* filter = " \n\t\v\r"){
  return trim(str, strlen(str), filter, strlen(filter));
}
static inline std::vector<std::string_view> split(const char *str, size_t size_1,
                                                  const char *filter, size_t size_2){
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
