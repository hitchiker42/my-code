#ifndef __REGEX_H__
#define __REGEX_H__
#include <regex>
#include <string_view>
#include <stdlib.h>
#include <errno.h>
namespace util {
namespace regex_backreferences {
inline thread_local std::string_view backreferences[10];
inline thread_local std::string_view &_0 = backreferences[0];
inline thread_local std::string_view &_1 = backreferences[1];
inline thread_local std::string_view &_2 = backreferences[2];
inline thread_local std::string_view &_3 = backreferences[3];
inline thread_local std::string_view &_4 = backreferences[4];
inline thread_local std::string_view &_5 = backreferences[5];
inline thread_local std::string_view &_6 = backreferences[6];
inline thread_local std::string_view &_7 = backreferences[7];
inline thread_local std::string_view &_8 = backreferences[8];
inline thread_local std::string_view &_9 = backreferences[9];
void clear_backreferences(){
  memset(backreferences, '\0', sizeof(std::string_view) * 10);
}
void set_backreferences(const std::cmatch& m){
  for(size_t i = 0; i < m.size(); i++){
    //placement new still sucks.
    new (backreferences + i) std::string_view(m[i].first, m[i].length());
  }
}
void init_backreferences(const std::cmatch& match){
  clear_backreferences();
  set_backreferences(match);
}
bool regex_match_backreferences(const std::string_view& str,
                                const std::regex& re,
                                std::regex_constants::match_flag_type flags =
                                std::regex_constants::match_default){
  std::cmatch m;
  bool matched = std::regex_match(str.begin(), str.end(),  m, re, flags);
  if(matched){ set_backreferences(m); }
  return matched;
}
}
struct regex_matcher {
  using regex_flag_type = std::regex_constants::syntax_option_type;
  using match_flag_type = std::regex_constants::match_flag_type;
  using string_view = std::string_view;
  //The intent is to use this structure for iterating over an entire
  //file, so the default options cause ^ and $ to only match the beginning
  //and end of lines, not the beginning and end of the input.
  //For some reason std::regex_constants::multiline doesn't exist, despite
  //being part of C++17
  static inline constexpr regex_flag_type default_regex_flags =
    (std::regex_constants::ECMAScript /*| std::regex_constants::multiline*/ |
     std::regex_constants::optimize);
  static inline constexpr match_flag_type default_match_flags =
    (std::regex_constants::match_not_bol | std::regex_constants::match_not_eol);


  //These next 3 fields are theoretically const, but making a member variable
  //const is too much of a hassle.
  std::regex pattern;
  const char *text;
  size_t text_len;
  size_t offset = 0;
  std::cmatch last_match;
  std::vector<string_view> submatches;
  regex_matcher(const char *pat, size_t pat_len,
                const char *text, size_t text_len,
                regex_flag_type flags = default_regex_flags)
    : pattern(pat, pat_len, flags), text{text}, text_len{text_len} {}
  //This works for std::strings + null terminated strings due to them
  //being implicitly convertible to string_views
  regex_matcher(string_view pat, string_view text,
                regex_flag_type flags = default_regex_flags)
    : pattern(pat.data(), pat.size(), flags),
      text{text.data()}, text_len{text.size()} {}

  bool next_match(match_flag_type flags = default_match_flags){
    bool result = std::regex_search(text + offset, text + text_len,
                                    last_match, pattern, flags);
    offset = (result ? (last_match[0].second - text) : text_len);
    return result;
  }
  bool next_match_bind(match_flag_type flags = default_match_flags){
    bool result = std::regex_search(text + offset, text + text_len,
                                    last_match, pattern, flags);
    offset = (result ? (last_match[0].second - text) : text_len);
    if(result){
      regex_backreferences::init_backreferences(last_match);
    }
    return result;
  }
  bool sub_match_matched(int n) const {
    return last_match[n].matched;
  }
  bool matched(int n = 0) const {
    return last_match[n].matched;
  }  
  string_view sub_match(int n) const {
    const auto& sm = last_match[n];
    return (sm.matched ? string_view(sm.first, sm.length()) : string_view());
  }
  //There's no way to indicate an error here so we need to rely on errno
  long sub_match_as_int(int n) const {
    const auto& sm = last_match[n];
    errno = 0;
    if(sm.matched){
      char *end;
      const char *str = sm.first;
      long ret = strtol(str, &end, 0);
      //since sub matches aren't null terminated we need to manually check
      //if we went past the end of the submatch, this is only a partial solution
      //and will cause some false negitives, I'll replace this with to_chars
      //either the std version or my own when its available.
      if(end == str || (end > sm.second)){
        errno = EINVAL;
        return 0;
      }
      return ret;
    }
    errno = EINVAL;
    return 0;
  }
  double sub_match_as_double(int n) const {
    const auto& sm = last_match[n];
    errno = 0;
    if(sm.matched){
      char *end;
      const char *str = sm.first;
      double ret = strtod(str, &end);
      //since sub matches aren't null terminated we need to manually check
      //if we went past the end of the submatch, this is only a partial solution
      //and will cause some false negitives, I'll replace this with to_chars
      //either the std version or my own when its available.
      if(end == str || (end > sm.second)){
        errno = EINVAL;
        return 0;
      }
      return ret;
    }
    errno = EINVAL;
    return 0;
  }    
  //same as sub_match, but using the name of the emacs function with
  //the same purpose.
  string_view match_string(int n = 0) const {
    return sub_match(n);
  }
  //We could cache 'submatches' by keeping a flag indicating if we
  //already filled it. But I don't see any reason for calling
  //sub_matches twice for the same match.
  const std::vector<string_view>& sub_matches(){
    submatches.resize(last_match.size());
    for(size_t i = 0; i < last_match.size(); i++){
      submatches[i] = sub_match(i);
    }
    return submatches;
  }
  void set_backreferences(){
    regex_backreferences::init_backreferences(last_match);
  }
};
} /* namespace util */

#endif /* __REGEX_H__ */
