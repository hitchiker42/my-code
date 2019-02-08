#ifndef __REGEX_H__
#define __REGEX_H__
#include <string_view>
#include <string> //We could remove this if we removed a couple convenience funcitons
#include <type_traits>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "macros.h"
/*
  Header
*/
namespace util {
struct regex_impl; //opaque handle to a regex
struct regex_match_impl; //opaque handle to a regex match
void destroy_regex_impl(regex_impl *re);
void destroy_regex_match_impl(regex_match_impl *re_m);
//Constants for options for compiling / matching regexes
namespace regex_constants {
enum syntax_option_type : unsigned int {
//Different regex syntaxes, all mutually exclusive.
  syntax_perl = (1 << 0), // Be as perl compatiable as possible
  syntax_basic = (1 << 1), // Standard POSIX regex
  syntax_extended = (1 << 2), // Extended POSIX regex
//Special options
  syntax_icase = (1 << 4), // Match case insensitively
  syntax_nosubs = (1 << 5), // Don't save submatches
  syntax_optimize = (1 << 6), // Take extra time to optimize the regex
  //Not supported by C++11 regexes (not all implementations anyway).
  syntax_multiline = (1 << 7), // ^/$ match begin/end of line not the whole string
  syntax_dot_newline = (1 << 8), //'.' matches a newline
// Synonyms
  syntax_default = syntax_perl,
  syntax_posix = syntax_basic,
};
gen_simple_enum_bitwise_operators(syntax_option_type);
enum match_flag_type : unsigned int {
  match_default = 0,
  match_not_bol = (1 << 0), //^ doesn't match the begining of the string
  match_not_eol = (1 << 1), //$ doesn't match the end of the string
  match_not_null = (1 << 2), //empty matches are not concidered
  match_anchored = (1 << 3) //only match at the first character
};
gen_simple_enum_bitwise_operators(match_flag_type);
}
struct regex {
  using flag_type = regex_constants::syntax_option_type;
  regex_impl* impl;
  explicit regex(const char *s, flag_type f = regex_constants::syntax_perl);
  regex(const char *s, size_t count, flag_type f = regex_constants::syntax_perl);
  regex(std::string_view str, flag_type f = regex_constants::syntax_perl);
  regex(const regex& other);
  regex(regex&& other);
  ~regex(){
    destroy_regex_impl(impl);
  }
};
/*
  Represents a possible match to a subexpression in regexp.
  It is a pair of char*s pointing to the start of the match and 1 past
  the end of the match. The pointers are equal for a zero length match
  and are set to null if there was no match.
*/
struct regex_submatch {
  const char *first = nullptr;
  const char *last = nullptr;
  constexpr regex_submatch() = default;
  constexpr regex_submatch(const char *f, const char *l)
    : first{f}, last{l} {}
  const char* begin() const {
    return first;
  }
  const char* end() const {
    return last;
  }
  size_t length() const {
    return last - first;
  }
  bool matched() const {
    return first && last;
  }
  std::string_view sv() const {
    return std::string_view(first, last-first);
  }
  std::string str() const {
    return std::string(first, last-first);
  }
  long to_long() const;
  int to_int() const {
    return static_cast<int>(to_long());
  }
  double to_float() const;
};
struct regex_match_result {
  regex_match_impl* impl;
  regex_match_result();
  regex_match_result(const regex_match_result& other);
  regex_match_result(regex_match_result&& other);
  regex_match_result& operator=(const regex_match_result& other);
  regex_match_result& operator=(regex_match_result&& other);  
  ~regex_match_result(){
    destroy_regex_match_impl(impl);
  }
  //Can't return a reference here since we may need to create a regex_submatch
  regex_submatch operator[](size_t) const;
};
bool regex_match(const char *begin, const char *end,
                 regex_match_result& m, const regex &re,
                 regex_constants::match_flag_type flags =
                   regex_constants::match_default);
//IDEA: maybe change the signature of this version so I can have
//a version which returns a regex_match_result.
bool regex_match(const char *begin, const char *end,
                 const regex &re,
                 regex_constants::match_flag_type flags =
                   regex_constants::match_default);
inline bool regex_match(const std::string_view sv,
                        regex_match_result& m, const regex &re,
                        regex_constants::match_flag_type flags =
                          regex_constants::match_default){
  return regex_match(sv.begin(), sv.end(), m, re, flags);
}
inline bool regex_match(const std::string_view sv,
                        const regex &re,
                        regex_constants::match_flag_type flags =
                          regex_constants::match_default){
  return regex_match(sv.begin(), sv.end(), re, flags);
}
inline bool regex_match(const std::string_view sv,
                        regex_match_result& m, const regex &re,
                        regex_constants::match_flag_type flags =
                          regex_constants::match_default){
  return regex_match(sv.begin(), sv.end(), m, re, flags);
}
bool regex_search(const char *begin, const char *end,
                  regex_match_result& m, const regex &re,
                  regex_constants::match_flag_type flags =
                    regex_constants::match_default);
bool regex_search(const char *begin, const char *end,
                  const regex &re,
                  regex_constants::match_flag_type flags =
                    regex_constants::match_default);
inline bool regex_search(const std::string_view sv,
                         regex_match_result& m, const regex &re,
                         regex_constants::match_flag_type flags =
                           regex_constants::match_default){
  return regex_search(sv.begin(), sv.end(), m, re, flags);
}
inline bool regex_search(const std::string_view sv,
                         const regex &re,
                         regex_constants::match_flag_type flags =
                           regex_constants::match_default){
  return regex_search(sv.begin(), sv.end(), re, flags);
}


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
void set_backreferences(const regex_match_result& m);
void init_backreferences(const regex_match_result& match){
  clear_backreferences();
  set_backreferences(match);
}
bool regex_match_backreferences(const std::string_view& str,
                                const regex& re,
                                regex_constants::match_flag_type flags =
                                regex_constants::match_default){
  regex_match_result m;
  bool matched = regex_match(str.begin(), str.end(),  m, re, flags);
  if(matched){ set_backreferences(m); }
  return matched;
}
}
struct regex_matcher {
  using regex_flag_type = regex_constants::syntax_option_type;
  using match_flag_type = regex_constants::match_flag_type;
  using string_view = std::string_view;
  //The intent is to use this structure for iterating over an entire
  //file, so the default options cause ^ and $ to only match the beginning
  //and end of lines, not the beginning and end of the input.
  //For some reason regex_constants::multiline doesn't exist, despite
  //being part of C++17
  static inline constexpr regex_flag_type default_regex_flags =
    (regex_constants::syntax_perl | regex_constants::syntax_multiline |
     regex_constants::syntax_optimize);
  static inline constexpr match_flag_type default_match_flags =
    (regex_constants::match_not_bol | regex_constants::match_not_eol);


  //These next 3 fields are theoretically const, but making a member variable
  //const is too much of a hassle.
  regex pattern;
  regex_match_result last_match;
  const char *text;
  size_t text_len;
  size_t offset = 0;
  //  std::vector<string_view> submatches;
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
    bool result = regex_search(text + offset, text + text_len,
                               last_match, pattern, flags);
    offset = (result ? (last_match[0].end() - text) : text_len);
    return result;
  }
  bool next_match_bind(match_flag_type flags = default_match_flags){
    bool result = regex_search(text + offset, text + text_len,
                               last_match, pattern, flags);
    offset = (result ? (last_match[0].end() - text) : text_len);
    if(result){
      regex_backreferences::init_backreferences(last_match);
    }
    return result;
  }
  bool sub_match_matched(int n) const {
    return last_match[n].matched();
  }
  bool matched(int n = 0) const {
    return last_match[n].matched();
  }
  string_view sub_match(int n) const {
    const auto& sm = last_match[n];
    return (sm.matched() ? string_view(sm.first, sm.length()) : string_view());
  }
  //There's no way to indicate an error here so we need to rely on errno
  long sub_match_as_int(int n) const {
    return last_match[n].to_long();
  }
  double sub_match_as_double(int n) const {
    return last_match[n].to_float();
  }
  //same as sub_match, but using the name of the emacs function with
  //the same purpose, I may remove this.
  string_view match_string(int n = 0) const {
    return sub_match(n);
  }
  /*
  const std::vector<string_view>& sub_matches(std::vector<string_view> &subs){
    subs.resize(last_match.size());
    for(size_t i = 0; i < last_match.size(); i++){
      subs[i] = sub_match(i);
    }
    return subs;
    }*/
  void set_backreferences(){
    regex_backreferences::init_backreferences(last_match);
  }
};
} /* namespace util */

#endif /* __REGEX_H__ */
