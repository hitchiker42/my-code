#ifndef __REGEX_H__
#define __REGEX_H__
#include <regex>
#include <string_view>
#include <stdlib.h>
namespace util {
//TODO (maybe): create static/thread local string_views _0,_1,_2,_3,..._N to
//bind submatches to, i.e
//using namespace util::regex_backreferences;
//auto re_m = regex_matcher("(\d)(\d)(\d)","123 456 789");
//while(re_m.next_match_bind()){
//fmt::format("{}|{}|{}\n",_1,_2,_3);
//}
//Output: 1|2|3\n4|5|6\n7|8|9\n  

//Restricted to using a const char* + size as the text to search
//for now to allow using string_views for matches.
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
  bool sub_match_matched(int n){
    return last_match[n].matched;
  }
  string_view sub_match(int n){
    const auto& sm = last_match[n];
    return (sm.matched ? string_view(sm.first, sm.length()) : string_view());
  }
  //same as sub_match, but using the name of the emacs function with
  //the same purpose.
  string_view match_string(int n = 0){
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
};
} /* namespace util */  

#endif /* __REGEX_H__ */
