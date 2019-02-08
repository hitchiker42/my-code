#include "regex.h"
#include "macros.h"
#define USE_CXX11_REGEX
//#define USE_PRCE_REGEX
//#define USE_RE2_REGEX
#if (defined USE_CXX11_REGEX)
#include <regex>
namespace util {
struct regex_impl : std::regex {
  using std::regex::regex;
};
void destroy_regex_impl(regex_impl *re){
  re->~regex_impl();
}
struct regex_match_impl : std::cmatch {
  using std::cmatch::cmatch;
};
void destroy_regex_match_impl(regex_match_impl *re_m){
  re_m->~regex_match_impl();
}
static const std::regex& get_regex(const regex& re){
  return *static_cast<const std::regex*>(re.impl);
}
static const std::cmatch& get_regex_match(const regex_match_result& re_m){
  return *static_cast<const std::cmatch*>(re_m.impl);
}
static std::regex& get_regex(regex& re){
  return *static_cast<std::regex*>(re.impl);
}
static std::cmatch& get_regex_match(regex_match_result& re_m){
  return *static_cast<std::cmatch*>(re_m.impl);
}
static constexpr std::regex_constants::syntax_option_type
convert_flags(util::regex_constants::syntax_option_type f){
  namespace s = std::regex_constants;
  namespace u = util::regex_constants;
  static constexpr syntax_option_type syntax_option_0 =
    static_cast<s::syntax_flag_type>(0);
#define get_flag(fl) ((f & u::CAT(syntax_,fl)) ? s::fl : syntax_option_0)
  return (get_flag(icase) | get_flag(nosubs) | get_flag(optimize) |
          get_flag(basic) | get_flag(extended) |
          ((f & u::syntax_perl) ? s::EMCAScript : syntax_option_0));
#undef get_flag
}
static constexpr std::regex_constants::match_flag_type
convert_flags(util::regex_constants::match_flag_type f){
  namespace s = std::regex_constants;
  namespace u = util::regex_constants;
  static constexpr match_flag_type match_flag_0 =
    static_cast<s::match_flag_type>(0);
#define get_flag(fl) ((f & u::CAT(match_,fl)) ? s::CAT(match_, fl) : match_flag_0)
  return (get_flag(not_bol) | get_flag(not_eol) |
          get_flag(not_null) |
          ((f & u::match_anchored) ? s::match_continuous : match_flag_0));
#undef get_flag
}
explicit regex::regex(const char *s, flag_type f){
  this->impl = new regex_impl(s, convert_flags(f));
}
regex::regex(const char *s, size_t count, flag_type f){
  this->impl = new regex_impl(s, count, convert_flags(f));
}
regex::regex(std::string_view s, flag_type f){
  this->impl = new regex_impl(s.data(), s.size(), convert_flags(f));
}
regex::regex(const regex& other){
  this->impl = new regex_impl(*other.impl);
}
regex::regex(regex&& other){
  this->impl = new regex_impl(std::move(*other.impl));
}
regex_match_result::regex_match_result(){
  this->impl = new regex_match_impl();
}
regex_match_result::regex_match_result(const regex_match_result& other){
  this->impl = new regex_match_impl(*other.impl);
}
regex_match_result::regex_match_result(regex_match_result&& other){
  this->impl = new regex_match_impl(std::move(*other.impl));
}
regex_submatch regex_match_result::operator[](size_t idx) const {
  const std::sub_match& tmp = get_match_result(*this)[idx];
  if(tmp.matched){
    return regex_submatch(tmp.first, tmp.second);
  } else {
    return regex_submatch();
  }
}
bool regex_match(const char *begin, const char *end,
                 regex_match_result& m, const regex &re,
                 regex_constants::match_flag_type flags =
                 regex_constants::match_default){
  return std::regex_match(begin, end, get_regex_match(m), get_regex(re),
                          convert_flags(flags));
}
bool regex_match(const char *begin, const char *end,
                 const regex &re,
                 regex_constants::match_flag_type flags =
                 regex_constants::match_default){
  return std::regex_match(begin, end, get_regex(re), convert_flags(flags));
}
bool regex_search(const char *begin, const char *end,
                  regex_match_result& m, const regex &re,
                  regex_constants::match_flag_type flags =
                    regex_constants::match_default){
  return std::regex_search(begin, end, get_regex_match(m), get_regex(re),
                           convert_flags(flags));
}
bool regex_search(const char *begin, const char *end,
                  const regex &re,
                  regex_constants::match_flag_type flags =
                  regex_constants::match_default){
    return std::regex_search(begin, end, get_regex(re),
                           convert_flags(flags));
}
}
#elif (defined USE_RE2_REGEX)
//MOVE TO HEADER & change api.
#elif (defined USE_PCRE_REGEX)
//regex_impl / regex_match_impl are empty, since pcre2_code/pcre2_match_data
//are opaque we just cast the pointers.
struct regex_impl {};
struct regex_match_impl {};
void destroy_regex_impl(regex_impl *re){
  pcre2_code_free((pcre2_code*)re);
}
void destroy_regex_match_impl(regex_match_impl *re_m){
  pcre2_match_data_free((pcre2_match_data*)re_m);
}
static const pcre2_code* get_regex(const regex& re){
  return (const pcre2_code*)re.impl;
}
static const pcre2_match_data* get_regex_match(const regex_match& re_m){
  return (const pcre2_match_data*)re_m.impl;
}
static pcre2_code* get_regex(regex& re){
  return (pcre2_code*)re.impl;
}
static pcre2_match_data* get_regex_match(regex_match& re_m){
  return (pcre2_match_data*)re_m.impl;
}
static constexpr uint32_t
convert_flags(util::regex_constants::syntax_option_type f){
  namespace u = util::regex_constants;
#define get_flag(fl,opt) ((f & u::CAT(syntax_,fl)) ? CAT(PCRE2_,opt) : 0)
  return (get_flag(icase, CASELESS) | get_flag(nosubs, NO_AUTO_CAPTURE)
          get_flag(multiline, MULTILINE) | get_flag(dot_newline, DOTALL));
#undef get_flag
}
static constexpr uint32_t
convert_flags(util::regex_constants::match_flag_type f){
  namespace u = util::regex_constants;
#define get_flag(fl,opt) ((f & u::CAT(match_,fl)) ? CAT(PCRE2_, opt) : 0)
  return (get_flag(not_bol, NOTBOL) | get_flag(not_eol, NOTEOL) |
          get_flag(not_null, NOTEMPTY) |
          get_flag(anchored, ANCHORED));
#undef get_flag
}
regex_impl* create_regex(const char *str, size_t len, flag_type f){
  if(!(f & util::regex_constants::syntax_perl)){
    return nullptr;
  }
  int err;
  size_t offset;
  pcre2_code* code = pcre2_compile(str, len, convert_flags(f),
                                   &err, &offset, nullptr);
  if(f & util::regex_constants::optimize){
    pcre2_jit_compile(code, PCRE2_JIT_COMPLETE);
  }
  return (regex_impl*)code;
}
explicit regex::regex(const char *s, flag_type f){
  this->impl = create_regex(s, strlen(s), f);
}
regex::regex(const char *s, size_t count, flag_type f){
  this->impl = create_regex(s, count, f);
}
regex::regex(std::string_view s, flag_type f){
  this->impl = create_regex(s.data(), s.size(), f);
}
#endif //USE_X_REGEX

long regex_submach::to_long(){
  errno = 0;
  if(this->matched){
    char *end;
    const char *str = this->begin();
    long ret = strtol(str, &end, 0);
    //since sub matches aren't null terminated we need to manually check
    //if we went past the end of the submatch, this is only a partial solution
    //and will cause some false negitives, I'll replace this with to_chars
    //either the std version or my own when its available.
    if(end == str || (end > this->end())){
      errno = EINVAL;
      return 0;
    }
    return ret;
  }
  errno = EINVAL;
  return 0;
}
double regex_submatch::to_float(){
  errno = 0;
  if(this->matched){
    char *end;
    const char *str = this->begin();
    double ret = strtod(str, &end);
    //since sub matches aren't null terminated we need to manually check
    //if we went past the end of the submatch, this is only a partial solution
    //and will cause some false negitives, I'll replace this with to_chars
    //either the std version or my own when its available.
    if(end == str || (end > this->end())){
      errno = EINVAL;
      return 0;
    }
    return ret;
  }
  errno = EINVAL;
  return 0;
}
