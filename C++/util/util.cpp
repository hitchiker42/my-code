#include "util.h"
std::string_view string_trim(const std::string &str,
                             const char* delim){
  const char *begin = str.data();
  const char *end = str.data() + str.size()-1;

  begin = begin + strspn(begin, delim); //Strip leading chars
  if(begin > end){ return std::string_view(); } //check for empty string
  while(strchr(*end, delim)) { end--; } //strip trailing chars

  return std::string_view(begin, (ptrdiff_t)((end+1) - begin));
}


template<bool case_insensitive>
int strcmp_nospace_impl(const char *str1, const char *str2) {
  while((*str1) && (*str2)){
    //Need to strip spaces from both strings before
    //checking if we reach the end since if we get to the end of
    ///both strings then they're equal.
    while((*str1) == ' '){ ++str1; }
    while((*str2) == ' '){ ++str2; }
    if(!(*str1) || !(*str2)){ break; }
    int c1, c2;
    if constexpr (case_insensitive) {
      c1 = tolower_ascii(*str1++);
      c2 = tolower_ascii(*str2++);
    } else {
      c1 = *str1++;
      c2 = *str2++;
    }
    if(c1 != c2) {
      //if c2 is greater str1 is less and this is negitive, and vice versa.
      return c1 - c2;
    }
  }
  if(!(*str1)){
    return (*str2 ? -1 : 0);
  } else {
    return 1; //str2 was shorter than str1
  }
}
int strcmp_nospace(const char *str1, const char *str2) {
  return strcmp_nospace_impl<false>(str1,str2);
}
int strcasecmp_nospace(const char *str1, const char *str2) {
  return strcmp_nospace_impl<true>(str1,str2);
}
#ifdef PLATFORM_WINDOWS
//quick and dirty strcasecmp implementation.
int strcasecmp(const char *str1, const char *str2) {
  while((*str1) && (*str2)){
    int c1 = tolower_ascii(*str1++);
    int c2 = tolower_ascii(*str2++);
    if(c1 != c2){
      return c1 - c2;
    }
  }
  if(!(*str1)){
    return ((*str2) ? -1 : 0);
  } else {
    return 1; //str1 was longer;
  }
}
#endif
int string_compare_natural(const std::string &a, const std::string &b,
                           bool foldcase, bool ignore_leading_zeros){
  return string_compare_natural(a.c_str(), b.c_str(), foldcase, ignore_leading_zeros);
};
int string_compare_natural(const char *a, const char *b,
                           bool foldcase, bool ignore_leading_zeros){
  bool dig_a = isdigit(a);
  bool dig_b = isdigit(b);
  //If one begins with a digit and one doesn't just use strcmp
  if(dig_a != dig_b){
    return (foldcase ? strcasecmp(a,b) : strcmp(a,b));
  }
  if(dig_a){
    return string_compare_natural_digits(a,b);
  } else {
    return string_compare_natural_nondigits(a,b);
  }
}
static int string_compare_natural_digits(const char *a, const char *b,
                                         bool foldcase, bool ignore_leading_zeros){
  //when ignore_leading_zeros is false it's as if a decimal point is placed before
  //the leading 0, with ties being determined by length of the digit sequence.
  if(!ignore_leading_zeros && *a == '0' || *b == '0'){
    while(*a == *b && *a){
      ++a,++b;
    }
    if(!is_digit(a) && !is_digit(b)){
      return string_compare_natural_nondigits(a, b, foldcase, ignore_leading_zeros);
    } else if(!is_digit(a)){
      return *b;
    } else if(!is_digit(b)){
      return -(*a);
    } else {
      return (*a) - (*b);
    }
  }
  const char *a_start = a;
  const char *b_start = b;
  long a_val = strtol(a, &a, 10);
  long b_val = strtol(b, &b, 10);
  if(a_val != b_val){
    return a_val - b_val;
  }
  if(*a && *b){
    return string_compare_natural_nondigits(a, b, foldcase, ignore_leading_zeros);
  } else {
    return *a - *b;
  }
}
static int string_compare_natural_nondigits(const char *a, const char *b,
                                            bool foldcase, bool ignore_leading_zeros){
  //only need to check one string for a digit, since we only compare numbers
  //when they line up.
  if(foldcase){
    while(*a && !is_digit(*a) && tolower(*a) == tolower(*b)){
      ++a, ++b;
    }
  } else {
    while(*a && !isdigit(*a) && *a == *b){
      ++a, ++b;
    }
  }
  if(isdigit(*a) && isdigit(*b)){
    return string_compare_natural_nondigit(a, b, foldcase, ignore_leading_zeros);
  } else {
    return *a - *b;
  }
}
#if 0
std::string format_countable_noun(const std::string &article,
                                  const std::string &noun, int count,
                                  const std::string &plural){
  if (count == 1) {
    return article + " " + noun;
  } else {
    if (count < 10) {
      static constexpr std::array<const char*, 8> numbers = {{
        "two", "three", "four", "five",
        "six", "seven", "eight", "nine"
      }};
      return (numbers[count] + (" " + noun));
    } else {
      return std::to_string(count) + " " + noun + plural;
    }
  }
}
/*
  Question: Should I format a trailing number < 20 as 'and XX' or just 'XX'
  eg 1110011 as one million, eleven thousand, and eleven or
                one million, eleven thousand, eleven
  I like the version with and, but currently I do it the other way, should
  I change the code?
*/
static constexpr std::array<std::string_view, 20> cardinal_ones_list = {{
  ""sv, "one"sv, "two"sv, "three"sv, "four"sv, "five"sv, "six"sv, "seven"sv,
  "eight"sv, "nine"sv, "ten"sv, "eleven"sv, "twelve"sv, "thirteen"sv,
  "fourteen"sv, "fifteen"sv, "sixteen"sv, "seventeen"sv, "eighteen"sv,
  "nineteen"sv
}};
static constexpr std::array<std::string_view, 10> cardinal_tens_list = {{
  ""sv, ""sv, "twenty"sv, "thirty"sv, "forty"sv, "fifty"sv,
  "sixty"sv, "seventy"sv, "eighty"sv, "ninety"sv
}};
static constexpr std::array<std::string_view, 22> cardinal_thousand_block_list = {{
  ""sv, " thousand"sv, " million"sv, " billion"sv, " trillion"sv,
  " quadrillion"sv, " quintillion"sv, " sextillion"sv, " septillion"sv,
  " octillion"sv, " nonillion"sv, " decillion"sv, " undecillion"sv,
  " duodecillion"sv, " tredecillion"sv, " quattuordecillion"sv,
  " quindecillion"sv, " sexdecillion"sv, " septendecillion"sv,
  " octodecillion"sv, " novemdecillion"sv, " vigintillion"sv
}};
static constexpr std::array<std::string_view, 20> ordinal_ones_list = {{
    ""sv, "first"sv, "second"sv, "third"sv, "fourth"sv, "fifth"sv,
    "sixth"sv, "seventh"sv, "eighth"sv, "ninth"sv, "tenth"sv, "eleventh"sv,
    "twelfth"sv, "thirteenth"sv, "fourteenth"sv,"fifteenth"sv, "sixteenth"sv,
    "seventeenth"sv, "eighteenth"sv, "nineteeth"sv
}};
static constexpr std::array<std::string_view, 10> ordinal_tens_list = {{
    ""sv, "tenth"sv, "twentieth"sv, "thirtieth"sv, "fortieth"sv, "fiftieth"sv,
    "sixtieth"sv, "seventieth"sv, "eightieth"sv, "ninetieth"sv
}};
static constexpr std::string_view sv_space = " "sv;
static constexpr std::string_view sv_dash = "-"sv;
static constexpr std::string_view sv_zero = "zero"sv;
static constexpr std::string_view sv_hundred = " hundred"sv;
static constexpr std::string_view sv_minus = "minus "sv;
static constexpr std::string_view sv_thousands_sep = ", "sv;
//Use an array on the stack for potential optimiziation
//static util::array<std::string_view, 6> void format_small_cardinal(unsigned long n) {
//  util::array<std::string_view, 6> ls(std::string_view(), 0);

//Returns the total number of characters added to ls
static size_t format_small_cardinal(unsigned long n,
                                    std::vector<std::string_view> &ls) {
  int hundreds = n / 100;
  int tens_and_ones = n % 100;
  int tens = tens_and_ones / 10;
  int ones = tens_and_ones % 10;
  size_t sz = 0;
  if (hundreds) {
    ls.push_back(cardinal_ones_list[hundreds]);
    sz += ls.back().size();
    ls.push_back(sv_hundred);
    sz += ls.back().size();
    if (tens_and_ones) {
      ls.push_back(sv_space);
      sz++;
    }
  }
  if (tens_and_ones == 0) { return sz; }
  if (tens_and_ones < 20) {
    ls.push_back(cardinal_ones_list[tens_and_ones]);
    sz += ls.back().size();
  } else {//tens >= 20
    ls.push_back(cardinal_tens_list[tens]);
    sz += ls.back().size();
    if (ones) {
      ls.push_back(sv_dash);
      ls.push_back(cardinal_ones_list[ones]);
      sz += ls.back().size() + 1;
    }
  }
  return sz;
}
const std::string format_cardinal_number (long number) {
  if (number == 0) { return "zero"; }
  if (number > 0 && number < 20) { return std::string(cardinal_ones_list[number]); }
  std::string ret;
  //optimze for small numbers
  if (abs(number) < 1000) {
    std::vector<std::string_view> ls;
    size_t sz = 0;
    if (number < 0) {
      ls.push_back(sv_minus);
      sz += ls.back().size();
      number = abs(number);
    }
    sz += format_small_cardinal(number, ls);
    ret.resize(sz);
    auto it = ret.begin();
    for (auto s : ls) {
      it = std::copy(s.begin(), s.end(), it);
    }
    return ret;
  }
  /*
    For numbers > 999 we build the string in blocks of 1000, we can't
    store everything in a single vector since we build the string from
    smallest to largest.
  */
  int pow3 = 0;
  size_t sz = 0;
  bool minus = (number < 0);
  std::vector<std::vector<std::string_view>> ls;
  if (minus) {
    number = abs(number);
    sz += sv_minus.size();
  }
  while (number) {
    long group = number % 1000;
    number /= 1000;
    //it's quite possible for group to be 0 for parts of large numbers
    if (group) {
      ls.resize(ls.size() + 1);
      auto &v = ls.back();
      v.reserve(8);//almost always enough space
       if (number) {
        v.push_back(sv_thousands_sep);
        sz += sv_thousands_sep.size();
      }
      sz += format_small_cardinal(group, v);
      //Can't happen with a 64 bit long, but just in case we bail,
      //there is a way to do this, but it seems a little silly
      //to put it in unless we need to support bignums.
      if(pow3 >= cardinal_thousand_block_list.size()){
        return "";
      }
      v.push_back(cardinal_thousand_block_list[pow3]);
      sz += v.back().size();
    }
    pow3++;
  }
  ret.resize(sz);
  auto it = ret.begin();
  if (minus) {
    it = std::copy(std::begin(sv_minus), std::end(sv_minus), it);
  }
  auto ls_it = ls.rbegin();
  auto ls_end = ls.rend();
  while (ls_it != ls_end) {
    for (auto s : (*ls_it)) {
      it = std::copy(s.begin(), s.end(), it);
    }
    ++ls_it;
  }
  return ret;
}
/*
  Totally unoptimized, uses naieve string concatenation a bunch.
*/
const std::string format_ordinal_number (long number) {
  if (number == 0) { return "zeroth"; }
  bool minus = (number < 0);
  number = abs(number);
  if (number < 20) {
    return (minus ? "minus " : "") + std::string(ordinal_ones_list[number]);
  }
  int last_hundred = number % 100;
  std::string cardinal = format_cardinal_number((minus ? -1 : 1) *
                                                (number - last_hundred));
  if (last_hundred == 0) {
    return cardinal + "th"s;
  } else if (last_hundred < 20) {
    return cardinal + " "s + std::string(ordinal_ones_list[last_hundred]);
  } else {
    int tens = last_hundred / 10;
    int ones = last_hundred % 10;
    if (ones == 0) {
      return cardinal + " "s + std::string(ordinal_tens_list[tens]);
    } else {
      return (cardinal + " "s + std::string(cardinal_tens_list[tens]) + "-"s +
              std::string(ordinal_ones_list[ones]));
    }
  }
}
#endif
bool strtol_checked(long *value, const char *str,
                    const char** endptr = nullptr, int base = 0){
  errno = 0;
  const char *tmp;
  if(!endptr){ endptr = &tmp; }
  *value = strtol(str, endptr, base);
  return (errno != 0 || (*endptr == str));
}
bool strtoul_checked(long *value, const char *str,
                    const char** endptr = nullptr, int base = 0){
  errno = 0;
  const char *tmp;
  if(!endptr){ endptr = &tmp; }
  *value = strtoul(str, endptr, base);
  return (errno != 0 || (*endptr == str));
}
bool strtod_checked(long *value, const char *str,
                    const char** endptr = nullptr, int base = 0){
  errno = 0;
  const char *tmp;
  if(!endptr){ endptr = &tmp; }
  *value = strtod(str, endptr, base);
  return (errno != 0 || (*endptr == str));
}
extern "C" {
size_t memspn_table(const uint8_t *str, size_t len,
                    const uint8_t accept[256]){
  unsigned int i=0;
  //this is for speed, but I'm not sure how much it's worth it
  while(i+4 <= len){
    if(!accept[str[i]]){return i;}
    if(!accept[str[i+1]]){return i+1;}
    if(!accept[str[i+2]]){return i+2;}
    if(!accept[str[i+3]]){return i+3;}
    i+=4;
  }
  switch(len-i){
    case 3: if(!accept[str[i]]){return i;} i++;
    case 2: if(!accept[str[i]]){return i;} i++;
    case 1: if(!accept[str[i]]){return i;}
  }
  return len;
}
size_t memcspn_table(const uint8_t *str, size_t len,
                     const uint8_t reject[256]){
  unsigned int i=0;
  while(i+4 <= len){
    if(reject[str[i]]){return i;}
    if(reject[str[i+1]]){return i+1;}
    if(reject[str[i+2]]){return i+2;}
    if(reject[str[i+3]]){return i+3;}
    i+=4;
  }
  switch(len-i){
    case 3: if(reject[str[i]]){return i;} i++;
    case 2: if(reject[str[i]]){return i;} i++;
    case 1: if(reject[str[i]]){return i;}
  }
  return len;
}
size_t memspn(const uint8_t *buf, size_t len,
              const uint8_t *accept, size_t len2){
  uint8_t bytes[256] = {0};
  unsigned int i;
  for(i = 0; i < len2; i++){
    bytes[accept[i]] = 1;
  }
  return memspn_table(buf, len, bytes);
}
size_t memcspn(const uint8_t *buf, size_t len,
               const uint8_t *reject, size_t len2){
  uint8_t bytes[256] = {0};
  unsigned int i;
  for(i = 0; i < len2; i++){
    bytes[reject[i]] = 1;
  }
  return memcspn_table(buf, len, bytes);
}
}
#if 0
static thread_local char to_chars_static_buf[64];

char* binary_int_to_string(uint64_t x, int bitdepth, char *buf){
  int i;
  char *ret = buf;
  for(i = bitdepth-3; i > 3; i -= 4){
    int nibble = ((x & (0xFul << i))>>i);
    memcpy(buf, nibble_strings[nibble],4);
    buf += 4;
  }
  switch(i){//Fallthrough is deliberate
    case 3:
      *buf++ = (x & 4 ? '1' : '0');
    case 2:
      *buf++ = (x & 2 ? '1' : '0');
    case 1:
      *buf++ = (x & 1 ? '1' : '0');
    case 0:
      *buf = '\0';
  }
  return ret;
}
#define is_binary_digit(c) (c == '0' || c == '1')
#define is_oct_digit(c) (c >= '0' && c <= '8')
static const uint8_t digits_uc[] =
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
static const uint8_t digits_lc[] =
  "0123456789abcdefghijklmnopqrstuvwxyz";
//max_length_per_base[n] is the length of the longest string in base n
//than can possibly fit in a 64 bit integer
//the formula for m bits in base n is just ceil(m/lg(n))
static int max_length_per_base[35] =
  {64, 41, 32, 28, 25, 23, 22, 21, 20, 19, 18, 18,
   17, 17, 16, 16, 16, 16, 15, 15, 15, 15, 14, 14,
   14, 14 ,14, 14, 14, 13, 13, 13, 13, 13, 13};
static const char *nibble_strings[16] =
  {"0000","0001","0010","0011","0100","0101","0111","1000",
   "0111","1000","1001","1010","1011","1100","1101","1111"};
char* int_to_string(char *start, char *end,
                    unsigned long value, int base){
  if(value == 0){
    *(--end) = '0';
    return end;
  }
  unsigned long rem;
  switch(base){
    //Do base 2 numbers 4 bits at a time.
    case 2:{
      while(value >= 0xf && (end -= 4) >= start){
        memcpy(end, nibble_strings[value & 0xf], 4);
        value >>= 4;
      }
      while(value && (--end) >= start){
        *end = (value & 1) + '0';
        value >>= 1;
      }
      return (end >= start ? end : NULL);
    }
    case 8:{
      while(value && (--end) >= start){
        *end = (value & 0x7) + '0';
        value >>= 3;
      }
      return (end >= start ? end : NULL);
    }
    //we can't do base 10 by bitshifts, but by marking it explicitly we
    //can convert the division into a multiplication. This makes a
    //significant difference (I tested it)
    case 10:{
      while(value && (--end) >= start){
        rem = value % 10;
        value /= 10;
        *end = rem + '0';
      }
      return (end >= start ? end : NULL);
    }
    case 16:{
      while(value && (--end) >= start){
        char digit = digits_lc[value & 0xf];
        value >>= 4;
        *end = digit;
      }
      return (end >= start ? end : NULL);
    }
    default:
      while(value && (--end) >= start){
        char digit = digits_lc[value % base];
        value /= base;
        *end = digit;
      }
      return (end >= start ? end : NULL);
  }
}
char* int_to_string(char *start, char *end,
                    long value, int base){
  unsigned long uvalue;
  bool negitive = (value < 0);
  if(negitive){
    uvalue = ~((unsigned long)value) + 1;
  } else {
    uvalue = value;
  }
  char *ptr = int_to_string(start, end, uvalue, base);
  if(ptr && negitive){
    if(ptr == start){
      return NULL;
    } else {
      *(--ptr) = '-';
      return ptr;
    }
  } else {
    return ptr;
  }
}



template <typename T>
char *to_chars_static(T val){
  snprintf(to_chars_static_buf, sizeof(to_chars_static_buf),
           printf_spec<T>, val);
  return to_chars_static_buf;
}
template char *to_chars_static(int val);
template char *to_chars_static(unsigned int val);
template char *to_chars_static(long val);
template char *to_chars_static(unsigned long val);
template char *to_chars_static(double val);
template char *to_chars_static(float val);
#endif
