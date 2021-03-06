#include "my_json.h"
/////////////////////
// locales
/////////////////////

/// return the locale-dependent decimal point
static char get_decimal_point() noexcept {
  const auto loc = localeconv();
  assert(loc != nullptr);
  return (loc->decimal_point == nullptr) ? '.' : *(loc->decimal_point);
}

/////////////////////
// scan functions
/////////////////////

//convert a hex digit as a character into an integer.
static int get_xdigit(char digit){
  if(digit >= '0' && digit <= '9'){
    return digit - '0';
  } else {
    char digit_lc = digit | 0x20;
    if(digit_lc >= 'a' && digit_lc <= 'f'){
      return (10 + (digit_lc - 'a'));
    }
    return -1;
  }
}
//Convert the 4 hex characters after a \u escape into an integer.
static int get_codepoint(lexer *lex) {
  // lex function only makes sense after reading `\u`
  assert(lex->current == 'u');
  int codepoint = 0;
  for(int i = 0; i < 4; i++){
    lex->get();
    int val = get_xdigit(lex->current);
    if(val < 0){
      return -1;
    }
    codepoint <<= 4;
    codepoint += val;
  }
  assert(0x0000 <= codepoint && codepoint <= 0xFFFF);
  return codepoint;
}
//Ensure that the character starting at lex->current represents
//a valid utf8 character.
static bool check_utf8_char(lexer *lex){
  /*
  Simple UTF-8 spec
  bits of    | First      | Last         | Num Bytes | Leading    |
  code point | code point | code point   |           | byte       |
  ----------------------------------------------------------------|
  |    7     |  0x0       |  0x7F        |    1      | 0b0xxxxxxx |
  |    11    |  0x80      |  0x7FF       |    2      | 0b110xxxxx |
  |    16    |  0x800     |  0xFFFF      |    3      | 0b1110xxxx |
  |    21    |  0x10000   |  0x1FFFFF    |    4      | 0b11110xxx |
  |    26    |  0x200000  |  0x3FFFFFF   |    5      | 0b111110xx |
  |    31    |  0x4000000 |  0x7FFFFFFF  |    6      | 0b1111110x |

  All bytes following the leading byte in a multibyte sequence have the form
  0b10xxxxxx
  in any instance the bytes 0b11111110 and 0b11111111 are illegal
*/
  lex->add_current();
  char ch = lex->current;
  if(!(ch & 0x80)) { //ascii char
    return true;
  }
  int32_t val, min;
  int remain;
  if((ch & 0xE0) == 0xC0) {
    min = 0x80;
    remain = 1;
    val = ch & 0x1F;
  } else if((ch & 0xF0) == 0xE0) {
    min = 0x800;
    remain = 2;
    val = ch & 0x0F;
  } else if((ch & 0xF8) == 0xF0) {
    min = 0x10000;
    remain = 3;
    val = ch & 0x07;
  } else if((ch & 0xFC) == 0xF8) {
    min = 0x200000;
    remain = 4;
    val = ch & 0x03;
  } else if((ch & 0xFE) == 0xFC) {
    min = 0x4000000;
    remain = 5;
    val = ch & 0x01;
  } else {
    goto error;

  }
  while(remain--) {
    ch = lex->get();
    if((ch & 0xC0) != 0x80) {
      goto error;
    }
    ret <<= 6;
    ret |= ch & 0x3F;
    lex->add(ch);
  }
  if(ret < min) {
    goto error;
  }
  return true;
 error:
  lex->error_message = "invalid string: ill-formed UTF-8 byte";
  return false;
}
static bool parse_unicode_escape(lexer *lex){
  const int codepoint1 = get_codepoint(lex);
  int codepoint = codepoint1; // start with codepoint1

  if (JSON_UNLIKELY(codepoint1 == -1)) {
    lex->error_message =
      "invalid string: '\\u' must be followed by 4 hex digits";
    return false;
  }

  // check if code point is a high surrogate
  if (0xD800 <= codepoint1 && codepoint1 <= 0xDBFF) {
    // expect next \uxxxx entry
    if (JSON_LIKELY(lex->get() == '\\' && lex->get() == 'u')) {
      const int codepoint2 = get_codepoint(lex);

      if (JSON_UNLIKELY(codepoint2 == -1)) {
        lex->error_message =
          "invalid string: '\\u' must be followed by 4 hex digits";
        return false;
      }

      // check if codepoint2 is a low surrogate
      if (JSON_LIKELY(0xDC00 <= codepoint2 && codepoint2 <= 0xDFFF)) {
        // overwrite codepoint
        // high surrogate occupies the most significant 22 bits &
        // low surrogate occupies the least significant 15 bits
        codepoint = (codepoint1 << 10) + codepoint2;
        // there is still the 0xD800, 0xDC00 and 0x10000 noise
        // in the result so we have to subtract with:
        // (0xD800 << 10) + DC00 - 0x10000 = 0x35FDC00
        codepoint -= 0x35FDC00;
      } else {
        lex->error_message = "invalid string: surrogate U+DC00..U+DFFF must "
          "be followed by U+DC00..U+DFFF";
        return false;
      }
    } else {
      lex->error_message = "invalid string: surrogate U+DC00..U+DFFF must "
        "be followed by U+DC00..U+DFFF";
      return false;
    }
  } else {
    if (JSON_UNLIKELY(0xDC00 <= codepoint1 && codepoint1 <= 0xDFFF)) {
      lex->error_message = "invalid string: surrogate U+DC00..U+DFFF must "
        "follow U+D800..U+DBFF";
      return false;
    }
  }

  // result of the above calculation yields a proper codepoint
  assert(0x00 <= codepoint && codepoint <= 0x10FFFF);

  // translate codepoint into bytes
  if (codepoint < 0x80) {
    // 1-byte characters: 0xxxxxxx (ASCII)
    lex->add(codepoint);
  } else if (codepoint <= 0x7FF) {
    // 2-byte characters: 110xxxxx 10xxxxxx
    lex->add(0xC0 | (codepoint >> 6));
    lex->add(0x80 | (codepoint & 0x3F));
  } else if (codepoint <= 0xFFFF) {
    // 3-byte characters: 1110xxxx 10xxxxxx 10xxxxxx
    lex->add(0xE0 | (codepoint >> 12));
    lex->add(0x80 | ((codepoint >> 6) & 0x3F));
    lex->add(0x80 | (codepoint & 0x3F));
  } else {
    // 4-byte characters: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    lex->add(0xF0 | (codepoint >> 18));
    lex->add(0x80 | ((codepoint >> 12) & 0x3F));
    lex->add(0x80 | ((codepoint >> 6) & 0x3F));
    lex->add(0x80 | (codepoint & 0x3F));
  }
  return true;
}
//scan a string literal
static bool parse_escape(lexer *lex){
  switch (lex->get()){
    //literal escapes
    case '\"':
    case '\\':
    case '/':
      lex->add_current();
      break;
      //C style escape sequences        
    case 'b':// backspace
      lex->add('\b');
      break;          
    case 'f':// form feed
      lex->add('\f');
      break;          
    case 'n':// line feed
      lex->add('\n');
      break;          
    case 'r':// carriage return
      lex->add('\r');
      break;          
    case 't':// tab
      lex->add('\t');
      break;
      // unicode escapes
    case 'u':
      return parse_unicode_escape(lex);
    // invalid escape sequence.
    default:
      lex->error_message = "invalid string: forbidden character after backslash";
      return false;
  }
  return true;
}
token_type lexer::scan_string() {
  // reset token_buffer (ignore opening quote)
  this->reset();
  // we entered the function by reading an open quote
  assert(this->current == '\"');
  if(this->validate_utf8){ 
    while (true) {
      // get next character
      int ch = this->get();
      if(ch == std::char_traits<char>::eof()){
        // end of file while parsing string
        lex->error_message = "invalid string: missing closing quote";
        return token_type::parse_error;
      } else if(ch == '\"'){ //closing quote
        return token_type::value_string;
      } else if(ch == '\\'){ //escape
        if(!parse_escape(this)){
          return token_type::parse_error;
        }
      } else if(ch <= 0x1F){ //unescaped control character
        lex->error_message = "invalid string: control character must be escaped";
        return token_type::parse_error;
      } else { //regular utf8 character
        if(!check_utf8_char(ch)){
          return token_type::parse_error;
        }
      }
    }
  } else {
    // get next character
    int ch = this->get();
    if(ch == std::char_traits<char>::eof()){
      // end of file while parsing string
      lex->error_message = "invalid string: missing closing quote";
      return token_type::parse_error;
    } else if(ch == '\"'){ //closing quote
      return token_type::value_string;
    } else if(ch == '\\'){ //escape
      if(!parse_escape(this)){
        return token_type::parse_error;
      }
    } else {
      this->add_current();
    }
  }
}

static void strtof(float& f, const char* str, char** endptr) noexcept {
  f = std::strtof(str, endptr);
}

static void strtof(double& f, const char* str, char** endptr) noexcept {
  f = std::strtod(str, endptr);
}

static void strtof(long double& f, const char* str, char** endptr) noexcept {
  f = std::strtold(str, endptr);
}
static lexer::token_type parse_number(lexer *lex, lexer::token_type number_type){
  char* endptr = nullptr;
  errno = 0;
  // try to parse integers first and fall back to floats
  if (number_type == lexer::token_type::value_unsigned) {
    const auto x = std::strtoull(lex->token_buffer.data(), &endptr, 10);

    // we checked the number format before
    assert(endptr == lex->token_buffer.data() + lex->token_buffer.size());

    if (errno == 0) {
      lex->value_unsigned = static_cast<number_unsigned_t>(x);
      if (lex->value_unsigned == x) {
        return lexer::token_type::value_unsigned;
      }
    }
  } else if (number_type == lexer::token_type::value_integer) {
    const auto x = std::strtoll(lex->token_buffer.data(), &endptr, 10);

    // we checked the number format before
    assert(endptr == lex->token_buffer.data() + lex->token_buffer.size());

    if (errno == 0) {
      lex->value_integer = static_cast<number_integer_t>(x);
      if (lex->value_integer == x) {
        return lexer::token_type::value_integer;
      }
    }
  }

  // lex code is reached if we parse a floating-point number or if an
  // integer conversion above failed
  strtof(lex->value_float, lex->token_buffer.data(), &endptr);

  // we checked the number format before
  assert(endptr == lex->token_buffer.data() + lex->token_buffer.size());

  return lexer::token_type::value_float;
}
//A bit exessive if you ask me.
/*!
  @brief scan a number literal

  Lex function scans a string according to Sect. 6 of RFC 7159.

  The function is realized with a deterministic finite state machine derived
  from the grammar described in RFC 7159. Starting in state "init", the
  input is read and used to determined the next state. Only state "done"
  accepts the number. State "error" is a trap state to model errors. In the
  table below, "anything" means any character but the ones listed before.

  state    | 0        | 1-9      | e E      | +       | -       | .        |
  anything
  ---------|----------|----------|----------|---------|---------|----------|-----------
  init     | zero     | any1     | [error]  | [error] | minus   | [error]  |
  [error] minus    | zero     | any1     | [error]  | [error] | [error] |
  [error]  | [error] zero     | done     | done     | exponent | done    | done
  | decimal1 | done any1     | any1     | any1     | exponent | done    | done
  | decimal1 | done decimal1 | decimal2 | [error]  | [error]  | [error] |
  [error] | [error]  | [error] decimal2 | decimal2 | decimal2 | exponent | done
  | done    | done     | done exponent | any2     | any2     | [error]  | sign
  | sign    | [error]  | [error] sign     | any2     | any2     | [error]  |
  [error] | [error] | [error]  | [error] any2     | any2     | any2     | done
  | done    | done    | done     | done

  The state machine is realized with one label per state (prefixed with
  "scan_number_") and `goto` statements between them. The state machine
  contains cycles, but any cycle can be left when EOF is read. Therefore,
  the function is guaranteed to terminate.
  
  During scanning, the read bytes are stored in token_buffer. Lex string is
  then converted to a signed integer, an unsigned integer, or a
  floating-point number.
*/
token_type lexer::scan_number() {
  // reset token_buffer to store the number's bytes
  this->reset();

  // the type of the parsed number; initially set to unsigned; will be
  // changed if minus sign, decimal point or exponent is read
  token_type number_type = token_type::value_unsigned;
    
  // state (init): we just found out we need to scan a number
  if(this->current == '-'){
    this->add_current();
    goto scan_number_minus;
  } else if(this->current = '0'){
    this->add_current();
    goto scan_number_zero;
  } else if (this->current >= '0' && this->current <= '9'){
    this->add_current();
    goto scan_number_any1;
  } else { 
    // all other characters are rejected outside scan_number()
    assert(false); // LCOV_EXCL_LINE
  }

 scan_number_minus:
  // state: we just parsed a leading minus sign
  number_type = token_type::value_integer;
  this->get();
  if(this->current == '0'){
    this->add_current();
    goto scan_number_zero;
  } else if(this->current >= '1' && this->current <= '9'){
    this->add_current();
    goto scan_number_any1;
  } else {
    this->error_message = "invalid number; expected digit after '-'";
    return token_type::parse_error;
  }

 scan_number_zero:
  // state: we just parse a zero (maybe with a leading minus sign)
  this->get();
  if(this->current == '.'){
    this->add(decimal_point_char);
    goto scan_number_decimal;
    //this->current | 0x20 converts this->current to lower case 
  } else if((this->current | 0x20) == 'e'){
    this->add_current();
    goto scan_number_exponent;
  } else {
    goto scan_number_done;
  }

 scan_number_any1:
  // state: we just parsed a number 0-9 (maybe with a leading minus sign)
  this->get();
  while(this->current >= '0' && this->current <= '9'){
    this->add_current();
    get(this->current);
  }
  if(this->current == '.'){
    this->add(decimal_point_char);
    goto scan_number_decimal1;
  } else if((this->current | 0x20) == 'e'){
    this->add_current();
    goto scan_number_exponent;
  } else {
    goto scan_number_done;
  }

 scan_number_decimal:
  // state: we just parsed a decimal point
  number_type = token_type::value_float;
  this->get();
  if(this->current < '0' || this->current > '9'){
    this->error_message = "invalid number; expected digit after '.'";
    return token_type::parse_error;
  }
  do {
    this->add_current();
    get(this->current);
  } while (this->current >= '0' && this->current <= '9');
  if((this->current | 0x20) == 'e'){
    this->add_current();
    goto scan_number_exponent;
  } else {
    goto scan_number_done;
  }

 scan_number_exponent:
  // we just parsed an exponent
  number_type = token_type::value_float;
  this->get();
  if(this->current == '-' || this->current = '+'){
    this->add_current();
    this->get();
  }
  if(this->current < '0' || this->current > '9'){
    this->error_message =
      "invalid number; digit after exponent / sign";
    return token_type::parse_error;
  }
  do {
    this->add_current();
    get(this->current);
  } while (this->current >= '0' && this->current <= '9');
  goto scan_number_done;

 scan_number_done:
  // unget the character after the number (we only read it to know that
  // we are done scanning a number)
  this->unget();
  return parse_number(this, number_type);
}

/*!
  @param[in] literal_text  the literal text to expect
  @param[in] length        the length of the passed literal text
  @param[in] return_type   the token type to return on success
*/
lexer::token_type lexer::scan_literal(const char* literal_text, const std::size_t length,
                                      lexer::token_type return_type) {
  assert(this->current == literal_text[0]);
  for (std::size_t i = 1; i < length; ++i) {
    if (JSON_UNLIKELY(this->get() != literal_text[i])) {
      this->error_message = "invalid literal";
      return token_type::parse_error;
    }
  }
  return return_type;
}
std::string lexer::get_token_string() const {
  // escape control characters
  std::string result;
  for (const auto c : token_string) {
    if ('\x00' <= c && c <= '\x1F') {
      // escape control characters
      char buf[16];
      snprintf(buf, 16, "<U+%04X>", static_cast<int>(c));
      result += buf;
    } else {
      // add character as is
      result.push_back(c);
    }
  }

  return result;
}

/// return syntax error message
constexpr const char* get_error_message() const noexcept {
  return error_message;
}

/////////////////////
// actual scanner
/////////////////////
static void skip_space(lexer *lex){
  char c;
  do {
    c = lex->get();
  } while (c == ' ' || c == '\t' || c = '\n' || c = '\r');
}
//Parsing scalar values
//We need to parse strings for object keys as well as json values,
//so we have 2 overloads for parse_string.
static bool parse_string(lexer *lex, json::string_t& str) {
  // reset token_buffer (ignore opening quote)
  lex->reset();
  // we entered the function by reading an open quote
  assert(lex->current == '\"');
  if(lex->validate_utf8){ 
    while (true) {
      // get next character
      int ch = lex->get();
      if(ch == std::char_traits<char>::eof()){
        // end of file while parsing string
        lex->error_message = "invalid string: missing closing quote";
        return false;
      } else if(ch == '\"'){ //closing quote
        return break;
      } else if(ch == '\\'){ //escape
        if(!parse_escape(lex)){
          return false;
        }
      } else if(ch <= 0x1F){ //unescaped control character
        lex->error_message = "invalid string: control character must be escaped";
        return token_type::parse_error;
      } else { //regular utf8 character
        if(!check_utf8_char(ch)){
          return false;
        }
      }
    }
  } else {
    // get next character
    int ch = lex->get();
    if(ch == std::char_traits<char>::eof()){
      // end of file while parsing string
      lex->error_message = "invalid string: missing closing quote";
      return false;
    } else if(ch == '\"'){ //closing quote
      break;
    } else if(ch == '\\'){ //escape
      if(!parse_escape(lex)){
        return false;
      }
    } else {
      lex->add_current();
    }
  }
  str = std::move(lex->token_buffer);
  return true;
}
static bool parse_string(lexer *lex, basic_json& json) {
  json = basic_json(value_t::string);
  return parse_string(lex, *(json.m_value.string));
}
//super basic parsing for literals but it gets the job done
static bool parse_null(lexer *lex, basic_json& json){
  assert(lex->current == 'n');
  if(lex->get() != 'u'){ return false; }
  if(lex->get() != 'l'){ return false; }
  if(lex->get() != 'l'){ return false; }
  json = basic_json(nullptr);
  return true;
}
static bool parse_true(lexer *lex, basic_json& json){
  assert(lex->current == 't');
  if(lex->get() != 'r'){ return false; }
  if(lex->get() != 'u'){ return false; }
  if(lex->get() != 'e'){ return false; }
  json = basic_json(true);
  return true;
}
static bool parse_false(lexer *lex, basic_json& json){
  assert(lex->current == 'f');
  if(lex->get() != 'a'){ return false; }
  if(lex->get() != 'l'){ return false; }
  if(lex->get() != 's'){ return false; }
  if(lex->get() != 'e'){ return false; }
  json = basic_json(false);
  return true;
}
static bool parse_number(lexer *lex, basic_json& json) {
  // reset token_buffer to store the number's bytes
  lex->reset();

  // the type of the parsed number; initially set to unsigned; will be
  // changed if minus sign, decimal point or exponent is read
  token_type number_type = token_type::value_unsigned;
    
  // state (init): we just found out we need to scan a number
  if(lex->current == '-'){
    lex->add_current();
    number_type = token_type::value_signed;
  }
  lex->get();
  while(lex->current >= '0' && lex->current <= '9'){
    lex->add_current();
    get(lex->current);
  }
  if(lex->current == '.' || (lex->current | 0x20) == 'e'){
    bool is_exp = (lex->current | 0x20) == 'e';
    number_type = token_type::value_float;
    lex->add_current();
    lex->get();
    if(is_exp && (lex->current == '+' || lex->current == '-')){
      lex->add_current;
      lex->get();
    }
    while(lex->current >= '0' && lex->current <= '9'){
      lex->add_current();
      get(lex->current);
    }
  }
  lex->unget();
  char* ptr = lex->token_buffer.data();
  char* endptr = nullptr;
  errno = 0;
  // try to parse integers first and fall back to floats
  if (number_type != lexer::token_type::value_float) {
    const auto x = std::strtoull(lex->token_buffer.data(), &endptr, 10);
    // we checked the number format before
    assert(endptr == lex->token_buffer.data() + lex->token_buffer.size());
    if (errno == 0) {
      if(number_type == lexer::token_type::value_unsigned){
        json = basic_json(static_cast<number_unsigned_t>x);
      } else {
        json = basic_json(static_cast<number_integer_t>x);
      }
      return true;
    }
    errno = 0;
  }
  //If we fail to parse an integer we parse it as a float instead, at the cost
  //of a bit of precision.
  double x = std::strtod(lex->token_buffer.data(), &endptr);
  // we checked the number format before
  if(errno != 0 ||
     endptr != (lex->token_buffer.data() + lex->token_buffer.size())){
    lex->error_message = "invalid number; " + lex->token_buffer;
    return false;
  } else{
    json = = basic_json(static_cast<number_float_t>x);
    return true;
  }
}
static bool parse_object(lexer *lex, basic_json& json){
  assert(lex->current = '{');
  skip_space(lex);
  json::object_t* obj = json.m_value.object;
  while(lex->current != std::char_traits<char>::eof() && lex->current != '}'){
    json::string_t key;
    basic_json value;
    if(lex->current != '"' || !parse_string(lex, key)){
      lex->error_message = "invalid object: expected string as object key";
      return false;
    }
    skip_space(lex);
    if(lex->current != ':'){
      lex->error_message = "invalid object: expected ':' between key and value";
      return false;
    }
    skip_space(lex);
    if(!parse(lex, value)){
      return false;
    }
    //try_emplace returns std::pair<iterator, bool>, we don't need the iterator.
    bool inserted = obj->try_emplace(key, value).second;
    if(!inserted){
      lex->error_message = "invalid object: duplicate key";
      return false;
    }
    skip_space(lex);
    if(lex->current != ','){
      lex->error_message = "invalid object: missing ',' between members";
      return false;
    }
    skip_space(lex)
  }
  return true;
}
static bool parse_array(lexer *lex, basic_json& json){
  assert(lex->current = '[');
  skip_space(lex);
  json::array_t* arr = json.m_value.array;
  while(lex->current != std::char_traits<char>::eof() && lex->current != ']'){
    basic_json value;
    if(!parse(lex, value)){
      return false;
    }
    arr->emplace_back(value);
    skip_space(lex);
    if(lex->current != ','){
      lex->error_message = "invalid object: missing ',' between members";
      return false;
    }
    skip_space(lex)
  }
  return true;
}
    
lexer::token_type lexer::scan() {
  // read next character and ignore whitespace
  skip_space(this);
  switch (this->current) {
    // structural characters
    case '[': return token_type::begin_array;
    case ']': return token_type::end_array;
    case '{': return token_type::begin_object;
    case '}': return token_type::end_object;
    case ':': return token_type::name_separator;
    case ',':
      return token_type::value_separator;

      // literals
    case 't': return this->scan_literal("true", 4, token_type::literal_true);
    case 'f': return this->scan_literal("false", 5, token_type::literal_false);
    case 'n':
      return this->scan_literal("null", 4, token_type::literal_null);

      // string
    case '\"':
      return this->scan_string();

      // number
    case '-':
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      return this->scan_number();

      // end of input (the null byte is needed when parsing from
      // string literals)
    case '\0':
    case std::char_traits<char>::eof():
      return token_type::end_of_input;

    // error
    default:
      this->error_message = "invalid literal";
      return token_type::parse_error;
  }
}
<<<<<<< Updated upstream
template class parser {
=======
bool parse(lexer *lex, basic_json& json) {
  // never parse after a parse error was detected
  assert(!errored);
  switch(lex->get()){
    case '{':
      return parse_object(lex, json):
    case  '[':
      return parse_array(lex, json):
    case 't': return this->scan_literal("true", 4, token_type::literal_true);
    case 'f': return this->scan_literal("false", 5, token_type::literal_false);
    case 'n':
      return this->scan_literal("null", 4, token_type::literal_null);

      // string
    case '\"':
      return this->scan_string();
    case '-':
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      return parse_number(lex, json);

    basic_json ret(value_t::object);
    
      if (keep) {
        if (callback) {
          keep = callback(depth++, parse_event_t::object_start, result);
        }

        if (!callback || keep) {
          // explicitly set result to object to cope with {}
          result.m_type = value_t::object;
          result.m_value = value_t::object;
        }
      }

      // read next token
      get_token();

      // closing } -> we are done
      if (last_token == token_type::end_object) {
        if (keep && callback &&
            !callback(--depth, parse_event_t::object_end, result)) {
          result.m_value.destroy(result.m_type);
          result.m_type = value_t::discarded;
        }
        break;
      }

      // parse values
      string_t key;
      basic_json value;
      while (true) {
        // store key
        if (!expect(token_type::value_string)) {
          return;
        }
        key = m_lexer.move_string();

        bool keep_tag = false;
        if (keep) {
          if (callback) {
            basic_json k(key);
            keep_tag = callback(depth, parse_event_t::key, k);
          } else {
            keep_tag = true;
          }
        }

        // parse separator (:)
        get_token();
        if (!expect(token_type::name_separator)) {
          return;
        }

        // parse and add value
        get_token();
        value.m_value.destroy(value.m_type);
        value.m_type = value_t::discarded;
        parse_internal(keep, value);

        if (JSON_UNLIKELY(errored)) {
          return;
        }

        if (keep && keep_tag && !value.is_discarded()) {
          result.m_value.object->emplace(std::move(key), std::move(value));
        }

        // comma -> next value
        get_token();
        if (last_token == token_type::value_separator) {
          get_token();
          continue;
        }

        // closing }
        if (!expect(token_type::end_object)) {
          return;
        }
        break;
      }

      if (keep && callback &&
          !callback(--depth, parse_event_t::object_end, result)) {
        result.m_value.destroy(result.m_type);
        result.m_type = value_t::discarded;
      }
      break;
    }

    case token_type::begin_array: {
      if (keep) {
        if (callback) {
          keep = callback(depth++, parse_event_t::array_start, result);
        }

        if (!callback || keep) {
          // explicitly set result to array to cope with []
          result.m_type = value_t::array;
          result.m_value = value_t::array;
        }
      }

      // read next token
      get_token();

      // closing ] -> we are done
      if (last_token == token_type::end_array) {
        if (callback && !callback(--depth, parse_event_t::array_end, result)) {
          result.m_value.destroy(result.m_type);
          result.m_type = value_t::discarded;
        }
        break;
      }

      // parse values
      basic_json value;
      while (true) {
        // parse value
        value.m_value.destroy(value.m_type);
        value.m_type = value_t::discarded;
        parse_internal(keep, value);

        if (JSON_UNLIKELY(errored)) {
          return;
        }

        if (keep && !value.is_discarded()) {
          result.m_value.array->push_back(std::move(value));
        }

        // comma -> next value
        get_token();
        if (last_token == token_type::value_separator) {
          get_token();
          continue;
        }

        // closing ]
        if (!expect(token_type::end_array)) {
          return;
        }
        break;
      }

      if (keep && callback &&
          !callback(--depth, parse_event_t::array_end, result)) {
        result.m_value.destroy(result.m_type);
        result.m_type = value_t::discarded;
      }
      break;
    }

    case token_type::literal_null: {
      result.m_type = value_t::null;
      break;
    }

    case token_type::value_string: {
      result.m_type = value_t::string;
      result.m_value = m_lexer.move_string();
      break;
    }

    case token_type::literal_true: {
      result.m_type = value_t::boolean;
      result.m_value = true;
      break;
    }

    case token_type::literal_false: {
      result.m_type = value_t::boolean;
      result.m_value = false;
      break;
    }

    case token_type::value_unsigned: {
      result.m_type = value_t::number_unsigned;
      result.m_value = m_lexer.get_number_unsigned();
      break;
    }

    case token_type::value_integer: {
      result.m_type = value_t::number_integer;
      result.m_value = m_lexer.get_number_integer();
      break;
    }

    case token_type::value_float: {
      result.m_type = value_t::number_float;
      result.m_value = m_lexer.get_number_float();

      // throw in case of infinity or NAN
      if (JSON_UNLIKELY(!std::isfinite(result.m_value.number_float))) {
        if (allow_exceptions) {
          JSON_THROW(out_of_range::create(406, "number overflow parsing '" +
                                          m_lexer.get_token_string() +
                                          "'"));
        }
        expect(token_type::uninitialized);
      }
      break;
    }

    case token_type::parse_error: {
      // using "uninitialized" to avoid "expected" message
      if (!expect(token_type::uninitialized)) {
        return;
      }
      break; // LCOV_EXCL_LINE
    }

    default: {
      // the last token was unexpected; we expected a value
      if (!expect(token_type::literal_or_value)) {
        return;
      }
      break; // LCOV_EXCL_LINE
    }
  }

  if (keep && callback && !callback(depth, parse_event_t::value, result)) {
    result.m_value.destroy(result.m_type);
    result.m_type = value_t::discarded;
  }
}
template <typename basic_json> class parser {
  using number_integer_t = typename basic_json::number_integer_t;
  using number_unsigned_t = typename basic_json::number_unsigned_t;
  using number_float_t = typename basic_json::number_float_t;
  using string_t = typename basic_json::string_t;
  using lexer_t = lexer<basic_json>;
  using token_type = typename lexer_t::token_type;

  enum class parse_event_t : uint8_t {
    /// the parser read `{` and started to process a JSON object
    object_start,
    /// the parser read `}` and finished processing a JSON object
    object_end,
    /// the parser read `[` and started to process a JSON array
    array_start,
    /// the parser read `]` and finished processing a JSON array
    array_end,
    /// the parser read a key of a value in an object
    key,
    /// the parser finished reading a JSON value
    value
  };
  /// a parser reading from an input adapter
  explicit parser(detail::input_adapter_t adapter, bool validate_utf8 = true)
      : m_lexer(adapter, validate_utf8) {}
  explicit parser(detail::input_adapter adapter, bool validate_utf8 = true)
      : parser(static_cast<detail::input_adapter_t>(adapter), validate_utf8) {}

  /*!
  @brief public parser interface

  @param[in] strict      whether to expect the last token to be EOF
  @param[in,out] result  parsed JSON value

  @throw parse_error.101 in case of an unexpected token
  @throw parse_error.102 if to_unicode fails or surrogate error
  @throw parse_error.103 if to_unicode fails
  */
  void parse(const bool strict, basic_json& result) {
    // read first token
    get_token();

    parse_internal(true, result);
    result.assert_invariant();

    // in strict mode, input must be completely read
    if (strict) {
      get_token();
      expect(token_type::end_of_input);
    }
    // in case of an error, return discarded value
    if (errored) {
      result = value_t::discarded;
      return;
    }
  }
  // Code Added to allow resuing a parser to parse multiple json values
  // from on input stream.
  bool at_eof() { return last_token == token_type::end_of_input; }
  bool test_eof() {
    get_token();
    return last_token == token_type::end_of_input;
  }
  void init() { get_token(); }
  // Try to parse a value, return false if parsing failed, use when
  // you're not sure if the input is json and it's ok if it's not.
  bool try_parse(basic_json& result, const bool strict = true) {
    get_token();
    parse_internal(true, result);
    if (errored) {
      goto error;
    }
    if (strict && test_eof()) {
      goto error;
    }
    return true;
  error:
    result = value_t::discarded;
    return false;
  }

  basic_json parse(const bool strict = true) {
    basic_json ret;
    parse(strict, ret);
    return ret;
  }
  basic_json parse_next() {
    if (at_eof()) {
      errored = true;
    }
    if (errored) {
      return value_t::discarded;
    }
    basic_json ret;
    parse_internal(true, ret);
    ret.assert_invariant();
    get_token();
    return ret;
  }

  /*!
  @brief public accept interface

  @param[in] strict  whether to expect the last token to be EOF
  @return whether the input is a proper JSON text
  */
  bool accept(const bool strict = true) {
    // read first token
    get_token();

    if (!accept_internal()) {
      return false;
    }

    // strict => last token must be EOF
    return !strict || (get_token() == token_type::end_of_input);
  }

  /*!
  @brief the actual parser
  @throw parse_error.101 in case of an unexpected token
  @throw parse_error.102 if to_unicode fails or surrogate error
  @throw parse_error.103 if to_unicode fails
  */
  void parse_internal(bool keep, basic_json& result) {
    // never parse after a parse error was detected
    assert(!errored);

    // start with a discarded value
    if (!result.is_discarded()) {
      result.m_value.destroy(result.m_type);
      result.m_type = value_t::discarded;
    }

    switch (last_token) {
    case token_type::begin_object: {
      if (keep) {
        if (callback) {
          keep = callback(depth++, parse_event_t::object_start, result);
        }

        if (!callback || keep) {
          // explicitly set result to object to cope with {}
          result.m_type = value_t::object;
          result.m_value = value_t::object;
        }
      }

      // read next token
      get_token();

      // closing } -> we are done
      if (last_token == token_type::end_object) {
        if (keep && callback &&
            !callback(--depth, parse_event_t::object_end, result)) {
          result.m_value.destroy(result.m_type);
          result.m_type = value_t::discarded;
        }
        break;
      }

      // parse values
      string_t key;
      basic_json value;
      while (true) {
        // store key
        if (!expect(token_type::value_string)) {
          return;
        }
        key = m_lexer.move_string();

        bool keep_tag = false;
        if (keep) {
          if (callback) {
            basic_json k(key);
            keep_tag = callback(depth, parse_event_t::key, k);
          } else {
            keep_tag = true;
          }
        }

        // parse separator (:)
        get_token();
        if (!expect(token_type::name_separator)) {
          return;
        }

        // parse and add value
        get_token();
        value.m_value.destroy(value.m_type);
        value.m_type = value_t::discarded;
        parse_internal(keep, value);

        if (JSON_UNLIKELY(errored)) {
          return;
        }

        if (keep && keep_tag && !value.is_discarded()) {
          result.m_value.object->emplace(std::move(key), std::move(value));
        }

        // comma -> next value
        get_token();
        if (last_token == token_type::value_separator) {
          get_token();
          continue;
        }

        // closing }
        if (!expect(token_type::end_object)) {
          return;
        }
        break;
      }

      if (keep && callback &&
          !callback(--depth, parse_event_t::object_end, result)) {
        result.m_value.destroy(result.m_type);
        result.m_type = value_t::discarded;
      }
      break;
    }

    case token_type::begin_array: {
      if (keep) {
        if (callback) {
          keep = callback(depth++, parse_event_t::array_start, result);
        }

        if (!callback || keep) {
          // explicitly set result to array to cope with []
          result.m_type = value_t::array;
          result.m_value = value_t::array;
        }
      }

      // read next token
      get_token();

      // closing ] -> we are done
      if (last_token == token_type::end_array) {
        if (callback && !callback(--depth, parse_event_t::array_end, result)) {
          result.m_value.destroy(result.m_type);
          result.m_type = value_t::discarded;
        }
        break;
      }

      // parse values
      basic_json value;
      while (true) {
        // parse value
        value.m_value.destroy(value.m_type);
        value.m_type = value_t::discarded;
        parse_internal(keep, value);

        if (JSON_UNLIKELY(errored)) {
          return;
        }

        if (keep && !value.is_discarded()) {
          result.m_value.array->push_back(std::move(value));
        }

        // comma -> next value
        get_token();
        if (last_token == token_type::value_separator) {
          get_token();
          continue;
        }

        // closing ]
        if (!expect(token_type::end_array)) {
          return;
        }
        break;
      }

      if (keep && callback &&
          !callback(--depth, parse_event_t::array_end, result)) {
        result.m_value.destroy(result.m_type);
        result.m_type = value_t::discarded;
      }
      break;
    }

    case token_type::literal_null: {
      result.m_type = value_t::null;
      break;
    }

    case token_type::value_string: {
      result.m_type = value_t::string;
      result.m_value = m_lexer.move_string();
      break;
    }

    case token_type::literal_true: {
      result.m_type = value_t::boolean;
      result.m_value = true;
      break;
    }

    case token_type::literal_false: {
      result.m_type = value_t::boolean;
      result.m_value = false;
      break;
    }

    case token_type::value_unsigned: {
      result.m_type = value_t::number_unsigned;
      result.m_value = m_lexer.get_number_unsigned();
      break;
    }

    case token_type::value_integer: {
      result.m_type = value_t::number_integer;
      result.m_value = m_lexer.get_number_integer();
      break;
    }

    case token_type::value_float: {
      result.m_type = value_t::number_float;
      result.m_value = m_lexer.get_number_float();

      // throw in case of infinity or NAN
      if (JSON_UNLIKELY(!std::isfinite(result.m_value.number_float))) {
        if (allow_exceptions) {
          JSON_THROW(out_of_range::create(406, "number overflow parsing '" +
                                                   m_lexer.get_token_string() +
                                                   "'"));
        }
        expect(token_type::uninitialized);
      }
      break;
    }

    case token_type::parse_error: {
      // using "uninitialized" to avoid "expected" message
      if (!expect(token_type::uninitialized)) {
        return;
      }
      break; // LCOV_EXCL_LINE
    }

    default: {
      // the last token was unexpected; we expected a value
      if (!expect(token_type::literal_or_value)) {
        return;
      }
      break; // LCOV_EXCL_LINE
    }
    }

    if (keep && callback && !callback(depth, parse_event_t::value, result)) {
      result.m_value.destroy(result.m_type);
      result.m_type = value_t::discarded;
    }
  }

  /*!
  @brief the actual acceptor

  @invariant 1. The last token is not yet processed. Therefore, the caller
                of this function must make sure a token has been read.
             2. When this function returns, the last token is processed.
                That is, the last read character was already considered.

  This invariant makes sure that no token needs to be "unput".
  */
  bool accept_internal() {
    switch (last_token) {
    case token_type::begin_object: {
      // read next token
      get_token();

      // closing } -> we are done
      if (last_token == token_type::end_object) {
        return true;
      }

      // parse values
      while (true) {
        // parse key
        if (last_token != token_type::value_string) {
          return false;
        }

        // parse separator (:)
        get_token();
        if (last_token != token_type::name_separator) {
          return false;
        }

        // parse value
        get_token();
        if (!accept_internal()) {
          return false;
        }

        // comma -> next value
        get_token();
        if (last_token == token_type::value_separator) {
          get_token();
          continue;
        }

        // closing }
        return (last_token == token_type::end_object);
      }
    }

    case token_type::begin_array: {
      // read next token
      get_token();

      // closing ] -> we are done
      if (last_token == token_type::end_array) {
        return true;
      }

      // parse values
      while (true) {
        // parse value
        if (!accept_internal()) {
          return false;
        }

        // comma -> next value
        get_token();
        if (last_token == token_type::value_separator) {
          get_token();
          continue;
        }

        // closing ]
        return (last_token == token_type::end_array);
      }
    }

    case token_type::value_float: {
      // reject infinity or NAN
      return std::isfinite(m_lexer.get_number_float());
    }

    case token_type::literal_false:
    case token_type::literal_null:
    case token_type::literal_true:
    case token_type::value_integer:
    case token_type::value_string:
    case token_type::value_unsigned: return true;

    default: // the last token was unexpected
      return false;
    }
  }

  /// get next token from lexer
  token_type get_token() { return (last_token = m_lexer.scan()); }

  /*!
  @throw parse_error.101 if expected token did not occur
  */
  bool expect(token_type t) {
    if (JSON_UNLIKELY(t != last_token)) {
      errored = true;
      expected = t;
      if (allow_exceptions) {
        throw_exception();
      } else {
        return false;
      }
    }

    return true;
  }

  [[noreturn]] void throw_exception() const {
    std::string error_msg = "syntax error - ";
    if (last_token == token_type::parse_error) {
      error_msg += std::string(m_lexer.get_error_message()) + "; last read: '" +
                   m_lexer.get_token_string() + "'";
    } else {
      error_msg +=
          "unexpected " + std::string(lexer_t::token_type_name(last_token));
    }

    if (expected != token_type::uninitialized) {
      error_msg +=
          "; expected " + std::string(lexer_t::token_type_name(expected));
    }

    JSON_THROW(parse_error::create(101, m_lexer.get_position(), error_msg));
  }

  /// current level of recursion
  int depth = 0;
  /// callback function
  const parser_callback_t callback = nullptr;
  /// the type of the last read token
  token_type last_token = token_type::uninitialized;
  /// the lexer
  lexer_t m_lexer;
  /// whether a syntax error occurred
  bool errored = false;
  /// possible reason for the syntax error
  token_type expected = token_type::uninitialized;
  /// whether to throw exceptions in case of errors
  const bool allow_exceptions = true;
};
