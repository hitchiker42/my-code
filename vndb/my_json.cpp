#include "my_json.h"
struct lexer {
  using number_integer_t = typename basic_json::number_integer_t;
  using number_unsigned_t = typename basic_json::number_unsigned_t;
  using number_float_t = typename basic_json::number_float_t;
  using string_t = typename basic_json::string_t;

  /// token types for the parser
  enum class token_type {
    uninitialized,  ///< indicating the scanner is uninitialized
    literal_true,   ///< the `true` literal
    literal_false,  ///< the `false` literal
    literal_null,   ///< the `null` literal
    value_string,   ///< a string -- use get_string() for actual value
    value_unsigned, ///< an unsigned integer -- use get_number_unsigned() for
                    ///< actual value
    value_integer,  ///< a signed integer -- use get_number_integer() for actual
                    ///< value
    value_float,    ///< an floating point number -- use get_number_float() for
                    ///< actual value
    begin_array,    ///< the character for array begin `[`
    begin_object,   ///< the character for object begin `{`
    end_array,      ///< the character for array end `]`
    end_object,     ///< the character for object end `}`
    name_separator, ///< the name separator `:`
    value_separator, ///< the value separator `,`
    parse_error,     ///< indicating a parse error
    end_of_input,    ///< indicating the end of the input buffer
    literal_or_value ///< a literal or the begin of a value (only for
                     ///< diagnostics)
  };

  /// return name of values of type token_type (only used for errors)
  static const char* token_type_name(const token_type t) noexcept {
    switch (t) {
    case token_type::uninitialized: return "<uninitialized>";
    case token_type::literal_true: return "true literal";
    case token_type::literal_false: return "false literal";
    case token_type::literal_null: return "null literal";
    case token_type::value_string: return "string literal";
    case lexer::token_type::value_unsigned:
    case lexer::token_type::value_integer:
    case lexer::token_type::value_float: return "number literal";
    case token_type::begin_array: return "'['";
    case token_type::begin_object: return "'{'";
    case token_type::end_array: return "']'";
    case token_type::end_object: return "'}'";
    case token_type::name_separator: return "':'";
    case token_type::value_separator: return "','";
    case token_type::parse_error: return "<parse error>";
    case token_type::end_of_input: return "end of input";
    case token_type::literal_or_value: return "'[', '{', || a literal";
    default:                  // catch non-enum values
      return "unknown token"; // LCOV_EXCL_LINE
    }
  }

  explicit lexer(detail::input_adapter_t adapter)
      : ia(std::move(adapter)), decimal_point_char(get_decimal_point()) {}

  // delete because of pointer members
  lexer(const lexer&) = delete;
  lexer& operator=(lexer&) = delete;
}
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
