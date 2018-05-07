#include <type_traits>
#include <unordered_map>
#include <string>
#include <vector>
#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include "util.h"
#include "string_view.h"
struct json_value {
  enum class json_null_t {val = 0};
  using string_t = std::string;
  using object_t = std::unordered_map<string_t, json_value>;
  using array_t = std::vector<json_value>;
  using real_t = double;
  using integer_t = int64_t;
  using boolean_t = bool;
  using null_t = json_null_t;
  enum json_tag {
    //Stored as pointers
    object_tag = 0b000, object_type = object_tag,
    array_tag = 0b001, array_type = array_tag,
    string_tag = 0b010, string_type = string_tag,
    real_tag = 0b011, real_type = real_tag,
    //stored literally
    integer_tag = 0b100, integer_type = integer_tag,
    boolean_tag = 0b101, boolean_type = boolean_tag,
    null_tag = 0b110, null_type = null_tag,
    tag_mask = 0b111 //also indicates an unknown type (i.e an error)
  };
  using json_type = json_tag;
  static constexpr bool type_stored_as_pointer(json_type t){
    return (t == object_type || t == array_type ||
            t == string_type || t == real_type);
  }
  static constexpr bool type_stored_inline(json_type t){
    return !type_stored_as_pointer(t);
  }


  template<typename T>
  static const json_tag type_tag = tag_mask;

  template<typename T>
  struct stored_as_pointer : std::false_type {};

  template<typename T>
  static inline constexpr bool stored_as_pointer_v = stored_as_pointer<T>::value;

  template<typename T>
  struct stored_inline : std::false_type {};


  template<typename T>
  static inline constexpr bool stored_inline_v = stored_inline<T>::value;

  //null constant
  static constexpr json_null_t json_null = json_null_t::val;

  union value {
    //internal use
    void *ptr;
    uintptr_t bits;
    //stored as pointers
    object_t *obj;
    array_t *arr;
    string_t *str;
    real_t *real;
    //stored inline
    integer_t num;
    boolean_t boolean;
    null_t nul;
    value() : bits{0} {}
    value(uintptr_t bits) : bits{bits} {}
      
    //No typechecking here, the constructors for the actual
    //json_value type do that.
    template<typename T>
    value(T *t_ptr)
      : bits{reinterpret_cast<uintptr_t>(t_ptr) | type_tag<T>} {}
    template<typename T>
    value(T t_val)
      : bits{(static_cast<uintptr_t>(t_val) << 3) | type_tag<T>} {}
  };
  //only data member
  value val;

  json_tag tag() const {
    return (json_tag)(val.bits & tag_mask);
  }
  json_type type() const {
    return (json_type)(val.bits & tag_mask);
  }
  void set_tag(json_tag t){
    val.bits |= t;
  }
  //just masks out the tag bits
  uintptr_t as_bits() const {
    return (val.bits & ~tag_mask);
  }
  void* as_ptr() const {
    return reinterpret_cast<void*>(as_bits());
  }
  // object_t* as_obj(){
  //   assert(tag() == object_tag);
  //   return static_cast<object_t*>(as_ptr());
  // }
  // const object_t* as_obj() const {
  //   assert(tag() == object_tag);
  //   return static_cast<object_t*>(as_ptr());
  // }
  // array_t* as_arr(){
  //   assert(tag() == array_tag);
  //   return static_cast<array_t*>(as_ptr());
  // }
  // const array_t* as_arr() const {
  //   assert(tag() == array_tag);
  //   return static_cast<array_t*>(as_ptr());
  // }
  // string_t* as_str(){
  //   assert(tag() == string_tag);
  //   return static_cast<string_t*>(as_ptr());
  // }
  // const string_t* as_str() const {
  //   assert(tag() == string_tag);
  //   return static_cast<string_t*>(as_ptr());
  // }
  // real_t* as_real_ptr(){
  //   assert(tag() == real_tag);
  //   return static_cast<real_t*>(as_ptr());
  // }
  // const real_t* as_real_ptr() const {
  //   assert(tag() == real_tag);
  //   return static_cast<real_t*>(as_ptr());
  // }
  // integer_t as_int() const {
  //   assert(tag() == integer_tag);
  //   return (static_cast<integer_t>(as_bits()) >> 3);
  // }
  // boolean_t as_bool() const {
  //   assert(tag() == boolean_tag);
  //   return (static_cast<boolean_t>(as_bits()) >> 3);
  // }
  // null_t as_null() const {
  //   assert(tag() == null_tag);
  //   return json_null;
  // }
#define define_get_test_ptr_(type, abbr, extra)          \
  CAT(type,_t)* CAT3(as_, abbr, extra)(){               \
    assert(tag() == CAT(type, _tag));                   \
    return static_cast<CAT(type,_t)*>(as_ptr());        \
  }                                                     \
  const CAT(type,_t)* CAT3(as_, abbr, extra)() const {  \
    assert(tag() == CAT(type, _tag));                   \
    return static_cast<const CAT(type,_t)*>(as_ptr());  \
  }                                                     \
  bool CAT(is_, type)() const {                         \
    return (tag() == CAT(type, _tag));                  \
  }

#define define_get_test_ptr(type, abbr) define_get_test_ptr_(type, abbr,)

#define define_get_test_lit(type, abbr)                 \
  CAT(type, _t) CAT(as_, abbr)() const {                \
    assert(tag() == CAT(type, _tag));                   \
    return static_cast<CAT(type,_t)>(as_bits()>>3);     \
  }                                                     \
  bool CAT(is_, type)() const {                         \
    return (tag() == CAT(type, _tag));                  \
  }
  
  
define_get_test_ptr_(real, real, _ptr);
define_get_test_ptr(object, obj);
define_get_test_ptr(array, arr);
define_get_test_ptr(string, str);

define_get_test_lit(integer, int);
define_get_test_lit(boolean, bool);
define_get_test_lit(null, null);

  char* as_c_str(){
    return as_str()->data();
  }
  const char* as_c_str() const {
    return as_str()->c_str();
  }
  real_t as_real() const {
    return *as_real_ptr();
  }

  //default constructor makes a null value
  json_value() : val{json_null} {};
  //Takes ownership of given pointer.
  json_value(object_t *obj) : val{obj} {};
  json_value(array_t *arr) : val{arr} {};
  json_value(string_t *str) : val{str} {};
  json_value(real_t *dbl) : val{dbl} {};

  //Basic constructors 
  json_value(const object_t& obj) : val{new object_t(obj)} {};
  json_value(const array_t& arr) : val{new array_t(arr)} {};
  json_value(const string_t& str) : val{new string_t(str)} {};
  json_value(const real_t dbl) : val{new real_t(dbl)} {};

  json_value(const integer_t num) : val{num} {};
  json_value(const int num) : val{num} {};

  json_value(const bool boolean) : val{boolean} {};
  json_value(const null_t nul) : val{nul} {};

  json_value(const json_value &other) {
    if(type_stored_inline(other.type())){
      val.bits = other.val.bits;
    } else {
      switch(other.type()){
        case object_type:
          val.obj = new object_t(*other.as_obj()); break;
        case array_type:
          val.arr = new array_t(*other.as_arr()); break;
        case string_type:
          val.str = new string_t(*other.as_str()); break;
        case real_type:
          val.real = new real_t(other.as_real()); break;
        default:
          __builtin_unreachable();
      }
      val.bits |= other.type();
    }
  }
  json_value(json_value &&other)
    : val{other.val.bits} {
    other.val.bits = null_tag;
  }
    
  
  //Construct an empty value of a given type
  json_value(const json_type type) {
    switch(type) {
      case object_type:
        val.obj = new object_t(); break;
      case array_type:
        val.arr = new array_t(); break;
      case string_type:
        val.str = new string_t(); break;
      case real_type:
        val.real = new real_t(); break;
      default:
        val.bits = 0;
    }
    val.bits |= type;
  }
  json_value(std::initializer_list<json_value> ls){
    auto is_obj_p = [](const json_value &val){
      return ((val.type() == array_type) &&
              (val.size() == 2) &&
              val[0].type() == string_type);
    };
    for(auto &&v : ls){
      if(!is_obj_p(v)){
        val.arr = new array_t(ls);
        val.bits |= array_type;
        return;
      }
    }
    val.obj = new object_t();
    for(auto &&v : ls){
      val.obj->emplace(*v[0].as_str(), v[1]);
    }
    val.bits |= object_type;
  }
           
  static json_value make_object(std::initializer_list<
                                std::pair<string_t, json_value>> ls){
    return json_value(new object_t(ls.begin(), ls.end()));
  }

  static json_value make_array(std::initializer_list<json_value> ls){
    return json_value(new array_t(ls));
  }

  template<typename T, typename ... Ts>
  static json_value create(Ts&&... Args){
    if constexpr(type_tag<T> == object_tag){
      return json_value(new object_t(std::forward<Ts>(Args)...));
    } else if(type_tag<T> == array_tag){
      return json_value(new array_t(std::forward<Ts>(Args)...));
    } else if(type_tag<T> == string_tag){
      return json_value(new string_t(std::forward<Ts>(Args)...));
    } else if(type_tag<T> == real_tag){
      return json_value(new real_t(std::forward<Ts>(Args)...));
    } else {
      return json_value(Args...);
    };
  }
  json_value(const char *str)
    : val{new string_t(str)} {}


  ~json_value(){
    switch(tag()){
      case object_tag:
        delete as_obj(); return;
      case array_tag:
        delete as_arr(); return;
      case string_tag:
        delete as_str(); return;
      case real_tag:
        delete as_real_ptr(); return;
      default:
        return;
    }
  }
  template<typename T,
           std::enable_if_t<stored_inline_v<T>, int> = 0>
  json_value& operator=(const T&val){
    val.bits = ((static_cast<uintptr_t>(val) << 3) | type_tag<T>);
    return (*this);
  }

  json_value& operator[](size_t idx){
    assert(type() == array_type);
    return (*as_arr())[idx];
  }
  const json_value& operator[](size_t idx) const {
    assert(type() == array_type);
    return (*as_arr())[idx];
  }
  json_value& operator[](std::string str){
    assert(type() == object_type);
    return (*as_obj())[str];
  }
  // const json_value& operator[](std::string str){
  //   assert(type() == object_type);
  //   return (*as_obj())[str];
  // }
  json_value& push_back(const json_value& val){
    assert(type() == array_type);
    as_arr()->push_back(val);
    return (*this);
  }
  size_t size() const {
    switch(type()){
      case object_type: return as_obj()->size();
      case array_type: return as_arr()->size();
      case string_type: return as_str()->size();
      default://should this be an error instead?
        return 1;
    }
  }
  
};
template <>
const json_value::json_tag
json_value::type_tag<json_value::object_t> = json_value::object_tag;
template <>
const json_value::json_tag
json_value::type_tag<json_value::array_t> = json_value::array_tag;
template <>
const json_value::json_tag
json_value::type_tag<json_value::string_t> = json_value::string_tag;
template <>
const json_value::json_tag
json_value::type_tag<json_value::real_t> = json_value::real_tag;
template <>
const json_value::json_tag
json_value::type_tag<json_value::integer_t> = json_value::integer_tag;
template <>
const json_value::json_tag
json_value::type_tag<int> = json_value::integer_tag;
template <>
const json_value::json_tag
json_value::type_tag<json_value::boolean_t> = json_value::boolean_tag;
template <>
const json_value::json_tag
json_value::type_tag<json_value::null_t> = json_value::null_tag;

template<>
struct json_value::stored_as_pointer<json_value::object_t> : std::true_type {};
template<>
struct json_value::stored_as_pointer<json_value::array_t> : std::true_type {};
template<>
struct json_value::stored_as_pointer<json_value::string_t> : std::true_type {};
template<>
struct json_value::stored_as_pointer<json_value::real_t> : std::true_type {};

template<>
struct json_value::stored_inline<json_value::integer_t> : std::true_type {};
template<>
struct json_value::stored_inline<int> : std::true_type {};
template<>
struct json_value::stored_inline<json_value::boolean_t> : std::true_type {};
template<>
struct json_value::stored_inline<json_value::null_t> : std::true_type {};
int main(){
  json_value int_test(42);
  json_value real_test(3.1415926);
  json_value vec_test{1,2,3};
  json_value obj_test{{"Hello", ", World"}};

  printf("%ld | %f | {%ld, %ld, %ld} | %s\n",
         int_test.as_int(), real_test.as_real(),
         vec_test[0].as_int(),
         vec_test[1].as_int(),
         vec_test[2].as_int(),
         obj_test["Hello"].as_c_str());
}

#if 0
//default json type uses a std::map for objects, I'd rather
//use an unordered_map
using json = json_ns::basic_json<>;
void serialize_json(const json& obj, string_buf& buf){
  using value_t = json::value_t;
  switch(obj.type()){
    case value_t::null:
      return (void)buf.append("null");
    case value_t::boolean:
      return (void)buf.append(obj.get<bool>() ? "true" : "false");
    case value_t::number_integer:
      return (void)buf.append(obj.get<int64_t>());
    case value_t::number_unsigned:
      return (void)buf.append(obj.get<uint64_t>());
    case value_t::number_float:
      return (void)buf.append(obj.get<double>());
    //This just directly copies the string, which could cause
    //issues if there are any weird characters/escape sequences.
    case value_t::string:{
      auto *str = obj.get_ptr<json::string_t*>();
      return (void)buf.append(str.data(), str.size());
    }
    case value_t::object:{
      auto it = obj.begin();
      //could do; char prefix = '{', after loop: prefix = ',';
      buf.append("{\"");
      buf.append(it->first);
      buf.append("\" : ");
      serialize_json(it->second, buf);
      while(++it != obj.end()){
        buf.append(",\"");
        buf.append(it->first);
        buf.append("\" : ");
        serialize_json(it->second, buf);
      }
      buf.append('}');
      return;
    }
    case value_t::array:{
      auto it = obj.begin();
      buf.append('[');
      serialize_json(*it, buf);
      while(++it != obj.end){
        buf.append(',');
        serialize_json(*it, buf);
      }
      buf.append(']');
    }
    default:
      abort();
  }
}
#endif
