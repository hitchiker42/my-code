#include "util.h"
#include "string_buf.h"
#include <catch/catch.hpp>
void clear_string_view_buf(util::string_view_buf& buf){
  buf.vec.clear();
  buf.buf.clear();
}
TEST_CASE("String view equality", "[string_view]"){
  const char *data = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  SECTION("Equility between copy and view"){
    util::string_view t1(data);
    util::string_view t2(data, true);
    REQUIRE(t1 == t2);
  }
  SECTION("Equility ignores null terminator"){
    util::string_view t1(data);
    util::string_view t2(data, 26*2, false, false);
    REQUIRE(t1 == t2);
  }
  SECTION("Equility with std::string_view"){
    util::string_view t1(data);
    std::string_view t2(data);
    REQUIRE(t1 == t2);
  }
  SECTION("Equility when cast to std::string_view"){
    util::string_view t1(data);
    std::string_view t2(data);
    REQUIRE(((std::string_view)t1) == t2);
  }  
  SECTION("Equility with std::string"){
    util::string_view t1(data);
    std::string t2(data);
    REQUIRE(t1 == t2);
  }
}  
TEST_CASE("Basic string_view_buf functionality", "[string_view_buf]"){
  util::string_view_buf buf{};
  util::string_view hw("Hello, World");
  util::string_view alpha("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");

  REQUIRE(buf.size() == 0);
  REQUIRE(buf.buf.capacity() == buf.bufsz);

  SECTION("Appending raw bytestrings"){
    buf.append(hw.data(), hw.size());
    REQUIRE(buf.to_string_view() == hw);
  }
  SECTION("Appending individual characters"){
    for(auto c : hw){
      buf.append(c);
    }
    REQUIRE(buf.to_string_view() == hw);
  }
  SECTION("Appending small strings copies them"){
    buf.append(hw);
    REQUIRE(buf.size() == hw.size());
    REQUIRE(buf.sz == 0);
    REQUIRE(buf.buf.size() == hw.size());
    REQUIRE(buf.to_string_view() == hw);
  }      
    
  SECTION("Appending long strings doesn't copy them"){
    std::string alpha2 = alpha.to_std_string();
    
    buf.append(alpha);
    REQUIRE(buf.size() == alpha.size());
    REQUIRE(buf.buf.size() == 0);

    buf.append(alpha2);   
    
    REQUIRE(buf.size() == alpha.size()*2);
    REQUIRE(buf.vec.size() == 2);
    REQUIRE(buf.buf.size() == 0);

    SECTION("Moving data clears buffer"){
      util::string_view alpha3 = buf.move_to_string_view();
      REQUIRE(buf.size() == 0);
      REQUIRE(buf.vec.size() == 0);
      REQUIRE(buf.buf.size() == 0);

      alpha2.append(alpha.data(), alpha.size());
      REQUIRE(((std::string_view)alpha3) == ((std::string_view)alpha2));
    }
  }

  SECTION("Appending Numbers"){
    SECTION("Appending Integers"){
      std::string_view control("0123456789101112131415");
      for(int i = 0; i < 16; i++){
        buf.append(i);
      }
      util::string_view tmp = buf.move_to_string_view();
      REQUIRE(tmp.size() == control.size());
      REQUIRE(tmp == control);

      std::string_view control2("0123456789abcdef");
      for(unsigned int i = 0; i < 16; i++){
        buf.append_hex(i);
      }
      tmp = buf.move_to_string_view();
      REQUIRE(tmp.size() == control2.size());
      REQUIRE(tmp == control2);
    }
    SECTION("Appending Floats"){
      std::string_view control("2.718282 3.141593");
      buf.append(M_E);
      buf.append(" ");
      buf.append(M_PI);
      REQUIRE(buf.to_string_view().size() == control.size());
      REQUIRE(buf.to_string_view() == control);
    }
  }

  SECTION("Appending formatted data"){
    char test_buf[512];
    int i = 10;
    double d = M_PI;
    const char *cstr = "Hello, ";
    char char_buf[] = {'W','o','r','l','d'};
    const char* fmt = "Integer : %#0x\nDouble : %10f\nc-string : %s\n"
                      "character buffer : %.*s\npointer : %p\n";
    INFO("Calling snprintf");
    snprintf(test_buf, 512, fmt,
             i, d, cstr, 5, char_buf, fmt);
    INFO(test_buf);
    buf.append_formated(fmt,
                        i, d, cstr, 5, char_buf, fmt);
    REQUIRE(!strcmp(buf.c_str(), test_buf));
  }
}
             
