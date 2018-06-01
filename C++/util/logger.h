#ifndef __LOGGER_H__
#define __LOGGER_H__
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string_view>
#include <type_traits>
#if (defined USE_FMT)
#include "fmt/core.h"
#include "fmt/format.h"
//#include "fmt/time.h"
//#include "fmt/printf.h"
#endif
namespace util {
//Names borrowed from linux printk levels
enum class log_level {
  emergency = 0,
  critical = 1,
  error = 2,
  warning = 3,
  warn = 3,
  notice = 4,
  info = 5,
  debug = 6,
  all = 7
};
//Core logger struct, uses integer log levels & logs to a single FILE*,
//provides support for using an enumeration for log levels via templated wrappers,
//the above log_level enum is provided as a convenient default.
struct logger {
  FILE* out;
  int log_level_max;
  //If logging to a file change to line buffering so tail -f can be used.
  logger(const char *outfile, int max_level, bool append = false)
    : out{fopen(outfile, (append ? "a" : "w"))}, log_level_max{max_level} {
      setlinebuf(out);
    }
  logger(FILE *out, int max_level)
    : out{out}, log_level_max{max_level} {
      setlinebuf(out);
    }
  logger(int max_level)
    : out{stderr}, log_level_max{max_level} {}
  template<typename T,
           std::enable_if_t<std::is_enum_v<T>, int> = 0>
  logger(const char *outfile, T max_level, bool append = false)
    : out{fopen(outfile, (append ? "a" : "w"))},
      log_level_max{to_underlying(max_level)} {}
  template<typename T,
           std::enable_if_t<std::is_enum_v<T>, int> = 0>
  logger(FILE *out, T max_level)
    : out{out}, log_level_max{to_underlying(max_level)} {}
  template<typename T,
           std::enable_if_t<std::is_enum_v<T>, int> = 0>
  logger(T max_level)
    : out{stderr}, log_level_max{to_underlying(max_level)} {}

  ~logger(){
    //If we're logging to stdout/stderr don't close them when we go out of scope.
    if(out != stdout && out == stderr){
      fclose(out);
    }
  }
  operator bool(){
    return out;
  }
  //if last_msg doesn't end with a newline write a newline to out and return true,
  //otherwise return false.
  bool ensure_newline(std::string_view last_msg){
    if(last_msg.back() != '\n'){
      fputc('\n', out);
      return true;
    } else {
      return false;
    }
  }
  //Log pthe msg exactly as is
  void log(int level, std::string_view msg){
    if(level <= log_level_max){
      fwrite(msg.data(), msg.size(), 1, out);
    }
  }
  template<typename T,
           std::enable_if_t<std::is_enum_v<T>, int> = 0>
  void log(T level, std::string_view msg){
    return log(to_underlying(level), msg);
  }
  //log the msg and append a newline if it doesn't already end with one.
  void print(int level, std::string_view msg){
    if(level <= log_level_max){
      fwrite(msg.data(), msg.size(), 1, out);
      ensure_newline(msg);
    }
  }
  template<typename T,
           std::enable_if_t<std::is_enum_v<T>, int> = 0>
  void print(T level, std::string_view msg){
    return print(to_underlying(level), msg);
  }
  template<typename ... Ts>
  void printf(int level, const char *fmt, const Ts&... args){
    if(level <= log_level_max){
      fprintf(out, fmt, args...);
      //if fmt ends in %c or %s its possible we already output a newline,
      //but there's no easy way to check that.
      ensure_newline(fmt);
    }
  }
  template<typename T,
           typename ... Ts,
           std::enable_if_t<std::is_enum_v<T>, int> = 0>
  void printf(T level, const char *fmt, const Ts&... args){
    return this->printf(to_underlying(level), fmt, args...);
  }
#if (defined USE_FMT)
  template<typename ... Ts>
  void format(int level, std::string_view fmt, const Ts&... args){
    if(level <= log_level_max){
      fmt::print(out, fmt, std::forward<Ts>(args)...);
      ensure_newline(fmt);
    }
  }
  template<  template<typename T,
                      typename ... Ts,
                      std::enable_if_t<std::is_enum_v<T>, int> = 0>
  void format(T level, std::string_view fmt, const Ts&... args){
    return format(to_underlying(level), fmt, std::forward<Ts>(args)...);
  }
#endif
};
}
#include "macros.h"
//Only works with a string literal as str.
#define LOG_HERE(logger, level, str, ...)                               \
  if constexpr(std::is_pointer_v<decltype(logger)>){                    \
    logger->printf(level, "File %s, Line %d, function %s:\n" str,       \
                   __FILE__, __LINE__, __PRETTY_FUNCTION__              \
                   MAYBE_VA_ARGS(__VA_ARGS__));                         \
  } else {                                                              \
    logger.printf(level, "File %s, Line %d, function %s:\n" str,        \
                  __FILE__, __LINE__, __PRETTY_FUNCTION__               \
                  MAYBE_VA_ARGS(__VA_ARGS__));                          \
  }
#if (defined USE_FMT)
#define LOG_HERE_FMT(logger, level, str, ...)                           \
  if constexpr(std::is_pointer_v<decltype(logger)>){                    \
    logger->format(level, "File %s, Line %d, function %s:\n" str,       \
                   __FILE__, __LINE__, __PRETTY_FUNCTION__              \
                   MAYBE_VA_ARGS(__VA_ARGS__));                         \
  } else {                                                              \
    logger.format(level, "File %s, Line %d, function %s:\n" str,        \
                  __FILE__, __LINE__, __PRETTY_FUNCTION__               \
                  MAYBE_VA_ARGS(__VA_ARGS__));                          \
  }
#endif

#endif /* __LOGGER_H__ */