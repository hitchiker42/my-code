#ifndef _DEBUG_H_
#define _DEBUG_H_
#if !(defined NDEBUG)
#define DEBUG_PRINTF(fmt,...)                   \
  fprintf(stderr,fmt,##__VA_ARGS__)
#define HERE()                                          \
  fprintf(stderr,"file: %s, line: %d, function: %s\n", \
          __FILE__,__LINE__,__func__)
#define HERE_FMT(fmt,...)                               \
  fprintf(stderr,"file: %s, line: %d, function: %s\n" fmt, \
          __FILE__,__LINE__,__func__,##__VA_ARGS__)
#define HERE_STR(str)                           \
  fprintf(stderr,"file: %s, line: %d, function: %s\n%s",        \
          __FILE__,__LINE__,__func__,str)
//I got these from the linux kernel
#define WARN(cond,fmt,...)                      \
  ({if(cond){                                   \
      HERE_FMT(fmt,##__VA_ARGS__);              \
    };})
#define WARN_ON(cond)                           \
  ({if(cond){                                   \
      HERE();                                   \
    };})
#define WARN_ONCE(cond, fmt, ...)               \
  ({static int warned;                          \
    if(cond){                                   \
      if(!warned){                              \
        HERE_FMT(fmt, ##__VA_ARGS__);           \
        warned = 1;                             \
      }                                         \
    };})

#define WARN_ON_ONCE(cond)                      \
  ({static int warned;                          \
    if(cond){                                   \
      if(!warned){                              \
        HERE();                                 \
        warned = 1;                             \
      }                                         \
    };})
#define BREAKPOINT() raise(SIGTRAP)
#else
#define DEBUG_PRINTF(fmt,...)
#define HERE()
#define HERE_FMT(fmt,...)
#define HERE_STR(str)
#define WARN_ON(...)
#define WARN_ON_ONCE(...)
#define WARN(...)
#define WARN_ONCE(...)
#define BREAKPOINT()
#endif
#endif
