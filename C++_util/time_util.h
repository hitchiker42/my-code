#ifndef __TIME_UTIL_H__
#define __TIME_UTIL_H__
#include <time.h>
#include <sys/time.h>
#include <chrono>
//As long as we only use sleep_for & sleep_until we don't need to link
//with pthreads or anything.
#include <thread>
namespace util {
//Sleep functions, using C++11 stuff
void sleep(double seconds){
  std::chrono::duration<double> dur(seconds);
  std::this_thread::sleep_for(dur);
}
//just a more convient name for std::this_thread::sleep_for
template<typename rep, typename period>
void sleep(const std::chrono::duration<rep, period> &dur){
  std::this_thread::sleep_for(dur);
}
//Timer functions
//Could put these constants / functions into a detail namespace.
static constexpr int NANO_SCALE = 1000000000;
static constexpr double NANO_SCALE_FLOAT = 1e9;
static constexpr int MICRO_SCALE = 1000000;
static constexpr double MICRO_SCALE_FLOAT = 1e6;
static constexpr double timespec_to_float(struct timespec t){
  return t.tv_sec + (t.tv_nsec / NANO_SCALE_FLOAT);
}
//Can't be constxepr due to uninitialized variable.
static struct timespec timeval_to_timespec(struct timeval tv){
  struct timespec ts;
  ts.tv_sec = tv.tv_sec;
  ts.tv_nsec = (tv.tv_usec * (NANO_SCALE/MICRO_SCALE));
  return ts;
}
static struct timespec get_current_time(){
  struct timespec ts;
#if (defined _POSIX_TIMERS) && (_POSIX_TIMERS > 0)
  clock_gettime(CLOCK_REALTIME, &ts);
#else
  struct timeval tv;
  gettimeofday(&tv, NULL);
  ts =  timeval_to_timespec(tv);
#endif
  return ts;
}
static struct timespec get_cpu_time(){
  struct timespec ts;
#if (defined _POSIX_TIMERS) && (_POSIX_TIMERS > 0) && (defined _POSIX_CPUTIME)
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
#else
  clock_t clk = clock();
  int64_t nsecs = (clk*NANO_SCALE)/CLOCKS_PER_SEC;
  ts.tv_sec = nsecs/NANO_SCALE;
  ts.tv_nsec = nsecs % NANO_SCALE;
#endif
  return ts;
}
static inline  double float_time(){
  return timespec_to_float(get_current_time());
}
static inline double float_cputime(){
  return timespec_to_float(get_cpu_time());
}
struct cpu_timer {
  double start_time = 0.0;
  double stop_time = 0.0;

  void start(){
    start_time = float_cputime();
  }
  void stop(){
    stop_time = float_cputime();
  }
  //not strictly necessary
  void reset(){
    start_time = stop_time = 0.0;
  }
  void restart(){
    stop_time = 0.0;
    start_time = float_cputime();
  }
  //Doesn't check if stop/start have been called.
  double elapsed(){
    return (stop_time - start_time);
  }
  template<typename T, typename ... Ts>
  double time_function(T fn, Ts&&... Args){
    reset();
    start();
    (void)fn(std::forward<Ts>(Args)...);
    stop();
    return elapsed();
  }
};
struct function_timer {
  function_timer(std::function<int()> fn)
    : fn{fn} {}
  template<typename T, typename... Ts>
  function_timer(T fn, Ts &&... Args)
    : fn(std::bind(fn, std::forward<Ts>(Args)...)) {}
  std::function<int()> fn;

  double time_fn(int count = 1){
    double start = 0.0, end = 0.0, total = 0.0;
    for(int i = 0; i < count; i++){
      start = float_cputime();
      if(fn() < 0){
        return -1.0;
      }
      end = float_cputime();
      total += (end - start);
    }
    return total / count;
  }
};
struct void_function_timer {
  void_function_timer(std::function<void()> fn)
    : fn{fn} {}
  template<typename T, typename... Ts>
  void_function_timer(T fn, Ts &&... Args)
    : fn(std::bind(fn, std::forward<Ts>(Args)...)) {}
  std::function<void()> fn;

  double time_fn(int count = 1){
    double start = 0.0, end = 0.0, total = 0.0;
    for(int i = 0; i < count; i++){
      start = float_cputime();
      fn();
      end = float_cputime();
      total += (end - start);
    }
    return total / count;
  }
};

}
#endif /* __TIME_UTIL_H__ */
