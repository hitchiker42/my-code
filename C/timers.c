#include <sys/types.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#define BILLION 1000000000
//return x-y in nanoseconds
long timespec_ptr_subtract_nano (struct timespec *x, struct timespec *y){
  long nsec = (x->tv_nsec - y->tv_nsec);
  long sec = (x->tv_sec - y->tv_sec);
  return (nsec + (sec*BILLION));
}
long timespec_subtract_nano (struct timespec x, struct timespec y){
  long nsec = (x->tv_nsec - y->tv_nsec);
  long sec = (x->tv_sec - y->tv_sec);
  return (nsec + (sec*BILLION));
}
//return x-y as seconds using floating point
double timespec_ptr_subtract_sec (struct timespec *x, struct timespec *y){
  double nsec = (x->tv_nsec - y->tv_nsec);
  double sec = (x->tv_sec - y->tv_sec);
  return (sec + (nsec/BILLION));
}
double timespec_subtract_sec (struct timespec x, struct timespec y){
  double nsec = (x->tv_nsec - y->tv_nsec);
  double sec = (x->tv_sec - y->tv_sec);
  return (sec + (nsec/BILLION));
}
struct timespec timespec_ptr_subtract (struct timespec *x, struct timespec *y){
  long nsec = (x->tv_nsec - y->tv_nsec);
  long sec = (x->tv_sec - y->tv_sec);
  return (struct timespec){.tv_sec=sec,.tv_nsec=nsec};
}
struct timespec timespec_subtract (struct timespec x, struct timespec y){
  long nsec = (x->tv_nsec - y->tv_nsec);
  long sec = (x->tv_sec - y->tv_sec);
  return (struct timespec){.tv_sec=sec,.tv_nsec=nsec};
}
struct time_differences {double t1;double t2;};
struct fn_times time_fns(void(*f1)(void),void(*f2)(void)){
  struct timespec start,f1_timespec,f2_timespec;
  clock_gettime(CLOCK_MONOTONIC,&start);
  f1();
  clock_gettime(CLOCK_MONOTONIC,&f1_timespec);
  f2();
  clock_gettime(CLOCK_MONOTONIC,&f2_timespec);
  return (fn_times){.t1=timespec_subtract_sec(f1_timespec,start),
      .t2=timespec_subtract_sec(f2_timespec,f1_timespec)}
}
#define time_exprs(expr1,expr2)                                 \
  ({struct timespec start,e1_timespec,e2_timespec;              \
    clock_gettime(CLOCK_MONOTONIC,&start);                      \
    expr1;                                                      \
    clock_gettime(CLOCK_MONOTONIC,&e1_timespec);                \
    expr2;                                                      \
    clock_gettime(CLOCK_MONOTONIC,&e2_timespec);                \
    (fn_times){.t1=timespec_subtract_sec(e1_timespec,start),    \
        .t2=timespec_subtract_sec(e2_timespec,e1_timespec);})
#define loop_expr(type,expr,count)                   \
  ({volatile type dummy;                             \
    int i;                                           \
    for(i=0;i<count;i++){                            \
      dummy=expr;                                    \
    };})
