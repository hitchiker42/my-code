#include <sys/time.h>
#include <time.h>
/*
  nano=1e-9
  micro=1e-6
  milli=1-e3
*/
#define TEN_E9 1000000000
#define TEN_E8 100000000
#define TEN_E7 10000000
#define TEN_E6 1000000
#define TEN_E5 100000
#define TEN_E4 10000
#define TEN_E3 1000

/*Any float literal in C is a double*/
#define NANO_TO_MICRO(nano) (nano/1000)
#define NANO_TO_MILLI(nano) (nano/(1000*1000))
#define NANO_TO_SEC(nano) (nano/TEN_E9)
#define NANO_TO_DOUBLE(nano) (nano*1e-9)

#define MICRO_TO_NANO(micro) (micro*1000)
#define MICRO_TO_MILLI(micro) (micro/1000)
#define MICRO_TO_SEC(nano) (micro/TEN_E6)
#define MICRO_TO_DOUBLE(micro) (nano*1e-6)

#define MILLI_TO_NANO(milli) (milli*1000*1000)
#define MILLI_TO_MICRO(milli) (milli*1000)
#define MILLI_TO_SEC(nano) (milli/TEN_E3)
#define MILLI_TO_DOUBLE(milli) (milli*1e-6)

#define SEC_TO_NANO(sec) (sec*(1000*1000*1000))
#define SEC_TO_MICRO(sec) (sec*(1000*1000))
#define SEC_TO_MILLI(sec) (sec*1000)
#define SEC_TO_DOBULE(sec) (sec*1e1)

#define DOUBLE_TO_NANO(dbl) ((long)(dbl*TEN_E9))
#define DOUBLE_TO_MICRO(dbl) ((long)(dbl*TEN_E6))
#define DOUBLE_TO_MILLI(dbl) ((long)(dbl*TEN_E3))
#define DOUBLE_TO_SEC(dbl) ((long)dbl)

//Conversions
//timeval-timespec conversions
static struct timespec timeval_ptr_to_timespec(struct timeval *tv){
  struct timespec ts={.tv_sec=tv->tv_sec,
                      .tv_nsec=MICRO_TO_NANO(tv->tv_usec)};
  return ts;
}
static struct timespec timeval_to_timespec(struct timeval tv){
  struct timespec ts={.tv_sec=tv.tv_sec,
                      .tv_nsec=MICRO_TO_NANO(tv.tv_usec)};
  return ts;
}

static struct timeval timespec_ptr_to_timeval(struct timespec *ts){
  struct timeval tv={.tv_sec=ts->tv_sec,
                      .tv_nsec=NANO_TO_MICRO(ts->tv_usec)};
  return tv;
}
static struct timeval timespec_to_timeval(struct timespec ts){
  struct timeval tv={.tv_sec=ts->tv_sec,
                      .tv_nsec=NANO_TO_MICRO(ts->tv_usec)};
  return tv;
}

//timeval to number conversions
static long timeval_to_sec(timeval tv){
  return tv.sec + MICRO_TO_SEC(tv.usec);
}
static long timeval_to_msec(timeval tv){
  return (SEC_TO_MILLI(tv.sec)+MICRO_TO_MILLI(tv.usec));
}
static long timeval_to_usec(timeval tv){
  return (SEC_TO_MICRO(tv.sec)+tv.usec);
}
static long timeval_to_nsec(timeval tv){
  return (SEC_TO_NANO(tv.sec)+MICRO_TO_NANO(tv.usec));
}
static double timeval_to_double(timeval tv){
  return (SEC_TO_DOUBLE(tv.sec)+MICRO_TO_DOUBLE(tv.usec));
}
//timespec to number conversions
static long timespec_to_sec(timespec tv){
  return tv.sec + NANO_TO_SEC(tv.nsec);
}
static long timespec_to_msec(timespec tv){
  return (SEC_TO_MILLI(tv.sec)+NANO_TO_MILLI(tv.nsec));
}
static long timespec_to_usec(timespec tv){
  return (SEC_TO_MICRO(tv.sec)+NANO_TO_MICRO(tv.nsec));
}
static long timespec_to_nsec(timespec tv){
  return (SEC_TO_NANO(tv.sec)+tv.nsec);
}
static double timespec_to_double(timespec tv){
  return (SEC_TO_DOUBLE(tv.sec)+NANO_TO_DOUBLE(tv.usec));
}

//number to timeval converisons
static struct timeval sec_to_timeval(long sec){
  struct timeval tv={.tv_sec=sec};
  return tv;
}
static struct timeval msec_to_timeval(long msec){
  long sec=MILLI_TO_SEC(msec);
  struct timeval tv={.tv_sec=sec,
                     .tv_usec=MILLI_TO_MICRO(msec%TEN_E3);}
  return tv;
}
static struct timeval usec_to_timeval(long usec){
  long sec=MICRO_TO_SEC(usec);
  struct timeval tv={.tv_sec=sec,
                     .tv_usec=usec%TEN_E6};
  return tv;
}
static struct timeval nsec_to_timeval(long nsec){
  long sec=NANO_TO_SEC(nsec);
  struct timeval tv={.tv_sec=sec,
                     .tv_usec=NANO_TO_MICRO(nsec%TEN_E9)};
  return tv;
}
static struct timeval double_to_timeval(double dbl){
  long sec=DOUBLE_TO_SEC(dbl);
  struct timeval tv={.tv_sec=sec,
                     .tv_usec=DOUBLE_TO_MICRO(dbl-sec)};
  return tv;
}

//number to timespec
static struct timespec sec_to_timespec(long sec){
  struct timespec tv={.tv_sec=sec};
  return tv;
}
static struct timespec msec_to_timespec(long msec){
  long sec=MILLI_TO_SEC(msec);
  struct timespec tv={.tv_sec=sec,
                      .tv_nsec=MILLI_TO_NANO(msec%TEN_E3)};
  return tv;
}
static struct timespec usec_to_timespec(long usec){
  long sec=MICRO_TO_SEC(usec);
  struct timespec tv={.tv_sec=sec,
                      .tv_nsec=MICRO_TO_NANO(usec%TEN_E6)};
  return tv;
}
static struct timespec nsec_to_timespec(long nsec){
  long sec=NANO_TO_SEC(nsec);
  struct timespec tv={.tv_sec=sec,
                      .tv_nsec=nsec%TEN_E9};
  return tv;
}
static struct timespec double_to_timespec(double dbl){
  long sec=DOUBLE_TO_SEC(dbl);
  struct timespec tv={.tv_sec=sec,
                      .tv_nsec=DOUBLE_TO_NANO(dbl-sec)};
  return tv;
}

//return x-y in nanoseconds
static long timespec_ptr_subtract_nano (struct timespec *x, struct timespec *y){
  long nsec = (x->tv_nsec - y->tv_nsec);
  long sec = (x->tv_sec - y->tv_sec);
  return (nsec + SEC_TO_NANO(sec));
}
static long timespec_subtract_nano (struct timespec x, struct timespec y){
  long nsec = (x.tv_nsec - y.tv_nsec);
  long sec = (x.tv_sec - y.tv_sec);
  return (nsec + SEC_TO_NANO(sec));
}
//return x-y as seconds using floating point
static double timespec_ptr_subtract_double (struct timespec *x, struct timespec *y){
  double nsec = (x->tv_nsec - y->tv_nsec);
  double sec = (x->tv_sec - y->tv_sec);
  return (sec + NANO_TO_SEC(nsec));
}
static double timespec_subtract_double (struct timespec x, struct timespec y){
  double nsec = (x.tv_nsec - y.tv_nsec);
  double sec = (x.tv_sec - y.tv_sec);
  return (sec + NANO_TO_SEC(nsec));
}
//return x-y as a timespec
static struct timespec timespec_ptr_subtract (struct timespec *x, struct timespec *y){
  long nsec = (x->tv_nsec - y->tv_nsec);
  long sec = (x->tv_sec - y->tv_sec);
  return (struct timespec){.tv_sec=sec,.tv_nsec=nsec};
}
static struct timespec timespec_subtract (struct timespec x, struct timespec y){
  long nsec = (x.tv_nsec - y.tv_nsec);
  long sec = (x.tv_sec - y.tv_sec);
  return (struct timespec){.tv_sec=sec,.tv_nsec=nsec};
}

//return x+y in nanoseconds
static long timespec_ptr_add_nano (struct timespec *x, struct timespec *y){
  long nsec = (x->tv_nsec + y->tv_nsec);
  long sec = (x->tv_sec + y->tv_sec);
  return (nsec + SEC_TO_NANO(sec));
}
static long timespec_add_nano (struct timespec x, struct timespec y){
  long nsec = (x.tv_nsec + y.tv_nsec);
  long sec = (x.tv_sec + y.v_sec);
  return (nsec + SEC_TO_NANO(sec));
}
//return x+y as seconds using floating point
static double timespec_ptr_add_double (struct timespec *x, struct timespec *y){
  double nsec = (x->tv_nsec + y->tv_nsec);
  double sec = (x->tv_sec + y->tv_sec);
  return (sec + NANO_TO_SEC(nsec));
}
static double timespec_add_double (struct timespec x, struct timespec y){
  double nsec = (x->tv_nsec + y->tv_nsec);
  double sec = (x.tv_sec + y.tv_sec);
  return (sec + NANO_TO_SEC(nsec));
}
//return x+y as a timespec
static struct timespec timespec_ptr_add (struct timespec *x, struct timespec *y){
  long nsec = (x->tv_nsec + y->tv_nsec);
  long sec = (x->tv_sec + y->tv_sec);
  return (struct timespec){.tv_sec=sec,.tv_nsec=nsec};
}
static struct timespec timespec_add (struct timespec x, struct timespec y){
  long nsec = (x.tv_nsec + y.tv_nsec);
  long sec = (x.tv_sec + y.tv_sec);
  return (struct timespec){.tv_sec=sec,.tv_nsec=nsec};
}

static struct timespec timespec_time(){
  struct timespec tv;
  clock_gettime(CLOCK_REALTIME,&tv);
  return tv;
}
static struct timespec timespec_current_diff(struct timespec tv){
  struct timespec now;
  clock_gettime(CLOCK_REALTIME,&now);
  now.tv_sec-=tv.tv_sec;
  now.tv_nsec-=tv.tv_nsec;
  return now;
}
