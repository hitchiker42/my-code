/* Alarm daemon, this is a rewrite of a previously failed attempt.
   I'm writing the header/api first, since I know what I want to
   be able to do, then writing the code second. This should
   prevent feature creep, which is something I do a lot

   daemon started via a systemd service, user interface to the
   daemon is via the alarm (name subject to change) program
   USAGE: alarm [general options] action [action options] action-args...
   actions include:
     list: list all pending alarms, or depending on options
           all alarms (alarms can be suspended without being 
           deleted), this is the primary method of determining
           alarm id's
     delete: given an alarm id check if an alarm with that id exists
             if so delete it with a message, otherwise print
             a message indicating the alarm does not exist
     add: add an alarm, varity of suboptions:
       default: given a time schedual a one time alarm for that time
       using the default alarm (which is playing an audio file,
       which file is used can be changed)
                given a time and a pathname assume that path
       represents an audio file (check if it's an audio file
       unless told not to)
       default arguments:
         -l/--loop count, how many times to play the file
       
       -c/--command: given two arguments a time and 
       a string create and add an alarm for  the time
       that runs the given string as a shell command
     snooze: given a alarm id and a time reschedual the 
     alarm, there is a more complicated function for reschedualing
     alarms in more complex ways. 
     the format is: alarm snooze id [+-]?[0-9]{1,2}(\:[0-9]{1,2})
     without if the +/- only [0-9]|1[0-9]|2[0-3]|00 are valid
     for the first two digits and only [0-9]|[1-5][0-9] are 
     valid for the second. 

     stop: stop any running alarms
     
     time formats will use parse_datatime from gnulib
       
*/
#ifndef _ALARMD_H
#define _ALARMD_H
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <semaphore.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include "sd-daemon.h"
#include "heap.h"//binary heap for use as a priority queue
/* Parse a date/time string, storing the resulting time value into *RESULT.
   string itself is pointed to by P. Return true if successful.
   can be an incomplete or relative time specification; if so, use
   as the basis for the returned time. */
int parse_datetime(struct timespec *result, const char *P, 
                   const struct timespec *now);
typedef struct internal_alarm *alarm_ptr;
typedef uint64_t alarm_id;
struct alarm_state{
}
#define MAX(a,b)                                \
  ({ __typeof__ (a) _a = (a);                   \
    __typeof__ (b) _b = (b);                    \
    _a > _b ? _a : _b;})
#define MIN(a,b)                                \
  ({ __typeof__ (a) _a = (a);                   \
    __typeof__ (b) _b = (b);                    \
    _a < _b ? _a : _b;})
#define SWAP(a,b)                               \
  ({ __typeof__ (a) _a = a;                     \
    a=b;                                        \
    b=_a;                                       \
    ;})
#if (defined DEBUG) && !(defined NDEBUG)
#define HERE() fprintf(stderr,"here at %s,line %d\n",__FILE__,__LINE__)
#define PRINT_MSG(string) fprintf(stderr,string);
#define PRINT_FMT(string,fmt...) fprintf(stderr,string,##fmt);
#define FN_START fprintf(stderr,"starting %s\n",__func__);
#define FN_END fprintf(stderr,"finishing %s\n",__func__);
#else
#define HERE()
#define PRINT_MSG(string)
#define PRINT_FMT(string,fmt...)
#define FN_START
#define FN_END
#endif

#endif

