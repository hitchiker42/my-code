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
//Put functions relating to time here
#include "time_manipulation.h"
typedef struct internal_alarm *alarm_ptr;
typedef uint64_t alarm_id;
#endif
