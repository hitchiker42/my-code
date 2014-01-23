/* global header file for alarmd implementation*/
#ifndef _ALARM_D
#define _ALARM_D
#include <pthread.h>
#include <signal.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include "sd-daemon.h"
#if !(defined(NDEBUG))
#define PRINT_MSG(msg) fprintf(stderr,msg "\n")
#define PRINT_FMT(msg,fmt...) fprintf(stderr,msg"\n",##fmt)
#else
#define PRINT_MSG(msg)
#define PRINT_FMT(msg,fmt...)
#endif
#define xfree free
typedef struct my_alarm my_alarm;
typedef enum alarmd_action alarmd_action;
static pthread_mutex_t alarm_lock=PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t alarm_cond=PTHREAD_COND_INITIALIZER;
int alarm_socket;
my_alarm **alarm_queue;
pid_t command_process;
pthread_attr_t detached_attr;
pthread_t main_thread;
time_t next_alarm;
uint32_t queue_length;
uint32_t queue_size;
static const char *dir_name="/var/run/alarmd";
static const char *mplayer="/usr/bin/mplayer";
static struct timespec wait_time = {.tv_sec=2,.tv_nsec=0};
//100 milisecond wait, presumably enough for a reasonable process 
//to run cleanup functions
static struct timespec nano_wait_time = {.tv_sec=0,.tv_nsec=1e4};
static struct timespec nano_wait_time2 = {.tv_sec=0,.tv_nsec=(1e9-1e4)};
static int make_alarm_socket(const char* filename,int mode);
my_alarm* alarm_heap_pop();
void alarm_heap_delete(int index);
void alarm_heap_add(my_alarm *alarm);
int alarm_heap_list(char **str_loc);
int alarm_heap_try_delete(uint32_t alarm_id);
int init_daemon();
static const char *sock_name="alarmd_socket";
static char repeat_opt[10]={'-','l','o','o','p',' ','0','0','1','\0'};
//I could use bitfields but eh, size isn't all that important
struct my_alarm {
  time_t alarm_time;
  char *command;
  uint32_t command_len;
  uint8_t today;
  uint8_t repeat;
  uint8_t music;
  uint8_t music_loop;//make sure to set this to 1 by default
  //because 0 is infinite loop, and thats something kinda useful
  uint32_t alarm_id;
};
#define oom_error_call() fprintf(stderr,"Error virtual memory exhausted\n")
//this really doesn't need to be super efficent so malloc=calloc
static inline void* xmalloc(size_t size){
  void *temp=calloc(size,sizeof(char));
  if(!temp && size){
    oom_error_call();
    exit(EXIT_FAILURE);
  } else {
    return temp;
  }
}
static inline void* xrealloc(void *ptr,size_t size){
  ptr=realloc(ptr,size);
  if(!ptr && size){
    oom_error_call();
    exit(EXIT_FAILURE);
  } else {
    return ptr;
  }
}
static inline void* xrecalloc(void *ptr,size_t old_size,size_t size){
  ptr=xrealloc(ptr,size);
  if(size>old_size){
    memset(ptr+old_size,(size-old_size),'\0');
  }
  return ptr;
} 
enum alarmd_action{
  add_action,
  clear_action,
  delete_action,
  kill_action,
  list_action,
  modify_action,
  remove_action,
  snooze_action,
  stop_action,
};
#define xfree free 
#define ALARM_REPEATS(alarm) (alarm->repeat & 0x80)
#define ALARM_MONDAY(alarm) (alarm->repeat & 0x40)
#define ALARM_TUESDAY(alarm) (alarm->repeat & 0x20)
#define ALARM_WEDNESDAY(alarm) (alarm->repeat & 0x10)
#define ALARM_THURSDAY(alarm) (alarm->repeat & 0x08)
#define ALARM_FRIDAY(alarm) (alarm->repeat & 0x40)
#define ALARM_SATURDAY(alarm) (alarm->repeat & 0x20)
#define ALARM_SUNDAY(alarm) (alarm->repeat & 0x10)
#define MONDAY 0x40
#define TUESDAY 0x20
#define WEDNESDAY 0x10
#define THURSDAY 0x08
#define FRIDAY 0x04
#define SATURDAY 0x02
#define SUNDAY 0x01
#define WEEKDAY MONDAY | TUESDAY | WEDNESDAY | THURSDAY | FRIDAY
#define WEEKEND SATURDAY | SUNDAY
#define SOCK_NAME "alarmd_socket"
#define PID_FILE "alarmd_pid"
#define DIR_NAME "/var/run/alarmd"
#define REPEAT_OPT "-loop 001"
#define SOCK_FILENAME DIR_NAME SOCK_NAME
static const char *driver="-ao alsa,";
//just took this from the libc manual
static int make_alarm_socket
(const char *filename,int mode){
  int sock,n;
  n=sd_listen_fds(0);//see if systemd will give us a socket
  if(n > 1){
    fprintf(stderr,"Too many file descriptors recieved by systemd");
    exit(EXIT_FAILURE);
  } else if(n==1){
    return SD_LISTEN_FDS_START;
  }//else {
  size_t size;
  struct sockaddr_un name;
  /* Create the socket. */
  sock = socket (PF_LOCAL, SOCK_STREAM, 0);
  if (sock < 0){
    perror ("socket");
    exit (EXIT_FAILURE);
  }  
  /* Bind a name to the socket. */
  name.sun_family = AF_LOCAL;
  strncpy (name.sun_path, filename, sizeof (name.sun_path));
  name.sun_path[sizeof (name.sun_path) - 1] = '\0';  
  /* The size of the address is
     the offset of the start of the filename,
     plus its length (not including the terminating null byte).
     Alternatively you can just do:
     size = SUN_LEN (&name);
  */
  size = (offsetof (struct sockaddr_un, sun_path)
          + strlen (name.sun_path));
  switch(mode){
    case 0:
      if (bind (sock, (struct sockaddr *) &name, size) < 0){
        perror ("bind");
        exit (EXIT_FAILURE);
      }  
      return sock;
    case 1:
      if (connect (sock, (struct sockaddr *) &name, size) < 0){
        perror ("bind");
        exit (EXIT_FAILURE);
      }  
      return sock;
  }
}
struct args {
  char **args;
  uint32_t num_args;
};
#endif
