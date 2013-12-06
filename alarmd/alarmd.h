#ifndef _ALARM_D
#define _ALARM_D
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/socket.h>
#include <stdint.h>
#include <sys/un.h>
#include <time.h>
#include <string.h>
#if !(defined(NDEBUG))
#define PRINT_MSG(msg) fprintf(stderr,msg "\n")
#define PRINT_FMT(msg,fmt...) fprintf(stderr,msg"\n",##fmt)
#else
#define PRINT_MSG(msg)
#define PRINT_FMT(msg,fmt...)
#endif
typedef struct my_alarm my_alarm;
typedef struct my_string my_string;
pthread_mutex_t alarm_lock=PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t alarm_cond=PTHREAD_COND_INITIALIZER;
pthread_attr_t detatched_attr;
pid_t command_process;
pthread_t command_thread;
pthread_t alarm_loop_thread;
pthread_t main_thread;
struct my_alarm {
  time_t alarm_time;
  char *command;
  uint_32 command_len;
  uint_8 repeat;//make sure to set this to 1 by default
  //because 0 is infinite loop, and thats something kinda useful
  uint_8 async;
  uint_8 music;
};
//this really doesn't need to be super efficent so malloc=calloc
static inline void* xmalloc(size_t size){
  void *temp=calloc(size,sizeof(char)));
  if(!temp && size){
    fprintf(stderr,"Error virtual memory exhausted\n");
    exit(EXIT_FAILURE);
  } else {
    return temp;
  }
}
static inline void* xrealloc(void *ptr,size_t size){
  ptr=realloc(ptr,size);
  if(!ptr && size){
    fprintf(stderr,"Error virtual memory exhausted\n");
    exit(EXIT_FAILURE);
  } else {
    return ptr;
  }
}
static inline xrecalloc(void *ptr,size_t old_size,size_t size){
  ptr=xrealloc(ptr,size);
  if(size>old_size){
    memset(ptr+old_size,(size-old_size),'\0');
  }
  return ptr;
} 
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
my_alarm *alarm_queue;
uint32_t queue_length;
uint32_t queue_size;
time_t next_alarm;
struct timespec wait_time = {.tv_sec=2,.tv_nsec=0};
struct timespec wait_time2 = {.tv_sec=0,.tv_nsec=1e6};
int make_alarm_socket(my_string filename);
int alarm_socket;
static const char *sock_name="alarmd_socket";
#define SOCK_NAME "alarmd_socket"
static const char *dir_name="/var/run/alarmd";
static const char *mplayer="/usr/bin/mplayer";
#define DIR_NAME "/var/run/alarmd"
#define REPEAT_OPT "-loop 001"
char repeat_opt[10];
const char *driver="-ao alsa,";

enum bind_or_connect {
  _bind,
  _connect,
};
//just took this from the libc manual
static int make_alarm_socket (const char *filename,enum bind_or_connect mode) {
  int sock;
  size_t size;
  /* Create the socket. */
  sock = socket (PF_LOCAL, SOCK_STREAM, 0);
  if (sock < 0){
    perror ("socket");
    exit (EXIT_FAILURE);
  }  
  /* Bind a name to the socket. */
  name.sun_family = AF_LOCAL;
  strncpy (sock_name.sun_path, filename, sizeof (sock_name.sun_path));
  sock_name.sun_path[sizeof (sock_name.sun_path) - 1] = '\0';  
  /* The size of the address is
     the offset of the start of the filename,
     plus its length (not including the terminating null byte).
     Alternatively you can just do:
     size = SUN_LEN (&name);
  */
  size = (offsetof (struct sockaddr_un, sun_path)
          + strlen (sock_name.sun_path));
  switch(mode){
    case _bind:
      if (bind (sock, (struct sockaddr *) &sock_name, size) < 0){
        perror ("bind");
        exit (EXIT_FAILURE);
      }  
      return sock;
    case _connect:
      if (connect (sock, (struct sockaddr *) &sock_name, size) < 0){
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
static struct args string_split(char *str,char *delim){
  if(NULL==delim){
    delim=" \t\n";
  }
  
#endif
