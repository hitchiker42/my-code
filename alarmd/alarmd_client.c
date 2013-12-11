#include "alarmd.h"
#include <getopt.h>
/* User Interface to alarmd daemon
 * Basically multiple programs in one (I suppose I could compile it as multiplle
 * programs but I'm too lazy, maybe I will eventually
 *
 * Takes a command and essentially runs a different program for each command
 * Each command has it's own set of arguments, etc..
 * It's kinda like git (i.e git add,git commit, git clone,etc)
 *
 * Commands Are:
 * add: add a new alarm
 * list: list all alarms (prints a unique id number for each alarm
 * which is used to identify an alarm in other commands
 * delete: delete an alarm specified by it's id number
 * modify: change parameters(time,repeate date,command)
 * snooze: stop currently executing alarm and set it to run x minutes later
 * stop: stop currently executing alarm
 * clear: clear all current alarms
*/
static pid_t daemon_pid;
static int sock;
static struct option global_opts;
static struct option add_opts;
static struct option list_opts;
static struct option delete_opts;
static struct option modify_opts;
static struct option snooze_opts;
#define mk_case(name)                           \
  if(!strcmp(argv[1],#name)){                   \
    name##_alarm(argc-1,argv+1);                \
  } else {                                      \
    fprintf(stderr,"Invalid command %s did you mean %s\n",argv[1],#name);\
    exit(EXIT_FAILURE);                                                 \
  }
void __attribute__((noreturn)) help(){
  exit(EXIT_SUCCESS);
}
void __attribute__((noreturn)) add_alarm(int argc,char *argv[]);
void __attribute__((noreturn)) list_alarm(int argc,char *argv[]);
void __attribute__((noreturn)) delete_alarm(int argc,char *argv[]);
void __attribute__((noreturn)) modify_alarm(int argc,char *argv[]);
void __attribute__((noreturn)) stop_alarm(int argc,char *argv[]){
  alarmd_action action=_stop;
  write(sock,&action,sizeof(alarmd_action));
  exit(EXIT_SUCCESS);
}  
void __attribute__((noreturn)) snooze_alarm(int argc,char *argv[]);
void __attribute__((noreturn)) clear_alarm(int argc,char *argv[]){
  alarmd_action action=_clear;
  write(sock,&action,sizeof(alarmd_action));
  exit(EXIT_SUCCESS);
}
int main(int argc,char *argv[]){
  if(argv[1][0] == '-'){
  }
  sock=make_alarm_socket(SOCK_FILENAME,_connect);
  switch(argv[1][0]){
    case 'a':
      mk_case(add);
    case 'e':
      mk_case(clear);
    case 'd':
      mk_case(delete);
    case 'l':
      mk_case(list);
    case 'm':
      mk_case(modify);
    case 's':
      if(!strcmp(argv[1],"snooze")){
        snooze_alarm(argc-1,argv+1);
      } else if (!strcmp(argv[1],"stop")){
        stop_alarm(argc-1,argv+1);
      } else {
        fprintf(stderr,"Invalid command %s\n",argv[1]);
      }
    default:
      fprintf(stderr,"Invalid command %s\n",argv[1]);
      exit(EXIT_FAILURE);
  }
}
/*static struct option global_opts;
static struct option add_opts;
static struct option list_opts;
static struct option delete_opts;
static struct option modify_opts;
static struct option snooze_opts;*/
 global_opts={{"help",no_argument,0,'h'},
              {0,0,0,0};
