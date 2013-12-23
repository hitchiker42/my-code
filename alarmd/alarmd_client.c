#include "alarmd.h"
#include "regex.h"
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
static sigset_t *set;
static const int wait_time=20;
static struct option global_opts;
static struct option add_opts;
static struct option list_opts;
static struct option delete_opts;
static struct option modify_opts;
static struct option snooze_opts;
static void sigusr_handler(int signum){
  if(signum==SIGUSR1){
    fprintf(stderr,"operation completed successfully, exiting\n");
    exit(EXIT_SUCCESS);
  } else if (signum==SIGUSR2){
    fprintf(stderr,"operation failed, daemon will contitue to run, exiting\n");
    exit(EXIT_FAILURE);
  } else if (signum == SIGABRT){
    fprintf(stderr,"operation failed, daemon exited, "
           "view syslog for daemon error message\n");
    exit(EXIT_FAILURE);
  } else if (signum == SIGALRM){
    fprintf(stderr,"Error, no responce from daemon in %d seconds, "
            "exiting client, daemon should be inspected\n",wait_time);
    exit(EXIT_FAILURE);
  }
}
static struct sigaction sigusr_action={.sa_handler=sigusr_handler};
static char *cat_args(int argc,char *argv[]){
  int *arglens=alloca(argc);
  int i,arglen=0;
  for(i=0;i<argc;i++){
    arglens[i]=strlen(argv[i]);
    arglen+=arglens[i];
  }
  char *retval=xmalloc(sizeof(char)*arglen-(argc-1));
  arglen=0;
  for(i=0;i<argc;i++){
    memcpy(retval+arglen,argv[i],arglens[i]);
    arglen=arglens[i];
  }
  retval[arglen+1]='\0';
  return retval;
}
//error checked wrapper around localtime_r;
//returned value needs to be freed when done
static struct tm *my_localtime(time_t cur_time);
static time_t parse_time_re(char *time_str,time_t cur_time);
static time_t parse_time(char *time_str,time_t cur_time);
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
void __attribute__((noreturn)) add_alarm(int argc,char *argv[]){
    time_t cur_time=time(NULL);
    struct tm *today=my_localtime(cur_time);
    char* command="/home/tucker/music/alarm.wav";
    uint32_t command_len,alarm_id;
    uint8_t alarm_today=0,repeat=0,music=1,music_loop=3;
    my_alarm *new_alarm=alloca(sizeof(my_alarm));
    //default alarm parameteres
    //not sure if the command parameter will work now that
    //it's an array
    *new_alarm=(my_alarm){.alarm_time=0;
                          .command=command,.command_len=28,
                          .today=alarm_today,.repeat=repeat,.music=music,
                          .music_loop=music_loop,.alarm_id=alarm_d};
    
    //parse options
    //assign to a variable alarm_time;
    time_t alarm_time=parse_time(argv[optind],cur_time);
    if(!alarm_time){
      fprintf(stderr,"error parsing time\n");
      exit(EXIT_FAILURE);
    } else {
      new_alarm->alarm_time=alarm_time;
    //write _add to socket then write new_alarm
      alarmd_action action=_add;
      write(sock,&action,sizeof(alarmd_action));
      write(sock,new_alarm,sizeof(my_alarm));
      alarm(wait_time);
      pause();
    }
}
void __attribute__((noreturn)) list_alarm(int argc,char *argv[]){
  /*
    -write _list to socket;
    -wait on socket, read an int giving the length
    of the alarm list, set to a varible name len
    -read len characters from socket
    -write to stdout
  */
}
void __attribute__((noreturn)) delete_alarm(int argc,char *argv[]){
  /*
    -figure out how to actually get the alarm id (I suppose I make
    the user give it)
    -write _delete to socket, then write alarm id
    -wait on socket to figure out if we actually deleted anything
   */
}
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
  set=xmalloc(sizeof(sigset_t));
  sigemptyset(&set);
  sigaction(SIGUSR1,&sigusr_action,NULL);
  sigaction(SIGUSR2,&sigusr_action,NULL);
  sigaction(SIGABRT,&sigusr_action,NULL);
  sigaction(SIGALRM,&sigusr_action,NULL);
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
#undef mk_case
/*static struct option global_opts;
static struct option add_opts;
static struct option list_opts;
static struct option delete_opts;
static struct option modify_opts;
static struct option snooze_opts;*/
 global_opts={{"help",no_argument,0,'h'},
              {0,0,0,0}};
static uint8_t parse_day_abbr(char *day_str){
  uint8_t retval=0x80;//assuming this only gets called on repeating alarms
  while(day_str){
    switch(day_str[1]){
      case "m":
        if(!strncmp(day_str,"mon",3)){
          retval&=0x40;
          break;
        } else {
          goto PARSE_ERROR;
        }
      case "t":
        if(!strncmp(day_str,"tue",3)){
          retval&=0x20;
          break;
        } else if(!strncmp(day_str,"thu",3)){
          retval&=0x08;
          break;
        } else {
          goto PARSE_ERROR;
        }
      case "w":
        if(!strncmp(day_str,"wed",3)){
          retval&=0x10;
          break;
        } else {
          goto PARSE_ERROR;
        }
      case "f":
        if(!strncmp(day_str,"fri",3)){
          retval&=0x04;
          break;
        } else {
          goto PARSE_ERROR;
        }
      case "s":
        if(!strncmp(day_str,"sat",3)){
          retval&=0x2;
          break;
        } else if (!strncmp(day_str,"sun",3)){
          retval&=0x1;
          break;
        } else {
          goto PARSE_ERROR;
        }
    }
    if(day_str[4] != ','){
      if(day_str[4]== '\0'){
        return retval;
      } else {
      PARSE_ERROR:
        fprintf(stderr,"maleformed repeat string, exiting\n");
        exit(EXIT_FAILURE);
      }
    } else {
      day_str=day_str+5;
    }
  }
}
#define SEC_DAY 86400
#define EST_OFFSET (-18000)
#define MINUTES(time) (time / 60)
#define HOURS(time) (time / 3600)
#define DAYS(time) (time / SEC_DAY)
#define EST(time) (time + EST_OFFSET)
#define MIN_TO_SEC(minutes) (minutes * 60)
#define HOUR_TO_SEC(hours) (hours * 3600)
//I probably could just use the normal localtime function...
static struct tm *my_localtime(time_t cur_time){
  struct tm *today=xmalloc(sizeof struct tm);
  struct tm *retval=localtime_r(&cur_time,today);
  if(!retval){
    free(today);
    return NULL:
  } else {
    return retval;
  }
}
//expects a string of the form of
//+?[0-9][0-9]?(:[0-9][0-9]?)?
//...why didn't I just use a regex
//
static time_t parse_time(char *time_str,time_t cur_time){
  long hours;
  long minutes;
  char *cur_str;
  int relative;
  if(time_str[0]=='+'){
    cur_str=time_str+1;
    relative=1;
  } else {
    cur_str=time_str;
    relative=0;
  }
  if(!cur_str[1]){
    return 0;
  } else if(!cur_str[2]){//1-9 hours from now
    hours=cur_str[1]-48;
    minutes=0;
  } else if (!time_str[3]){//10-99 hours from now
    hours=strtol(time_str,NULL,10);
    minutes=0;
  } else {
    if(time_str[3]==':'){
      hours=time_str[2]+48;
      cur_str=cur_str+4;
    } else if(time_str[4]==':'){
      hours=(time_str[2]+48)+((time_str[3]+48)*10);
      cur_str+cur_str+5;
    } else {
      return 0;
    }
    if(!cur_str[0]){
      return 0;
    } else if(!cur_str[1]){
      return 0;
      minutes=(cur_str[0]+48);
    } else if(!cur_str[2]){
      minutes=strtol(cur_str,NULL,10);
    } else {
      return 0;
    }
  }
  if(relative){
    return (cur_time+HOUR_TO_SEC(hours)+MIN_TO_SEC(minutes));
  } else {
    if(hours>=24 || mintes>=60){
      return 0;
    }
    time_t est_time=EST(cur_time);
    time_t sec_today=est_time % SEC_DAY;
    time_t alarm_time=HOUR_TO_SEC(hours)+MIN_TO_SEC(minutes);
    if(alarm_time<sec_today){
      return est_time+SEC_DAY+alarm_time;
    } else {
      return est_time+alarm_time;
    }
  }
}
//time regex
//\(+\)?\([0-9][0-9]?\)\(:[0-9][0-9]?\)?
static const char *time_re_pattern="\\(+\\)?\\([0-9][0-9]?\\)\\(:[0-9][0-9]?\\)?";
static regex_t time_re;
static struct re_registers time_regs;
//length is 38
static time_t parse_time_re(char *time_str,time_t cur_time){
  if(re_compile_pattern(time_re_pattern,strlen(time_re_pattern),&time_re)){
    return 0;
  }
  int len=strlen(time_str);
  if(re_match(&time_re_pattern,time_str,len,0,&time_regs) != len){
    return 0;
  }
  long hours;
  long minutes;
  int relative;
  if(time_regs.start[1] == time_regs.end[1] == -1){
    relative=0;
  } else {
    relative=1;
  }
  int hours_offset=time_regs.start[2]-time_regs.end[2];
  if(hours_offset>=1){//this is probably unecessary
    hours=(time_str[time_regs.start[2]]-48);
  }
  if(hours_offset==2){
    hours+=((time_str[time_regs.start[2]+1]-48)*10);
  }
  if(time_regs.start[3]==time_regs.end[3]==-1){
    minutes=0;
  } else {
    int minutes_offset=(time_regs.start[3]+1)-time_regs.end[3];
    if(minutes_offset>=1){
      minutes=(time_str[time_regs.start[3]+1]-48);
    }
    if(minutes_offset==2){
      minutes+=((time_str[time_regs.start[3]+2]-48)*10);
    }
  }
  if(relative){
    return (cur_time+HOUR_TO_SEC(hours)+MIN_TO_SEC(minutes));
  } else {
    if(hours>=24 || mintes>=60){
      return 0;
    }
    time_t est_time=EST(cur_time);
    time_t sec_today=est_time % SEC_DAY;
    time_t alarm_time=HOUR_TO_SEC(hours)+MIN_TO_SEC(minutes);
    if(alarm_time<sec_today){
      return est_time+SEC_DAY+alarm_time;
    } else {
      return est_time+alarm_time;
    }
  }
}
