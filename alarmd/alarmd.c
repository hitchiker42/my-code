/*code for the alarmd daemon and a simple priority queue*/
#include "alarmd.h"
#ifdef USE_SYSTEMD
#define syslog(priority,format,args...)         \
  fprintf(stderr,priority format,##args)
#else
#include <syslog.h>
#endif
#undef oom_error_call
#define oom_error_call() syslog(LOG_ERR,"Error virtual memory exhausted")
#define read_size_error(size,type)                              \
  if(size>sizeof(type)){                                            \
    syslog(LOG_ERR,"Read error, size of data read was greater than expected"); \
  } else {                                                              \
    syslog(LOG_ERR,"Read error, size of data read was less than expected"); \
  }
//callers are expected to hold a lock before calling functions
//which act on the alarm queue
static int stat_loc;
void kill_command_thread(int signo);
void kill_current_command();
void alarmd_cleanup();
struct sigaction sigusr_act ={.sa_handler=kill_current_command};
struct sigaction command_sigterm_act ={.sa_handler=kill_current_command};
struct sigaction alarm_loop_sigterm_act ={.sa_handler=kill_command_thread};
struct sigaction term_cleanup={.sa_handler=alarmd_cleanup};
static pthread_t command_thread;
void kill_command_thread(int signo){
  pthread_kill(command_thread,SIGTERM);
  if(pthread_timedjoin_np(command_thread,NULL,&wait_time)){
    pthread_kill(command_thread,SIGKILL);
  }
  pthread_join(command_thread,NULL);
  return;
}
void kill_current_command(){
  if(command_process <= 0){
    return;
  }
  kill(command_process,SIGTERM);
  nanosleep(&nano_wait_time,NULL);
  if(!waitpid(command_process,&stat_loc,WNOHANG)){
    nanosleep(&nano_wait_time2,NULL);
    if(!waitpid(command_process,&stat_loc,WNOHANG)){
      syslog(LOG_WARNING,"Child process %d not responding, sending SIGKILL",command_process);
      kill(command_process,SIGKILL);
    } else {
      return;
    }
  } else {
    return;
  }
  waitpid(command_process,&stat_loc,0);
  return;
}
void* run_alarm_command(void *data){
  sigaction(SIGTERM,&command_sigterm_act,NULL);
  //make a local copy so we can free the heap
  //allocated one and not worry about having to clean it up
  my_alarm *cur_alarm=(my_alarm*)data;
  xfree(data);
  //something...maybe
  command_process=fork();
  if(command_process==0){
    //we're the new process
    if(cur_alarm->music){
      //assume cur_alarm->command is a song title, run it w/mplayer
      //I'll leave setting the path of the file to the frontend;
      if(cur_alarm->repeat != 1){
        if(cur_alarm->repeat < 10){
          repeat_opt[9]=cur_alarm->repeat+0x30;
        } else if(cur_alarm->repeat < 100){
          //this is kinda tricky, I'll need to look it up
        } else {
        }
      }
      execl(mplayer,mplayer,cur_alarm->command,repeat_opt,driver,NULL);
      //error handling goes here
    } else {//execute some other command, to ba added
      execl(DIR_NAME "/" cur_alarm->command,NULL);
    }
  } else {
    //parent process, wait for child to complete, or for a signal
    waitpid(command_process,&stat_loc,0);
    return NULL;
  }
}
void* alarm_loop(void* data){
  sigaction(SIGUSR1,&sigusr_act,NULL);
  sigaction(SIGTERM,&sigusr_act,NULL);//make sure we don't leave any zombie processes
  while(1){
    pthread_mutex_lock(&alarm_lock);
    while(!queue_length){
      pthread_cond_wait(&alarm_cond,&alarm_lock);
    }
    if(time(NULL)>=next_alarm){
      my_alarm *cur_alarm=alarm_heap_pop();
      if(cur_alarm->repeat){
        //some how add the alarm back with a new date
      }
      pthread_mutex_unlock(&alarm_lock);
      /*      if(cur_alarm->async){//to bo added
        pthread_t detached_thread;
        pthread_create(&detached_thread,&detached_attr,run_alarm_command,NULL);
        continue;
        } else {*/
        pthread_create(&command_thread,NULL,run_alarm_command,(void*)cur_alarm);
        pthread_join(command_thread,NULL);
        continue;
        //      }
    } else {
      pthread_mutex_unlock(&alarm_lock);
    }
    sleep(60);
  }
}
void alarmd_cleanup(){
  close(alarm_socket);
  unlink(SOCK_NAME DIR_NAME);
  rmdir(DIR_NAME);
  syslog(LOG_NOTICE,"alarmd exiting");
}
void main_loop(){
  int sock=make_alarm_socket(SOCK_NAME,0);
  if(listen(sock,2)){
    char *err_str=strerror_r(errno,NULL,0);
    syslog(LOG_WARNING,"Error in listen, error was %s",err_str);
    exit(EXIT_FAILURE);
  }
  pthread_t alarm_thread;
  pthread_create(&alarm_thread,NULL,alarm_loop,NULL);
  struct sockaddr_un addr;
  socklen_t len;
  int accept_sock;
  alarmd_action action;
  pid_t client_pid=0;
  ssize_t size;
  while(1){
    if((accept_sock=accept(sock,(struct sockaddr*)&addr,&len))<0){
      char *err_str=strerror_r(errno,NULL,0);
      syslog(LOG_WARNING,"Error in accept, error was %s",err_str);
      //not sure what to do here, for now just exit
      //but preferably the program should continue
      exit(EXIT_FAILURE);
    }
    //get client pid first so we can let the user know of errors
    //via signals in the rest of the program
    if(read(accept_sock,&client_pid,sizeof(pid_t))<=0){
      syslog(LOG_ERR,"Error reading client pid, %m. Alarmd exiting");
      exit(EXIT_FAILURE);
    }
    if((size=read(accept_sock,&action,sizeof(alarmd_action)))>0){
      if(size != sizeof(alarmd_action)){
        //not sure what to do here, not sure how I would get here anyway
        read_size_error(size,alarmd_action);
        exit(EXIT_FAILURE);
      }
      switch(action){
        case add_action:{
          my_alarm *new_alarm=xmalloc(sizeof(my_alarm));
          if((size=read(accept_sock,new_alarm,sizeof(my_alarm)))>0){
            if(size != sizeof(my_alarm)){
              //some kind of error recovery, but to be safe exit for now
              read_size_error(size,my_alarm);
              exit(EXIT_FAILURE);
            }
            pthread_mutex_lock(&alarm_lock);
            alarm_heap_add(new_alarm);
            pthread_mutex_unlock(&alarm_lock);
            break;
          }
          goto READ_FAILURE;
        }
        case delete_action:{
          uint32_t alarm_id;
          if((size=read(accept_sock,&alarm_id,sizeof(uint32_t)))>0){
            if(size != sizeof(uint32_t)){
              exit(EXIT_FAILURE);//add recovery code
            }
            pthread_mutex_lock(&alarm_lock);
            int deleted=alarm_heap_try_delete(alarm_id);
            pthread_mutex_unlock(&alarm_lock);
            if(deleted<0){
              //I'm not sure how to best return to the front end(but I'll use an int for now)
              syslog(LOG_WARNING,"Could not delete alarm %d, no such alarm\n",alarm_id);
              write(accept_sock,&deleted,sizeof(int));
            }
          } else {
            goto READ_FAILURE;
          }
        }
        case kill_action:
          pthread_kill(alarm_thread,SIGTERM);
          if(pthread_timedjoin_np(alarm_thread,NULL,&wait_time)){
            pthread_kill(alarm_thread,SIGKILL);
          }
          pthread_join(alarm_thread,NULL);
          exit(EXIT_SUCCESS);
        case clear_action:
          pthread_mutex_lock(&alarm_lock);
          queue_length=0;
          //I suppose this is enough, there's no really reason to zero out
          //the memory for alarm_queue
        case stop_action:
          pthread_kill(alarm_thread,SIGUSR1);
        case snooze_action:
        case modify_action:
        case list_action:{
          char *alarm_list_str;
          int len=alarm_heap_list(&alarm_list_str);
          if(len){
            write(sock,alarm_list_str,len);
          }
        }
      }
    } else if (size<=0){
    READ_FAILURE:
      perror("read failure");
      //again not sure how to recover from this so exit for now
      exit(EXIT_FAILURE);
    }
  LOOP_END:;
  }
}
int systemd_init_daemon(){
  if(atexit(alarmd_cleanup)){
    fprintf(stderr,"failed to setup cleanup function,exiting");
    exit(1);
  }
  if(sigaction(SIGTERM,&term_cleanup,NULL)){
    perror("sigaction failure");
  }
  pthread_attr_setdetachstate(&detached_attr,PTHREAD_CREATE_DETACHED);
}
int sysv_init_daemon(){
  if(atexit(alarmd_cleanup)){
    fprintf(stderr,"failed to setup cleanup function,exiting");
    exit(1);
  }
  if(sigaction(SIGTERM,&term_cleanup,NULL)){
    perror("sigaction failure");
  }
  //  openlog(NULL,0,LOG_CRON);
  //need to remove this for systemd
  daemon(0,0);//library function to daemonize, fork exit parent,
  //redirect stdin,out,err to /dev/null and chdir to /
  pthread_attr_setdetachstate(&detached_attr,PTHREAD_CREATE_DETACHED);
  //child process, aka daemon
  pid_t sid=setsid();
  umask(0);
  PRINT_FMT("daemon pid = %d",getpid());
  //write permission only for the user who ran the daemon, this should be
  //fine since the same process will be doing the deletion
  if(mkdir(DIR_NAME,S_IRWXU|S_IXGRP|S_IRGRP|S_IROTH|S_IXOTH|S_ISVTX)){
    perror("mkdir failure");
    exit(EXIT_FAILURE);
  }
  if(chdir(DIR_NAME)){
    perror("chdir failure");
    exit(EXIT_FAILURE);
  }
  FILE* pid_file=fopen("alarmd.pid","w");
  fprintf(pid_file,"%d\n",getpid());
  fclose(pid_file);
  openlog("alarmd",LOG_PID,LOG_CRON);
  main_loop();
}
#endif
int main(int argc,char *argv[]){
  init_daemon();
}
