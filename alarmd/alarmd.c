/*code for the alarmd daemon and a simple priority queue*/
#include "alarmd.h"
//callers are expected to hold a lock before calling functions
//which act on the alarm queue
static int stat_loc;
void kill_command_thread(int signo){
  pthread_kill(command_thread,SIGTERM);
  if(pthread_timedjoin_np(command_thread,NULL,&wait_time)){
    pthread_kill(command_thread,SIGKILL);
  }
}
void kill_current_command(int signo){
  if(command_process <= 0){
    return;
  }
  kill(command_process,SIGTERM);
  sleep(1);
  int i;
  if(!waitpid(command_process,&stat_loc,WNOHANG)){
    kill(command_process,SIGKILL);
  } else {
    return;
  }
  waitpid(command_process,&stat_loc,0);
  return;
}
struct sigaction sigusr_act ={.sa_handler=kill_current_command};
struct sigaction sigterm_act ={.sa_handler=kill_current_command};
void* run_alarm_command(void *data){
  sigaction(SIGTERM,&sigterm_act,NULL);
  //make a local copy so we can free the heap
  //allocated one and not worry about having to clean it up
  my_alarm cur_alarm=*(my_alarm*)data;
  xfree(data);
  //something...maybe
  command_process=fork();
  if(command_process==0){
    //we're the new process
    if(cur_alarm.music){
      //assume cur_alarm->command is a song title, run it w/mplayer
      //I'll leave setting the path of the file to the frontend;
      if(cur_alarm.repeat != 1){
        if(cur_alarm.repeat < 10){
          repeat_opt[9]=cur_alarm.repeat+0x30;
        } else if(cur_alarm.repeat < 100){
          //this is kinda tricky, I'll need to look it up
        } else {
        }
      }
      execl(mplayer,mplayer,cur_alarm.command,repeat_opt,driver,NULL);
      //error handling goes here
    } else {//execute some other command, to ba added
    }
  } else {
    //parent process, wait for child to complete, or for a signal
    waitpid(command_process,&stat_loc,0);
    return NULL;
  }
}


void* alarm_loop(void* data){
  sigaction(SIGUSR1,&sigusr_act,NULL);
  while(1){
    pthread_mutex_lock(&alarm_lock);
    while(!queue_length){
      pthread_cond_wait(&alarm_cond,&alarm_lock);
    }
    if(time(NULL)>=next_alarm){
      my_alarm *cur_alarm=alarm_heap_pop();
      pthread_mutex_unlock(&alarm_lock);

      if(cur_alarm->async){
        pthread_create(&command_thread,&detached_attr,run_alarm_command,NULL);
        continue;
      } else {
        pthread_create(&command_thread,NULL,run_alarm_command,(void*)cur_alarm);
        pthread_join(command_thread,NULL);
        continue;
      }
    } else {
      pthread_mutex_unlock(&alarm_lock);
    }
    sleep(60);//Now wait just a minute...
    //bad joke
  }
}
void alarmd_cleanup(){
  close(alarm_socket);
  unlink(SOCK_NAME DIR_NAME);
  rmdir(DIR_NAME);
}
//this may not work statically
struct sigaction term_cleanup={.sa_handler=alarmd_cleanup};
int main(int argc,char *argv[]){
  if(atexit(alarmd_cleanup)){
    fprintf(stderr,"failed to setup cleanup function,exiting");
    exit(1);
  }
  if(sigaction(SIGTERM,&term_cleanup,NULL)){
    perror("sigaction failure");
  }
  pid_t alarmd_pid=fork();
  if(alarmd_pid<0){
    exit(EXIT_FAILURE);
  }
  if(alarmd_pid>0){
    exit(EXIT_SUCCESS);
  }
  pthread_attr_setdetachstate(&detached_attr,PTHREAD_CREATE_DETACHED);
  //child process, aka daemon
  pid_t sid=setsid();
  umask(0);
  PRINT_FMT("daemonn pid = %d",getpid());
  if(chdir("/")){
    perror("chdir failure");
    exit(EXIT_FAILURE);
  }
  close(STDIN_FILENO);
  close(STDOUT_FILENO);
  close(STDERR_FILENO);
}
