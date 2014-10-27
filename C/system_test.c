#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <signal.h>
#include <unistd.h>
void *run_yes(){
  system("yes");
  return 0;
}
int main(){
  pthread_t new_thread;
  pthread_create(&new_thread,NULL,run_yes,NULL);
  sleep(1);
  pthread_kill(new_thread,SIGTERM);
  pthread_join(new_thread,NULL);
  return 0;
}
