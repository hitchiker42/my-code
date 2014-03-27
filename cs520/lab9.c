#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <alloca.h>
#include <unistd.h>
#include <string.h>
#include <asm/unistd.h>
int syscall(int number,...);
#define file_size(fd)                           \
  ({off_t len=lseek(fd,0,SEEK_END);             \
    lseek(fd,0,SEEK_SET);                       \
    len;})
void *thread_main(void *filename_void){
  #ifdef DEBUG
  fprintf(stderr,"Thread number %ul\n",syscall(__NR_gettid));
  #endif
  char *filename=filename_void;
  int fd=open(filename,O_RDONLY);
  if(fd == -1){
    fprintf(stderr,"Error opening file %s\n",filename);
    exit(1);
  }
  off_t len=file_size(fd);
  uint8_t *buf=alloca(len+1);
  buf[len]='\0';
  ssize_t nbytes=read(fd,buf,len);
  if(nbytes == (ssize_t)-1 /*|| nbytes != len*/){
    fprintf(stderr,"Error reading from file %s\n",filename);
    pthread_exit(NULL);
  }
  if(close(fd) == -1){
    fprintf(stderr,"Error closing file %s\n",filename);
    pthread_exit(NULL);
  }
  int count=0;
  char *temp;
  while(temp=strchr(buf,'\n')){
    buf=temp+1;
    count++;
  }
  printf("%s %d\n",filename,count);
  return NULL;
}
int main(int argc,char *argv[]){
  if(argc==1){
    fprintf(stderr,"Error no filenames given\n");
    return 1;
  }
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
  pthread_t *threads=alloca(sizeof(pthread_t)*argc);
  int i;   
  for(i=1;i<argc;i++){
    pthread_create(threads+(i-1),&attr,thread_main,argv[i]);
  }
  pthread_exit(NULL);
}
    

