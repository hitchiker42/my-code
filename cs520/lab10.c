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
//the hard way
pthread_mutex_t lock=PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cond=PTHREAD_COND_INITIALIZER;
long threads_left;
long global_count_hard=0;
//the easy way
long global_count_mem=0;
long *global_count=&global_count_mem;
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
  long count=0;
  char *temp;
  while(temp=strchr(buf,'\n')){
    buf=temp+1;
    count++;
  }
  //the easy way
  __atomic_fetch_add(global_count,count,__ATOMIC_SEQ_CST);
  //the hard way
  pthread_mutex_lock(&lock);
  global_count_hard+=count;
  --threads_left;
  pthread_cond_signal(&cond);
  printf("%s %d\n",filename,count);
  pthread_mutex_unlock(&lock);
  return NULL;
}
int main(int argc,char *argv[]){
  if(argc==1){
    fprintf(stderr,"Error no filenames given\n");
    return 1;
  }
  pthread_attr_t default_thread_attr;
  pthread_attr_init(&default_thread_attr);
  pthread_attr_setdetachstate(&default_thread_attr,PTHREAD_CREATE_DETACHED);
  pthread_attr_setstacksize(&default_thread_attr,(2<<15));
    size_t stacksz;
  pthread_attr_getstacksize(&default_thread_attr,&stacksz);
  fprintf(stderr,"Attr stack size = %ld \n",stacksz);
  pthread_t *threads=alloca(sizeof(pthread_t)*argc-1);
  int i;   
  threads_left=argc-1;
  for(i=1;i<argc;i++){
    if(pthread_create(threads+(i-1),&default_thread_attr,thread_main,argv[i])!=0){
      perror("pthread create");
      exit(1);
    }
  }
  //the easy way
  //    for(i=1;i<argc;i++){
  //    void **retval;
  //    pthread_join(threads[i-1],NULL);
  //  }
  //the hard way
  pthread_mutex_lock(&lock);
  while(threads_left>=1){
    pthread_cond_wait(&cond,&lock);
  }
  pthread_mutex_unlock(&lock);
  printf("easy total %d\n",global_count_mem);
  printf("hard total %d\n",global_count_hard);
}
    

