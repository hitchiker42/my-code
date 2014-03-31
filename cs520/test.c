#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#define builtin_unlikely(expr) __builtin_expect((expr),0)
char *read_full_file(char *filename){
  static struct stat stat_buf;
  long fd=open(filename,O_RDONLY);
  if(builtin_unlikely(fd == -1)){
    perror("error opening file");
    exit(1);
  }
  int stat_retval=fstat(fd,&stat_buf);
  if(builtin_unlikely(stat_retval == (off_t)-1)){
    perror("error calling stat on file");
    exit(1);
  }
  uint8_t *buf=malloc(stat_buf.st_size);

  ssize_t nbytes=read(fd,buf,stat_buf.st_size);
  if(builtin_unlikely(nbytes == (ssize_t)-1)){
    perror("error reading from file");
    exit(EXIT_FAILURE);
  }
  if(builtin_unlikely(close(fd) == -1)){
    perror("error closing file");
    exit(EXIT_FAILURE);
  }
  uint32_t end=stat_buf.st_size-1;
  fprintf(stderr,"last characters are %#0hhx and %#0hhx\n",buf[end],buf[end+1]);
  return buf;
}
int main(int argc, char *argv[]){
  read_full_file(argv[1]);
  return 0;
} 
