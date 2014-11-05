/*
  Utility C routines
*/
#include "util.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <errno.h>
off_t file_len_by_fd(int fd){
  struct stat buf;
  fstat(fd, &buf);
  return buf.st_size;
}
off_t file_len_by_name(const char *filename){
  struct stat buf;
  stat(filename, &buf);
  return buf.st_size;
}
off_t FILE_len(FILE *file){
  off_t pos = ftello(file);//to get back to where we awere
  if(fseeko(file, 0, SEEK_END) == (off_t)-1){
    return (off_t)-1;
  }
  off_t end = ftello(file);
  fseeko(file, pos, SEEK_SET);
  return end;
}
  
int __regular_filep(void *arg, int is_fd){
  struct stat buf;
  if(is_fd){
    fstat((long)arg, &buf);
  } else {
    stat((char *)arg, &buf);
  }
  return S_ISREG(buf.st_mode);
}

struct buffer mmap_file(int fd, int shared){
  size_t len = file_len_by_fd(fd);
  int visibility = (shared ? MAP_SHARED : MAP_PRIVATE);
  void *retval = mmap(NULL, len+1, PROT_READ|PROT_WRITE, visibility,
                      fd, 0);
  if(retval == MAP_FAILED){
    int my_errno = errno;
    perror("mmap");
    errno = my_errno;
    return null_buffer;
  }
  ((char*)retval)[len] = EOF;
  return (struct buffer){.mem = retval, .size = len};
}
void *mmap_anon(size_t sz){
  void *mem = mmap(0, sz, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
  if(mem == MAP_FAILED){
    perror("mmap");
    return NULL;
  }
  return mem;
}
