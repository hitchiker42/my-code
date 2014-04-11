//given a file name make a stream that reads the file
//1000 bytes at a time counting the newlines and keeping a global
//count
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#define MAX(a,b)                                \
  ({ __typeof__ (a) _a = (a);                   \
    __typeof__ (b) _b = (b);                    \
    _a > _b ? _a : _b;})
#define MIN(a,b)                                \
  ({ __typeof__ (a) _a = (a);                   \
    __typeof__ (b) _b = (b);                    \
    _a < _b ? _a : _b;})
#define file_size(fd)                           \
  ({struct stat buf;                            \
    fstat(fd,&buf);                             \
    buf.st_size;})
typedef struct read_file_data read_file_data;
//it's a lot easier to use a single static buf 
struct read_file_data {
  //char buf[1024];//make it a power of two size because
  void *mmaped_file;
  off_t file_len;
  off_t file_offset;
  int32_t newline_count;
  int32_t fd;
  char *filename;
}
void *read_file_produce(void *client_data){
  read_file_data *data=(read_file_data*)(client_data);
  uint8_t *mem=data->mmaped_file+file_offset;
  ssize_t size=MAX(data->file_len-data->file_offset,1000);
  while(size>0){
    mem=memchr(mem,'\n',size);
    if(!mem){
      break;
    }
    mem++;
    size-=(mem-last_mem);
    last_mem=mem;
    data->newline_count++;
  }
  data->file_offset+=size;
  if(data->file_offset>=data->file_size){
    return NULL;
  } else {
    return (void*)1;
  }
}
void read_file_end(stream_handle handle){
  munmap(data->mmaped_file,data->file_len);
  xfree(handle);
}
//should this use return instead of exit?
void *read_file_init(void *client_data){
  read_file_data *data=(read_file_data*)(client_data);
  if((data->fd=open(data->filename,O_RDONLY))==-1){
    perror("open failed");
    exit(1);
  }
  data->file_len=file_size(data->fd);
  //it doesn't really matter if the flags argument is private or shared here
  if((data->mmaped_file = 
      mmap(NULL,file_len,PROT_READ,MAP_PRIVATE,data->fd,0))==MAP_FAILED){
    perror("mmap failed");
    exit(1);
  }
  return 1;
}
