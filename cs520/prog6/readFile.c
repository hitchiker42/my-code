//given a file name make a stream that reads the file
//1000 bytes at a time counting the newlines and keeping a global
//count
#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
typedef struct event_loop_data *event_loop_handle;
typedef void (*handler_fn)(void *);
typedef void*(produce_fn)(void*);
typedef void*(init_fn)(void*);
typedef struct stream_data *stream_handle;
event_loop_handle create_event_loop();
void cleanup_event_loop(event_loop_handle);
void stop_event_loop(event_loop_handle);
void announce_event(event_loop_handle,char *,void *);
void register_event(event_loop_handle , const char *, handler_fn);
void start_event_loop(event_loop_handle , handler_fn , void*);
event_loop_handle create_event_loop();
stream_handle create_stream(const char *,const char *,
                            produce_fn ,init_fn,event_loop_handle);
stream_handle create_stream(const char*,const char*,produce_fn,init_fn,
                            event_loop_handle);
int start_stream(stream_handle,void*);
void cleanup_stream(stream_handle);
static void *xmalloc (size_t n){
  void *p = malloc (n);
  if (!p && n != 0){
    perror("out of memory");
    exit(1);
  }
  return p;
}

/* Change the size of an allocated block of memory P to N bytes,
   with error checking.  */
/*
static void *xrealloc (void *p, size_t n){
  if (!n && p){
    free (p);
    return NULL;
  }
  p = realloc (p, n);
  if (!p && n){
    perror("out of memory");
    exit(1);
  }
  return p;
}
*/
static void *xcalloc (size_t s){
  return memset(xmalloc (s), 0, s);
}
static void *xmemdup (void const *p, size_t s){
  return memcpy (xmalloc (s), p, s);
}

/* Clone STRING.  */

static char *xstrdup (char const *string){
  return xmemdup (string, strlen (string) + 1);
}
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
  cleanup_stream(handle);
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
  return data;
}
int main(int argc,char *argv[]){
  if(argc < 2){
    fprintf(stderr,"Error no file names given\n");
    return 1;
  }
  event_loop_handle event_loop=create_event_loop();
  read_file_data *data=alloca(sizeof(read_file_data));
  
  if(event_loop==NULL){
    fprintf(stderr,"create_event_loop failed\n");
    exit(1);
  }
  start_event_loop(event_loop,read_file_init,data);
}
