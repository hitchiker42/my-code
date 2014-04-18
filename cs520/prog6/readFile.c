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
stream_handle create_stream(const char *,const char *,
                            produce_fn ,init_fn,event_loop_handle);
int start_stream(stream_handle,void*);
void cleanup_stream(stream_handle);
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
#define NDEBUG
#if (defined DEBUG) && !(defined NDEBUG)
#define HERE() fprintf(stderr,"here at %s,line %d\n",__FILE__,__LINE__)
#define PRINT_MSG(string) fprintf(stderr,string);
#define PRINT_FMT(string,fmt...) fprintf(stderr,string,##fmt);
#define PRINT_LN(string) fprintf(stderr,"%s%s",string,"\n")
#else
#define HERE()
#define PRINT_MSG(string)
#define PRINT_FMT(string,fmt...)
#define PRINT_LN()
#endif
typedef struct read_file_data read_file_data;
struct read_file_data {
  void *mmaped_file;
  void *stream;
  void *event_loop;
  uint8_t *mem_ptr;
  uint8_t *mem_ptr_last;
  off_t file_len;
  off_t file_offset;
  ssize_t block_size;
  char *filename;
  int32_t newline_count;
};
void *read_file_produce(void *client_data){
  read_file_data *data=(read_file_data*)(client_data);
  if(data->file_offset>=data->file_len){
    return NULL;
  }
  HERE();
  data->mem_ptr=data->mmaped_file+data->file_offset;
  data->mem_ptr_last=data->mem_ptr;
  data->block_size=MAX(data->file_len-data->file_offset,1000);
  data->file_offset+=data->block_size;
  return data;
}
void process_data(void *client_data){
  read_file_data *data=(read_file_data*)(client_data);
  while(data->block_size>0){
    data->mem_ptr=memchr(data->mem_ptr,'\n',data->block_size);
    if(!data->mem_ptr){
      break;
    }
    data->mem_ptr++;
    data->block_size-=(data->mem_ptr-data->mem_ptr_last);
    data->mem_ptr_last=data->mem_ptr;
    data->newline_count++;
  }
}
void read_file_end(void *client_data){
  HERE();
  read_file_data *data=(read_file_data*)(client_data);
  printf("Total newline count: %d\n",data->newline_count);
  //valgrind won't complain if this next line is ommited, because it
  //doesn't check calls to mmap/munmap
  munmap(data->mmaped_file,data->file_len);
  stop_event_loop(data->event_loop);
  cleanup_stream(data->stream);
  return;
}
//should this use return instead of exit?
void *read_file_init(void *client_data){
  HERE();
  read_file_data *data=(read_file_data*)(client_data);
  int fd;
  if((fd=open(data->filename,O_RDONLY))==-1){
    perror("open failed");
    exit(1);
  }
  data->file_len=file_size(fd);
  //it doesn't really matter if the flags argument is private or shared here
  if((data->mmaped_file =
      mmap(NULL,data->file_len,PROT_READ,MAP_PRIVATE,fd,0))==MAP_FAILED){
    perror("mmap failed");
    exit(1);
  }
  if(close(fd)==-1){
    perror("close failed");
    exit(1);
  }
  return data;
}
static void stream_main(read_file_data *data){
  stream_handle stream;
  if((stream=create_stream("data","end",read_file_produce,
                           read_file_init,data->event_loop))==NULL){
    fprintf(stderr, "failed to start stream\n");
    exit(1);
  }
  data->stream=stream;
  register_event(data->event_loop,"data",process_data);
  register_event(data->event_loop,"end",read_file_end);
  start_stream(stream,data);
}

int main(int argc,char *argv[]){
  if(argc < 2){
    fprintf(stderr,"Error no file name given\n");
    return 1;
  } else if (argc > 2){
    fprintf(stderr,"Error more than one file name given\n");
    return 1;
  }
  event_loop_handle event_loop=create_event_loop();
  read_file_data *data=alloca(sizeof(read_file_data));//allocated on the stack
  memset(data,'\0',sizeof(read_file_data));
  if(event_loop==NULL){
    fprintf(stderr,"create_event_loop failed\n");
    exit(1);
  }
  data->event_loop=event_loop;
  data->filename=argv[1];
  start_event_loop(event_loop,(handler_fn)stream_main,data);
  cleanup_event_loop(event_loop);
  //I didn't actually malloc anything so I don't have anything to cleanup
  //here
  return 0;
}
