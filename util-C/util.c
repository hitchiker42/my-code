/*
  Utility C routines
*/
#include "util.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <assert.h>
#include <errno.h>
#ifndef DEBUG_PRINTF
#define DEBUG_PRINTF(fmt,args...)               \
  fprintf(stderr,fmt,##args)
#endif
char * __attribute__((const)) filemode_bits_to_string(int mode){
  if(get_access_mode(mode) == O_RDWR){
    if(mode & O_TRUNC){
      return "w+";
    } else if(mode & O_APPEND){
      return "a+";
    } else {
      return "r+";
    }
  } else if(get_access_mode(mode) == O_WRONLY){
    if(mode & O_APPEND){
      return "a";
    } else {
      return "w";
    }
  } else if(get_access_mode(mode) == O_RDONLY){
    return "r";
  } else {
    return NULL;
  }
}

off_t file_len_by_fd(int fd){
  DEBUG_PRINTF("Calling file_len_by_fd\n");
  struct stat buf;
  fstat(fd, &buf);
  return buf.st_size;
}
off_t file_len_by_name(const char *filename){
  DEBUG_PRINTF("Calling file_len_by_name\n");
  struct stat buf;
  stat(filename, &buf);
  return buf.st_size;
}
off_t file_len_by_FILE(FILE *file){
  DEBUG_PRINTF("Calling file_len_by_FILE\n");
  off_t pos = ftello(file);//to get back to where we awere
  if(fseeko(file, 0, SEEK_END) == (off_t)-1){
    return (off_t)-1;
  }
  off_t end = ftello(file);
  fseeko(file, pos, SEEK_SET);
  return end;
}
int regular_filep_fd(long fd){
  DEBUG_PRINTF("Calling regular_filep_fd\n");
  struct stat buf;
  fstat(fd, &buf);
  return S_ISREG(buf.st_mode);
}
int regular_filep_filename(char *filename){
  DEBUG_PRINTF("Calling regular_filep_filename\n");
  struct stat buf;
  stat((char *)filename, &buf);
  return S_ISREG(buf.st_mode);
}
int regular_filep_FILE(FILE* file){
  DEBUG_PRINTF("Calling regular_filep_FILE\n");
  return regular_filep_fd(fileno(file));
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

void* mmap_file(int fd, int shared, size_t *sz){
  *sz= file_len_by_fd(fd);
  int visibility = (shared ? MAP_SHARED : MAP_PRIVATE);
  void *retval = mmap(NULL, *sz+1, PROT_READ|PROT_WRITE, visibility,
                      fd, 0);
  if(retval == MAP_FAILED){
    perror("mmap");
    return NULL;
  }
  ((char*)retval)[*sz] = EOF;
  return retval;
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
uint32_t strspn_table(const uint8_t *str, const uint8_t accept[256]){
  int i=0;
  
  //accept['\0'] == 0)
  //We assume (or we could force by uncommenting above) that '\0' is not
  //in the accept set, this means we'll always return the length of the
  //string if all characters are in the accept set
  while(1){
    if(!accept[str[i]]){return i;}
    if(!accept[str[i+1]]){return i+1;}
    if(!accept[str[i+2]]){return i+2;}
    if(!accept[str[i+3]]){return i+3;}
    i+=4;
  }
}
uint32_t strcspn_table(const uint8_t *str, const uint8_t reject[256]){
  int i=0;
  reject['\0'] == 1;//force the null byte to be in the reject set
  while(1){
    if(reject[str[i]]){return i;}
    if(reject[str[i+1]]){return i+1;}
    if(reject[str[i+2]]){return i+2;}
    if(reject[str[i+3]]){return i+3;}
    i+=4;
  }
}
uint32_t memspn_table(const uint8_t *str, uint32_t len,
                      const uint8_t accept[256]){
  int i=0;
  //this is for speed, but I'm not sure how much it's worth it
  while(i+4 < len){
    if(!accept[str[i]]){return i;}
    if(!accept[str[i+1]]){return i+1;}
    if(!accept[str[i+2]]){return i+2;}
    if(!accept[str[i+3]]){return i+3;}
    i+=4;
  }
  switch(len-i){
    case 3: if(!accept[str[i]]){return i;} i++;
    case 2: if(!accept[str[i]]){return i;} i++;
    case 1: if(!accept[str[i]]){return i;}
  }
  return len;
}
uint32_t memcspn_table(const uint8_t *str, uint32_t len,
                       const uint8_t reject[256]){
  int i=0;
  while(1+4 < len){
    if(reject[str[i]]){return i;}
    if(reject[str[i+1]]){return i+1;}
    if(reject[str[i+2]]){return i+2;}
    if(reject[str[i+3]]){return i+3;}
    i+=4;
  }
  switch(len-i){
    case 3: if(reject[str[i]]){return i;} i++;
    case 2: if(reject[str[i]]){return i;} i++;
    case 1: if(reject[str[i]]){return i;}
  }
  return len;
}
uint32_t memspn(const uint8_t *buf, uint32_t len,
                const uint8_t *accept, uint32_t len2){
  uint8_t bytes[256] = {0};
  int i;
  for(i = 0; i < len2; i++){
    bytes[accept[i]] = 1;
  }
  return memspn_table(buf, len, bytes);
}
uint32_t memcspn(const uint8_t *buf, uint32_t len,
                 const uint8_t *reject, uint32_t len2){
  uint8_t bytes[256] = {0};
  int i;
  for(i = 0; i < len2; i++){
    bytes[reject[i]] = 1;
  }
  return memcspn_table(buf, len, bytes);
}
struct timespec float_to_timespec(double t){
  struct timespec ts;
    //it'd be best to use trunc here, but this way we avoid linking libm
  ts.tv_sec = (time_t)t;//should truncate sleep_time
  ts.tv_nsec = (t - ts.tv_sec) * 1e9;
  //just in case the hacky truncate failed
  if(ts.tv_nsec < 0){
    ts.tv_nsec = 0;
  }
  return ts;
}
double timespec_to_float(struct timespec t){
  return t.tv_sec + (t.tv_nsec / 1.0e9);
}

double float_sleep(double sleep_time){
  struct timespec ts;
  if(sleep_time < 0){//we can't turn back time...
    return 0;
  }
  ts = float_to_timespec(sleep_time);
  int err = nanosleep(&ts, &ts);
  if(err == -1){
    //if we passed an invalid time it's an issue with this function
    assert(errno != EINVAL);
    return timespec_to_float(ts);
  }
  return 0;
}
void float_sleep_full(double sleep_time){
  struct timespec ts, tr;
  if(sleep_time < 0){//we can't turn back time...
    return;
  }
  ts = float_to_timespec(sleep_time);
  int err;
  do {
    errno = 0;
    err = nanosleep(&ts, &ts);
    assert(errno != EINVAL);
  } while (err == -1);
  return;
}    
//mremap was taken, thus the stupid name, also this assumes
//it's resizing an anonymous map, since that's the only thing
//that makes sense
/* void *re_mmap(void *ptr, size_t old_size, size_t new_size){ */
/*   #ifdef __linux__ */
/*   return mremap(ptr, old_size, new_size, MREMAP_MAYMOVE); */
/*   #else */
/*   void *buf = mmap_anon(new_size); */
/*   memcpy(buf, ptr, old_size); */
/*   munmap(ptr); */
/*   return buf; */
/*   #endif */
/* } */
/*#define SYSTEM_SHELL "bash"
static void *pthread_system_function(void * arg){
  char *command = arg;
  execl("/bin/" SYSTEM_SHELL, SYSTEM_SHELL, "-c", arg, NULL);
  return NULL;
}
*/
/*
+int _regular_file_fd(int fd){
   struct stat buf;
-  if(is_fd){
-    fstat((long)arg, &buf);
-  } else {
-    stat((char *)arg, &buf);
-  }
+  fstat(fd, &buf);
   return S_ISREG(buf.st_mode);
 }
+int _regular_file_filename(char *filename){
+  int fd = open(filename, O_RDONLY);
+  if(fd == -1){
+    perror("open");
+    return -1;
+  }
+  int retval = _regular_file_fd(fd);
+  close(fd);
+  return retval;
+}
+int _regular_file_FILE(FILE *f){
+  int fd = fileno(f);
+  if(fd == -1){
+    perror("fileno");
+    return -1;
+  }
+  return _regular_file_fd(fd);
+}
+void* _mmap_file_fd(int fd, int shared, size_t *sz){
+  *sz= _file_len_fd(fd);
   int visibility = (shared ? MAP_SHARED : MAP_PRIVATE);
   void *retval = mmap(NULL, *sz+1, PROT_READ|PROT_WRITE, visibility,
                       fd, 0);
  *sz= file_len_by_fd(fd);
  if(retval == MAP_FAILED){
    perror("mmap");
    return NULL;
  }
  ((char*)retval)[*sz] = EOF;
  return retval;
+void *_mmap_file_FILE(FILE *f, int shared, size_t *sz){
+  int fd = fileno(f);
+  if(fd == -1){
+    perror("fileno");
+    return NULL;
+  }
+  return _mmap_file_fd(fd, shared, sz);
+}
+void *_mmap_file_filename(char *filename, int shared, size_t *sz){
+  int fd = open(filename, O_RDONLY);
+  if(fd == -1){
+    perror("open");
+    return NULL;
+  }
+  void *buf = _mmap_file_fd(fd, shared, sz);
+  close(fd);
+  return buf;
+}
+#define choose_expr __builtin_choose_expr
+#define types_compatible __builtin_types_compatible_p
+off_t _file_len_fd(int fd);
+off_t _file_len_filename(const char *filename);
+off_t _file_len_FILE(FILE *file);
+    if(__builtin_types_compatible_p(__typeof(x), int)){                   \
+      retval = _file_len_fd(arg);                                       \
+    }  else if(__builtin_types_compatible_p(__typeof(x), long)){          \
+      retval = _file_len_fd(arg);                                       \
+    } else if (__builtin_types_compatible_p(__typeof(x), char*)){         \
+      retval = _file_len_filename(arg);                                 \
+    } else if (__builtin_types_compatible_p(__typeof(x), uint8_t *)){     \
+        retval = _file_len_filename(arg);                               \
+int _regular_file_fd(int fd);
+int _regular_file_filename(char *filname);
+int _regular_file_FILE(FILE *f);
+  ({choose_expr(types_compatible(__typeof(x), int),                     \
+                _regular_file_fd(x),                                    \
+                choose_expr(types_compatible(__typeof(x), long),        \
+                            _regular_file_fd(x),                        \
+                            choose_expr(types_compatible(__typeof(x), __typeof(char*)), \
+                                        _regular_file_filename(x),      \
+                                        choose_expr(types_compatible(__typeof(x), __typeof(uint8_t*)), \
+                                                    _regular_file_filename(x), \
+                                                    -1))));})
+void *_mmap_file_fd(int fd, int shared, size_t *sz);
+void *_mmap_file_FILE(FILE *f, int shared, size_t *sz);
+void *_mmap_file_filename(char *filename, int shared, size_t *sz);
+#define mmap_file(x, shared, sz)                                        \
+  ({choose_expr(types_compatible(__typeof(x), long) ||                  \
+                types_compatible(__typeof(x), int),                     \
+                _mmap_file_fd(x, shared, sz),                           \
+                choose_expr(types_compatible(__typeof(x), char*) ||     \
+                            types_compatible(__typeof(x), uint8_t*),    \
+                            _mmap_file_filename((char*)x, shared, sz),  \
+                            NULL));})
 /*
+
+  ({ __typeof (x) arg = (x);                                            \
+  void *buf = NULL;                                                     \
+    if(__builtin_types_compatible_p(typeof(x), int) ||                  \
+       __builtin_types_compatible_p(typeof(x), long)){                  \
+      buf = _mmap_file_fd(arg, shared, sz);                             \
+    } else if(types_compatible(typeof(x), char*) || types_compatible(typeof(x), uint8_t*)){ \
+      buf = _mmap_file_filename((char*)arg, shared, sz);                \
+    } else if(types_compatible(typeof(x), FILE*)){                      \
+      static_assert(types_compatible(int, FILE*), "fail\n");            \
+      buf = _mmap_file_FILE(arg, shared, sz);                           \
+    }                                                                   \
+    buf;})
*/
