#ifndef __MMAP_WRAPPER_H__
#define __MMAP_WRAPPER_H__
#ifdef __cplusplus
extern "C" {
#endif
//Actual memory mapping code (and thus system specific code)
//is implemented in C in mmap_wrapper.c
#include <sys/stat.h>
#include <sys/types.h>
#include <stdbool.h>

#if (defined __unix__)
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>
struct native_mmap_wrapper {
  void *data;
  size_t sz;
};

#elif (defined _WIN32)
#include <io.h>
#include <windows.h>
#define open _open
#define close _close
#define stat _stat
#define fstat _fstat
#define O_RDWR _O_RDWR
#define O_RDONLY _O_RDONLY
#define O_WRONLY _O_WRONLY

#define PROT_READ     0x1
#define PROT_WRITE    0x2
/* This flag is only available in WinXP+ */
#ifdef FILE_MAP_EXECUTE
#define PROT_EXEC     0x4
#else
#define PROT_EXEC        0x0
#define FILE_MAP_EXECUTE 0
#endif

#define MAP_SHARED    0x01
#define MAP_PRIVATE   0x02
#define MAP_ANONYMOUS 0x20
#define MAP_ANON      MAP_ANONYMOUS
#define MAP_FAILED    ((void *) -1)
struct native_mmap_wrapper {
  void *data;
  size_t sz;
  HANDLE fd;
  HANDLE fmap;
};
//mmap/munmap for windows, these really shouldn't be used if it can be helped
//since they will leak filehandles.
void *mmap(void *start, size_t length, int prot,
           int flags, int fd, off_t offset);
void munmap(void *addr, size_t length);
#else
#error "Don't know how to mmap files for the current system."
#endif

bool native_mmap(void *addr, size_t length, int prot, int flags,
                 int fd, off_t offset,
                 struct native_mmap_wrapper *mptr);
bool native_mmap_file(const char* filename, int prot, bool shared,
                      struct native_mmap_wrapper *mptr);
int native_munmap(struct native_mmap_wrapper *mptr);
#ifdef __cplusplus
}
//Wrapper 'class' implemented in C++.
struct mmap_wrapper {
  using pointer =  const unsigned char*;
  using reference = const unsigned char&;
  using value_type = unsigned char;
  using size_type = size_t;
  using difference_type = ptrdiff_t;
  using iterator =  pointer;
  using const_iterator = iterator;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = reverse_iterator;
  //native_mmap_wrapper will always begin like this.
  //struct native_mmap_header {
  //  void *data;
  //  size_t sz;
  //};
  union {
    native_mmap_wrapper mapping = {0};
    struct {
      unsigned char *mem;
      size_type sz;
    };
  };
  mmap_wrapper(const char *filename, int prot, int shared = -1){
    //Default to shared for read only mappings, private otherwise.
    if(shared == -1){
      shared = (prot & PROT_WRITE ? false : true);
    }
    native_mmap_file(filename, prot, shared, &mapping);
  }
  //just directly calls mmap (passing a null ptr for addr)
  mmap_wrapper(size_t length, int prot,
               int flags, int fd, off_t offset){
    native_mmap(nullptr, length, prot, flags, fd, offset, &mapping);
  }
  //different order of arguments with some sane defaults.
  mmap_wrapper(int fd, int prot, int shared = -1,
               off_t offset = 0, size_t length = 0){
    if(length == 0){
      struct stat buf;
      if(fstat(fd, &buf) < 0){
        return;
      }
      length = buf.st_size;
    }
    if(shared == -1){
      shared = (prot & PROT_WRITE ? MAP_PRIVATE : MAP_SHARED);
    }
    native_mmap(nullptr, length, prot, flags, fd, offset, &mapping);
  }
  ~mmap_wrapper(){
    if(mapping.data){
      native_munmap(&mapping);
    }
  }
  operator bool() const {
    return mapping.data;
  }
  std::string_view to_std_string_view() const {
    return std::string_view(static_cast<char*>(data()), size());
  }
  //implicit conversion to std::string_view allows using an mmaped
  //file anywhere you would use a string(view).
  operator std::string_view() const {
    return this->to_std_string_view();
  }
  //Standard container functions.
  const unsigned char* data() const {
    return mem;
  }
  size_type size() const {
    return sz;
  }
  unsigned char operator[](size_type idx) const {
    return mem[idx];
  }
  iterator begin() const {
    return iterator(mem);
  }
  iterator end() const {
    return iterator(mem+sz);
  }
  reverse_iterator rbegin() const {
    return reverse_iterator(end());
  }
  reverse_iterator rend() const {
    return reverse_iterator(begin());
  }
  reference front() const {
    return *begin();
  }
  reference back() const {
    return *rbegin();
  }
};
#endif
#endif /* __MMAP_WRAPPER_H__ */

/* Local Variables: */
/* mode: c++ */
/* End: */
