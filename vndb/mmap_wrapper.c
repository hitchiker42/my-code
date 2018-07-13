#include "mmap_wrapper.h"
#if (defined __unix__)
bool native_mmap(void *addr, size_t length, int prot, int flags,
                 int fd, off_t offset,
                 native_mmap_wrapper *mptr){
  void* pa = mmap(addr, length, prot, flags, fd, offset);
  if(pa == MAP_FAILED){
    return false;
  } else {
    mptr->data = pa;
    mptr->size = length;
    return true;
  }
}
bool native_mmap_file(const char* filename, int prot, bool shared,
                      native_mmap_wrapper *mptr){
  int open_flags = ((prot & PROT_READ) ?
                    ((prot & PROT_WRITE) ? O_RDWR : O_RDONLY) :
                    ((prot & PROT_WRITE) ? O_WRONLY : -1));
  int fd = open(filename, open_flags);
  if(fd < 0){
    return false;
  }
  struct stat buf;
  if(fstat(fd, &buf) < 0){
    close(fd);
    return false;
  }
  size_t length = buf.st_size;
  bool ret = native_mmap(nullptr, length, prot,
                         (shared ? MAP_SHARED : MAP_PRIVATE), fd, 0,
                         mptr);
  //doesn't matter if we succeeded or failed we need to close fd either way.
  close(fd);
  return ret;
}
int native_munmap(struct native_mmap_wrapper *mptr){
  return munmap(mptr->data, mptr->sz);
}
#elif (defined _WIN32)
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
#ifdef __USE_FILE_OFFSET64
# define DWORD_HI(x) (x >> 32)
# define DWORD_LO(x) ((x) & 0xffffffff)
#else
# define DWORD_HI(x) (0)
# define DWORD_LO(x) (x)
#endif
//basic code copied from uclibc (util/mmap-windows.c). I broke it into multiple
//functions and made some minor changes
static DWORD  mmap_prot_to_flProtect(int prot){
  DWORD flProtect;
  if (prot & PROT_WRITE) {
    if (prot & PROT_EXEC) {
      flProtect = PAGE_EXECUTE_READWRITE;
    } else {
      flProtect = PAGE_READWRITE;
    }
  } else if (prot & PROT_EXEC) {
    if (prot & PROT_READ) {
      flProtect = PAGE_EXECUTE_READ;
    } else if (prot & PROT_EXEC) {
      flProtect = PAGE_EXECUTE;
    }
  } else {
    flProtect = PAGE_READONLY;
  }
  return flProtect;
}
static DWORD mmap_prot_and_flags_to_dwDesiredAccess(int prot, int flags){
  DWORD dwDesiredAccess;
  if (prot & PROT_WRITE) {
    dwDesiredAccess = FILE_MAP_WRITE;
  } else {
    dwDesiredAccess = FILE_MAP_READ;
  }
  if (prot & PROT_EXEC) {
    dwDesiredAccess |= FILE_MAP_EXECUTE;
  }
  if (flags & MAP_PRIVATE){
    dwDesiredAccess |= FILE_MAP_COPY;
  }
  return dwDesiredAccess;
}
bool native_mmap(void *addr, size_t length, int prot, int flags,
                 int fd, off_t offset,
                 native_mmap_wrapper *mptr){
  if (prot & ~(PROT_READ | PROT_WRITE | PROT_EXEC)) {
    return false;
  }
  if (fd == -1) {
    if (!(flags & MAP_ANON) || offset) {
      return MAP_FAILED;
    }
  } else if (flags & MAP_ANON) {
    return MAP_FAILED;
  }
  DWORD flprotect = mmap_prot_to_flProtect(prot);
  off_t end = length + offset;
  HANDLE mmap_fd, h;
  if (fd == -1) {
    mmap_fd = INVALID_HANDLE_VALUE;
  } else {
    mmap_fd = (HANDLE)_get_osfhandle(fd);
  }
  HANDLE h = CreateFileMapping(mmap_fd, NULL, flProtect,
                               DWORD_HI(end), DWORD_LO(end), NULL);
  if (h == NULL) {
    return false;
  }
  DWORD dwDesiredAccess = mmap_prot_and_flags_to_dwDesiredAccess(prot, flags);
  void *pa = MapViewOfFile(h, dwDesiredAccess,
                           DWORD_HI(offset), DWORD_LO(offset), length);
  if (ret == NULL) {
    CloseHandle(h);
    return false;
  }
  mptr->data = pa;
  mptr->sz = length;
  mptr->fd = fhandle;
  mptr->fmap = h;
  return true;
}

static bool native_mmap_file(const char* filename, int prot, bool shared,
                             native_mmap_wrapper *mptr){
  int open_flags = ((prot & PROT_READ) ?
                    ((prot & PROT_WRITE) ? O_RDWR : O_RDONLY) :
                    ((prot & PROT_WRITE) ? O_WRONLY : -1));
  int fd = open(filename, open_flags);
  if(fd < 0){
    return false;
  }
  struct stat buf;
  if(fstat(fd, &buf) < 0){
    close(fd);
    return false;
  }
  size_t length = buf.st_size;
  bool ret = native_mmap(nullptr, length, prot,
                         (shared ? MAP_SHARED : MAP_PRIVATE), fd, 0,
                         mptr);
  //with windows we need to keep the filehandle open as long as the mapping
  //exists so we only close it if we fail.
  if(!ret){
    close(fd);
  }
  return ret;
}
static int native_munmap(struct native_mmap_wrapper *mptr){
  bool success = UnmapViewOfFile(mptr->data);
  success = CloseHandle(mptr->fd) && success;
  success = CloseHandle(mptr->fmap) && success;
  return (success ? 0 : -1);
}

void *mmap(void *start, size_t length, int prot,
                  int flags, int fd, off_t offset){
  native_mmap_wrapper m;
  if(windows_native_mmap(length, prot, flags, fd, offset, &m)){
    return m->data;
  } else {
    return MAP_FAILED;
  }
}
void munmap(void *addr, size_t length){
  UnmapViewOfFile(addr);
  /* ruh-ro, we leaked handle from CreateFileMapping() ... */
}
#endif
