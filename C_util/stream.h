#ifndef __STREAM_H__
#define __STREAM_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "C_util.h"
#include "string_buf.h"
enum stream_type {
  mem_stream = 0,
  string_stream = 1,//wrapper around string buf, probably unecessary
  file_stream = 2,
  fd_stream = 3,
  mmap_stream = 4,
  stream_type_max = 4
};
/*
  TODO: Add some sort of read/write lock to this so streams
  can be used in multithreaded programs.
*/
//header common to all streams
struct stream {
  size_t size;
  uint8_t type;
  uint8_t mode;//read/write/etc...
  uint16_t pad;
  //probably 32 bits of padding here
};
/*
  A mem stream is a hunk of memory, with pointers to the
  start and the current location, it also keeps track of the
  total size of the memory, and how much of that is currently being used.
*/
struct mem_stream {
//header
  size_t size;
  uint8_t type;
  uint8_t mode;//read/write/etc...
  uint16_t pad;
  //probably 32 bits of padding here

  uint8_t *buf;
  uint8_t *bufptr;
  size_t len;//ammount of buffer actually used
};
/*
  Basically a reimplementation of the builtin FILE type, though
  probably a bit less complex. The sizes of the read and write
  buffers are defined in stream.c.
*/
/*
  NOTE to self: Use pread/pwrite to read and write from the underlying
  file descriptor, and keep track of the file offset internally. This
  allows multiple streams on the same file to be at different positions,
  which is especially useful in multithreaded programs.
*/
struct fd_stream {
  size_t size;
  uint8_t type;
  uint8_t mode;//read/write/etc...
  uint16_t pad;
  //probably 32 bits of padding here

  int fd;
  char *filename;
  off_t offset;

  uint8_t *read_buf;
  uint8_t *read_buf_ptr;

  uint8_t *write_buf;
  uint8_t *write_buf_ptr;
};
/*
  An mmap stream is for the most part similar to a mem stream,
  execept an mmap file is used instead of just heap allocated memory.
  If opened in write mode the file can be written back to disk, and 
  with some internal tricks can be resized as well.
*/
struct mmap_stream {
  size_t size;
  uint8_t type;
  uint8_t mode;//read/write/etc...
  uint16_t pad;
//probably 32 bits of padding here

//compatable with mem_stream
  uint8_t *buf;
  uint8_t *bufptr;
  size_t len;

  int fd;
  char *filename;
};
#ifdef __cplusplus
}
#endif
#endif /* __STREAM_H__ */
