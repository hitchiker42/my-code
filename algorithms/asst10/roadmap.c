/**
 * \file roadmap.c
 *
 * Reads a roadmap in binary format into memory.  The nodes of the
 * roadmap are stored in an array where the index in the array is the
 * number of the node minus 1.
 *
 * \author eaburns
 * \date 18-08-2010
 */

#include "roadmap.h"

#include <stdio.h>
#include <stdlib.h>

#include <arpa/inet.h>		/* for POSIX byte order conversions */
#include <sys/mman.h>



static const size_t int_field_size = 4; /* bytes */
static const size_t ver_field_size = 1; /* byte */
static const uint8_t current_version = 2;
static int little_endian;

struct mem_buf {
  uint8_t *buf;
  uint8_t *bufptr;
  ssize_t bufsz;
};
typedef union int_buf int_buf;
union int_buf {
  uint32_t uint;
  int32_t sint;
  uint8_t buf[4];
};
typedef struct roadmap_internal roadmap_internal;
struct roadmap_internal {
  struct node *nodes;
  struct arc *arcs;
  size_t num_nodes;
  size_t num_arcs;
  int node_index;
  int arc_index;
  int ver;
};

//checks weather sz more bytes can be read from buf

#define check_bufsz(buf, sz)                            \
  (!(((buf->bufptr+sz) - buf->buf) > buf->bufsz))

static __attribute__((unused)) int is_little_endian(){
//  int_buf test = {.buf = {0x12,0x34,0x56, 0x78}};
//  return test.uint == 0x78563412;
  return !(ntohl(0x12345678) == 0x12345678);
}


//At least 4 bytes must be readable from buf when calling these functions
static int32_t read_int(struct mem_buf *buf){
  union int_buf tmp;
  int i;
//  DEBUG_PRINTF("Reading 4 bytes %hhx,%hhx,%hhx,%hhx\n",
//               *buf->bufptr,*buf->bufptr+1, *buf->bufptr+2, *buf->bufptr+3);
  /*
    This is a bit of a gamble performance wise, if the compiler can
    compute the result of the call to ntohl at compile time this will
    introduce no runtime overhead, however if not it will be much slower
    than just setting a global flag. I know gcc will optimize the test
    away which is why I'm using it, but I'm not sure about other compilers.
  */
  if(ntohl(0x12345678) == 0x12345678){
    //big endian
    for(i=0;i<4;i++){
      tmp.buf[i] = *buf->bufptr++;
    }
    little_endian = 0;
  } else {
    //little endian
    for(i=3;i>=0;i--){
      tmp.buf[i] = *buf->bufptr++;
    }
  }

  return tmp.sint;
}
static uint32_t read_uint(struct mem_buf *buf){
  union int_buf tmp;
  int i;
//  DEBUG_PRINTF("Reading 4 bytes %hhx,%hhx,%hhx,%hhx\n",
//               *buf->bufptr,*buf->bufptr+1, *buf->bufptr+2, *buf->bufptr+3);
  if(ntohl(0x12345678) == 0x12345678){
    //big endian
    for(i=0;i<4;i++){
      tmp.buf[i] = *buf->bufptr++;
    }
    little_endian = 0;
  } else {
    //little endian
    for(i=3;i>=0;i--){
      tmp.buf[i] = *buf->bufptr++;
    }
  }
  return tmp.uint;
}
/*
 * Reads the header from the file.
 *
 * Return 0 on success and -1 on failure.
 */
static int read_header(struct mem_buf *buf, roadmap_internal *map){
  HERE();
  if(!check_bufsz(buf, 5)){
    return -1;
  }
  map->ver = *buf->bufptr++;
  map->num_nodes = read_uint(buf);
  HERE();
  if(map->ver > 1){
    if(!check_bufsz(buf, 4)){
      return -1;
    }
    map->num_arcs = read_uint(buf);
  } else {
    map->num_arcs = 0;
  }
  return 0;
}
/*
  Read a node into the map, return 0 on success -1 on failure.
*/
int read_node(struct mem_buf *buf, roadmap_internal *map){
  /* Format of a node:
     4 bytes for the node number (unsigned)
     4 bytes for the x coordinate (signed)
     4 bytes for the y coordinate (signed)
     4 bytes for the number of arcs (unsigned)
     (8 * num_arcs) bytes for the arcs
     Each arc has the format:
     4 bytes for the target
     4 bytes for the weight
   */
  if(!check_bufsz(buf, 16)){
    return -1;
  }
  struct node *node = map->nodes + map->node_index;
  node->num = read_uint(buf);
  node->x = read_int(buf);
  node->y = read_int(buf);
  node->num_arcs = read_uint(buf);
  /*
    This is specific to my implementation
  */
  node->heap_index = map->node_index;
  node->dist = INFINITY;
  node->parent = NULL;

  map->node_index++;

  if(!check_bufsz(buf, node->num_arcs*8)){
    return -1;
  }

  struct arc *arcs;
  if(map->arcs != NULL){
    arcs = map->arcs + map->arc_index;
    map->arc_index += node->num_arcs;
    node->arcv = arcs;
  } else {
    map->num_arcs += node->num_arcs;
    node->arcv = xmalloc(sizeof(struct arc)*node->num_arcs);
    arcs = node->arcv;
  }

  int i;
  for(i=0;i<node->num_arcs;i++){
    arcs[i].target = read_uint(buf);
    //I need the weight of the arc as a float, so I convert it here
    //In general the code would just be:
    //arcs[i].weight = read_uint(buf);
    uint32_t tmp = read_uint(buf);
    arcs[i].weight = (float)tmp;
  }
  return 0;
}
/*
  Call the standard library function fn with the given arguments and
  return the value it returns. If there is an error exit the program.
*/
#define syscall_checked(fn, ...)                \
  ({__typeof(fn(__VA_ARGS__)) ret;              \
    errno = 0;                                  \
    ret = fn(__VA_ARGS__);                      \
    if(errno != 0){                             \
      perror(#fn);                              \
      exit(EXIT_FAILURE);                       \
    }                                           \
    ret;})

struct roadmap* read_input(const char *filename){
  int fd;
  void *mem;
  struct stat st_buf;
  int err;
  syscall_checked(stat, filename, &st_buf);

  fd = syscall_checked(open, filename, O_RDONLY);

  mem = syscall_checked(mmap, NULL, st_buf.st_size,
                        PROT_READ, MAP_PRIVATE, fd, 0);
  syscall_checked(close, fd);

  struct mem_buf buf = {.buf = mem,
                        .bufptr = mem, .bufsz = st_buf.st_size};
  roadmap_internal *map = zmalloc(sizeof(struct roadmap_internal));
#ifdef DEBUG
  if(is_little_endian()){
    DEBUG_PRINTF("Running on little endian computer\n");
  } else {
    DEBUG_PRINTF("Running on big endian computer\n");
  }
#endif

  err = read_header(&buf, map);
  DEBUG_PRINTF("%d\n",err);
  if(err != 0){
    goto cleanup;
  }
  if(map->ver > current_version){
    fprintf(stderr,
            "File version is %u, %u is the maximum version\n",
            (unsigned int) map->ver, (unsigned int) current_version);
    goto cleanup;
  }

  if(map->num_nodes == 0){
    fprintf(stderr, "No nodes to read\n");
    goto cleanup;
  }

  map->nodes = xmalloc(map->num_nodes * sizeof(struct node));
  if(map->num_arcs > 0){
    map->arcs = xmalloc(map->num_arcs * sizeof(struct arc));
  }

  //if we don't preallocate the arcs this will leak memory
  //if we fail in the middle of reading the map.
  int i;
  for(i=0;i<map->num_nodes;i++){
    if(read_node(&buf, map) != 0){
      goto cleanup;
    }
  }
  syscall_checked(munmap, buf.buf, buf.bufsz);

  return (struct roadmap*)map;

 cleanup:
  HERE();
  free(map->arcs);
  free(map->nodes);
  free(map);
  syscall_checked(munmap, buf.buf, buf.bufsz);
  return NULL;
}


void free_roadmap(struct roadmap *map){
  if(map->arcs != NULL){
    free(map->arcs);
  } else {
    size_t i;
    for(i=0;i<map->num_nodes;i++){
      free(map->nodes[i].arcv);
    }
  }
  free(map->nodes);
  free(map);
}
