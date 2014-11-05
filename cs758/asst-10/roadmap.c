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

#include <stdio.h>
#include <stdlib.h>

#include <netinet/in.h>		/* for POSIX byte order conversions */
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#include "roadmap.h"
#include <errno.h>

static const size_t int_field_size = 4; /* bytes */
static const size_t ver_field_size = 1; /* byte */
static const unsigned char current_version = 2;
static const size_t node_field_size = 16;
static const size_t arc_field_size = 8;
static const size_t header_size = 9;
/* What I can piece together about the file format

   static const size_t int_field_size = 4;
   static const size_t ver_field_size = 1;
   static const unsigned char current_version = 2;

   first byte is a version number, next 4 bytes are the number of
   nodes
   if the version is greater than 1 then the next 4 bytes are the
   number of arcs

   The rest of the file contains the nodes, the format for a node is:
   4 bytes for the node number (unsigned)
   4 bytes for the x coordinate (signed)
   4 bytes for the y coordinate (signed)
   4 bytes for the number of arcs (unsigned)
   (8 * num_arcs) bytes for the arcs
     Each arc has the format:
       4 bytes for the target
       4 bytes for the weight
*/
static uint32_t read_int(svector *input);
static inline void _read_arc(svector *input, struct roadmap *map);
static void _read_node(svector *input, struct roadmap *map);
static struct buffer read_input_into_buffer(char *filename);
struct roadmap *read_graph(struct buffer file_buf);
struct roadmap *read_input(char *input_filename){
  struct buffer file_buf = read_input_into_buffer(input_filename);
  if(file_buf.mem == NULL){
    return NULL;
  }
  struct roadmap *map = read_graph(file_buf);
  return map;
}
static uint32_t read_int(svector *input){
  uint32_t int_bytes = *(uint32_t*)(input->bytes+input->offset);
  uint32_t retval  = ntohl(int_bytes);
  input->offset+=4;
  return retval;
}
static inline void _read_arc(svector *input, struct roadmap *map){
  struct arc *a = map->arcs + map->num_arcs++;
  a->target = read_int(input);
  a->weight = (float)read_int(input);
  return;
}
static void _read_node(svector *input, struct roadmap *map){

  int node_num = read_int(input);
  struct node *n = map->nodes+node_num-1;
  map->num_nodes++;
  n->num = node_num;
  n->x = read_int(input);
  n->y = read_int(input);
  n->num_arcs = read_int(input);
  n->arcv = map->arcs + map->num_arcs;
  n->dist = INFINITY;
  n->parent = NULL;
  int i;
  for(i=0;i<n->num_arcs;i++){
    _read_arc(input, map);
  }
  return;
}
static struct buffer read_input_into_buffer(char *filename){
  int fd = open(filename, O_RDONLY);
  if(fd == -1){
    perror("open");
    return null_buffer;
  }
  errno = 0;
  struct buffer file_buf = mmap_file(fd, 0);
  if(errno != 0){
    return null_buffer;//mmap_file calls perror
  }
  close(fd);//mmap internally dups the file descriptor
  if(errno != 0){
    perror("close");
    return null_buffer;
  }
  return file_buf;
}
struct roadmap *read_graph(struct buffer file_buf){
  svector *input = alloca(sizeof(svector));
  struct node *nodes = NULL;
  struct arc *arcs = NULL;
  struct roadmap *retval = NULL;
  input->bytes = file_buf.mem;
  input->size = file_buf.size;
  input->len = file_buf.size;
  input->offset = 1;//for the version
  if(input->size < header_size){
    fprintf(stderr,"Error invalid data file header\n");
    goto error;
  }
  int version = *input->bytes;
  if(version < 2){
    fprintf(stderr,
            "Invalid data file version, expected version 2 or greater\n");
    goto error;
  }
  uint32_t num_nodes = read_int(input);
  uint32_t num_arcs = read_int(input);
  uint32_t expected_size = num_nodes*node_field_size + num_arcs+arc_field_size;
  if(input->size < expected_size){
    fprintf(stderr,
            "Error invalid data file length %lu bytes, expected %u bytes",
            input->size, expected_size);
    goto error;
  }
  retval = xmalloc(sizeof(struct roadmap));//zeros memory
  nodes = xmalloc_atomic(sizeof(struct node)*num_nodes);
  arcs = xmalloc_atomic(sizeof(struct arc)*num_arcs);
  retval->nodes = nodes;
  retval->arcs = arcs;
  while(retval->num_nodes < num_nodes){
    _read_node(input, retval);
  }
  return retval;

 error:
  munmap(input->bytes,input->size);
  //since free(NULL) is a noop this will always be valid
  free(nodes);
  free(arcs);
  free(input);
  return NULL;
}
void free_roadmap(struct roadmap *map){
  free(map->nodes);
  free(map->arcs);
  free(map);
}

/*double sq_dist(int x, int y, int u, int v){
  double dx = x - u;
  double dy = y - v;
  return dx * dx + dy * dy;
}*/


/*
 * Read an unsigned integer field from the file.
 *
 * Return 1 on success and 0 on failure.
 */
static int read_uint_field(FILE *f, uint *field){
  size_t ret;

  ret = fread(field, int_field_size, 1, f);
  if (ret != 1) {
    if (ret == 0) {
      fprintf(stderr, "Unexpected end of file\n");
    }
    else
      fprintf(stderr, "Short read\n");
    return 1;
  }
  *field = ntohl(*field);

  return 0;
}

/*
 * Read an integer field from the file.
 *
 * Return 1 on success and 0 on failure.
 */
static int read_int_field(FILE *f, int *field){
  size_t ret;

  ret = fread(field, int_field_size, 1, f);
  if (ret != 1) {
    if (ret == 0) {
      fprintf(stderr, "Unexpected end of file\n");
    }
    else
      fprintf(stderr, "Short read\n");
    return 1;
  }
  *field = ntohl(*field);

  return 0;
}


/*
 * Read the arc from the given file.
 *
 * Return 1 on success and 0 on failure.
 */
static int read_arc(FILE *f, struct arc *a){
  int err;

  err = read_uint_field(f, &a->target);
  a->target = a->target - 1; /* avoiding 1-indexing of input */
  if (err) {
    return 1;
  }
  uint temp;
  err = read_uint_field(f, &temp);
  if (err) {
    return 1;
  }
  a->weight = (float)temp;
  return 0;
}


/*
 * Read the arcs from the given file into the array of arcs.
 *
 * Return 1 on success and 0 on failure.
 */
static int read_arcs(FILE *f, struct arc arcs[], unsigned int num_arcs){
  unsigned int i;
  int err;

  for (i = 0; i < num_arcs; i += 1) {
    err = read_arc(f, &arcs[i]);
    if (err) {
      break;
    }
  }

  return err;
}


/*
 * Reads the next node from the given file.  On success, the 'arcv'
 * array is allocated and it must be freed by the caller.
 *
 * Return 1 on success, 0 on failure.
 *
 */
static int read_node(FILE *f, struct node *n){
  int err;

  err = read_uint_field(f, &n->num);
  n->num = n->num - 1; /* avoiding 1-indexing */
  if (err) {
    return 1;
  }
  err = read_int_field(f, &n->x);
  if (err) {
    return 1;
  }
  err = read_int_field(f, &n->y);
  if (err) {
    return 1;
  }
  err = read_uint_field(f, &n->num_arcs);
  if (err) {
    return 1;
  }

  n->arcv = xmalloc(n->num_arcs * sizeof(*n->arcv));
  err = read_arcs(f, n->arcv, n->num_arcs);
  if (err) {
    free(n->arcv);
    return 1;
  }

  return 0;
}

/*
 * Reads the nodes into the given array.
 *
 * Return 0 on success and 1 on failure.
 */
static int read_nodes(FILE *f, struct node *nodes, unsigned int nnodes){
  unsigned int i;
  int err;

  for (i = 0; i < nnodes; i += 1) {
    err = read_node(f, nodes+i);
    if (err) {
      break;
    }
    nodes[i].dist = INFINITY;
  }
  return err;
}
/*static int read_header(FILE *f, unsigned char *ver, unsigned int *nnodes){
  int err;
  ssize_t ret;

  ret = fread(ver, ver_field_size, 1, f);
  if (ret != 1) {
    if (ret == 0) {
      fprintf(stderr, "Unexpected end of file\n");
    }
    else
      fprintf(stderr, "Short read\n");
    return 1;
  }
  err = read_uint_field(f, nnodes);
  if (err) {
    return 1;
  }
  if (*ver > 1) {
    unsigned int narcs;
    err = read_uint_field(f, &narcs);
    if (err) {
      return 1;
    }
  }

  return 0;
}*/
/*
 * Reads the header from the file.
 *
 * Return 1 on success and 0 on failure.
 */
static int read_header(FILE *f, unsigned char *ver, unsigned int *nnodes){
  int err;
  ssize_t ret;

  ret = fread(ver, ver_field_size, 1, f);
  if (ret != 1) {
    if (ret == 0) {
      fprintf(stderr, "Unexpected end of file\n");
    }
    else
      fprintf(stderr, "Short read\n");
    return 1;
  }
  err = read_uint_field(f, nnodes);
  if (err) {
    return 1;
  }
  if (*ver > 1) {
    //...what?
    unsigned int num_arcs;
    err = read_uint_field(f, &num_arcs);
    if (err) {
      return 1;
    }
  }

  return 0;
}


int load_map(FILE *f, struct node **nodesp, unsigned int *nnodes){
  unsigned char ver = 0;
  struct node *nodes;
  int err;

  err = read_header(f, &ver, nnodes);
  if (err) {
    return 1;
  }
  if (ver > current_version) {
    fprintf(stderr,
            "File version is %u, %u is the maximum version\n",
            (unsigned int) ver, (unsigned int) current_version);
    return 1;
  }

  if (*nnodes == 0) {
    fprintf(stderr, "No nodes to read\n");
    return 1;
  }

  nodes = xmalloc((*nnodes) * sizeof(*nodes));

  err = read_nodes(f, nodes, *nnodes);
  if (err) {
    free(nodes);
    return 1;
  }

  *nodesp = nodes;

  return 0;
}


void free_map(struct node *nodes, unsigned int nnodes){
  unsigned int i;

  for (i = 0; i < nnodes; i += 1) {
    free(nodes[i].arcv);
  }

  free(nodes);
}
