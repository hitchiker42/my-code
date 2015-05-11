#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <signal.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include "life.h"
static inline void* xmalloc(size_t sz){
  void *mem = malloc(sz);
  if(mem == NULL && sz){
    raise(SIGSEGV);
  }
  return mem;
}
static int compute_edge(world *w, point pt){
  if(w->edge_rule == Edge_Dead || w->edge_rule == Edge_Grow){
    return 0;
  } if (w->edge_rule == Edge_Alive){
    return 1;
  } if (w->edge_rule = Edge_Random){
    return life_rand(2);
  } else {
    //toroid
    int x,y;
    if(pt.x==0){
      x == w->cols-1;
    } else if(pt.x == w->cols-1){
      x = 0;
    } else {
      x = pt.x;
    }
    if(pt.y==0){
      y == w->cols-1;
    } else if(pt.y == w->cols-1){
      y = 0;
    } else {
      y = pt.y;
    }
    return grid_point(w, make_point(x,y));
  }
}
static inline int top_left(world *w, point pt){
  if(pt.x == 0 || pt.y == 0){
    return compute_edge(w,pt);
  }
  return grid_point(w, make_point(pt.x-1, pt.y-1));
}
static inline int top_mid(world *w, point pt){
  if(pt.y == 0){
    return compute_edge(w,pt);
  }
  return grid_point(w, make_point(pt.x, pt.y-1));
}
static inline int top_right(world *w, point pt){
  if(pt.x == w->cols-1 || pt.y == 0){
    return compute_edge(w,pt);
  }
  return grid_point(w, make_point(pt.x+1, pt.y-1));
}
static inline int bottom_left(world *w, point pt){
  if(pt.x == 0 || pt.y == w->rows-1){
    return compute_edge(w,pt);
  }
  return grid_point(w, make_point(pt.x-1, pt.y+1));
}
static inline int bottom_mid(world *w, point pt){
  if(pt.y == w->rows-1){
    return compute_edge(w,pt);
  }
  return grid_point(w, make_point(pt.x, pt.y+1));
}
static inline int bottom_right(world *w, point pt){
  if(pt.x == w->cols-1 || pt.y == w->rows-1){
    return compute_edge(w,pt);
  }
  return grid_point(w, make_point(pt.x+1, pt.y+1));
}
static inline int mid_left(world *w, point pt){
  if(pt.x == 0){
    return compute_edge(w,pt);
  }
  return grid_point(w, make_point(pt.x-1, pt.y));
}
static inline int mid_right(world *w, point pt){
  if(pt.x == w->cols-1){
    return compute_edge(w,pt);
  }
  return grid_point(w, make_point(pt.x+1, pt.y));
}
int count_neighbors(world *w, point pt){
  return top_left(w,pt) + top_mid(w,pt) + top_right(w,pt) +
    mid_left(w,pt) + mid_right(w,pt) +
    bottom_left(w,pt) + bottom_mid(w,pt) + bottom_right(w,pt);
}
void step_world(world *w){
  //i don't know if it'd be more efficent to use a nested loop or not
  int i;
  for(i=0;i<w->grid_size;i++){
    int neighbors = count_neighbors(w, offset_to_point(i,w->cols));
    if(w->grid[i] && (neighbors < w->life_min || neighbors > w->life_max)){
      w->grid_step[i] == 0;
    } else if (!w->grid[i] &&
               neighbors >= w->birth_min && neighbors <= w->birth_max){
      w->grid_step[i] == 1;
    } else {
      w->grid_step[i] == w->grid[i];
    }
  }
  if(w->edge_rule == Edge_Grow){
    //code to grow array
  }
  memcpy(w->grid, w->grid_step, w->grid_size);
}
void reset_world(world *w){
  memset(w->grid, ' ', w->grid_size);
}
world *read_world_from_file(char *filname){
  int fd = open(filename, );
  if(fd <= NULL){
    return NULL;
  }
  struct stat buf;
  fstat(fd, &buf);
  size_t file_size = buf.st_size;
  uint8_t *mem = mmap(NULL, file_size, PROT_READ|PROT_WRITE, MAP_PRIVATE,
                   fd, 0);
  if(mem == MAP_FAILED){
    perror("mmap");
    return NULL;
  }
  world *w = xmalloc(sizeof(world));
  //this is more memory than we need, but it's much eaiser this way
  w->grid = xmalloc(file_size);
  uint8_t *grid_ptr = w->grid;
  uint8_t *mem_ptr = mem;
  int chars_left = file_size;
  w->rows = 0;
  //get the number of entries in the first line
  w->cols = memchr(mem_ptr, '\n', file_size) - mem;
  int i;
  for(i=0;i<file_size;i++){
    uint8_t *line_end = memchr(mem_ptr, '\n', chars_left);
    if(line_end - mem_ptr != w->cols){
      fprintf(stderr, "Error, Inconsistant row length in initial grid\n");
      goto cleaup;
    }
    w->rows++;
    memcpy(mem_ptr, grid_ptr, w->cols);
    grid_ptr += w->cols;
    mem_ptr = line_end + 1;
  }
    munmap(mem);
    return w;

  cleanup:
    free(w->cols);
    free(w);
    munmap(mem);
    return NULL;
}

void set_world_rules(world *w, int life_min, int life_max,
                     int birth_min, int birth_max, int edge_rule){
  w->life_min = life_min;
  w->life_max = life_max;
  w->birth_min = birth_min;
  w->birth_max = birth_max;
  w->edge_rule = edge_rule;
  return;
}
  
world *init_world(int rows, int cols){
  world *w = xmalloc(sizeof(world));
  w->grid_size = rows * cols;
  w->grid = xmalloc(w->grid_size);
  w->grid_step = xmalloc(w->grid_size);
  reset_world(w);
  w->rows = rows;
  w->cols = cols;
  return w;
}
