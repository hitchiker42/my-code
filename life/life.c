#define _GNU_SOURCE
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
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
int life_rand(int max){
  return random() % max;
}
/*
static uint32_t compute_edge(world *w, int x, int y){
  if(w->edge_rule == Edge_Dead || w->edge_rule == Edge_Grow){
    return 0;
  } if (w->edge_rule == Edge_Alive){
    return 1;
  } if (w->edge_rule = Edge_Random){
    return life_rand(2);
  } else {
    //toroid
    int x1,y1;
    if(x==0){
      x1 == w->cols-1;
    } else if(x == w->cols-1){
      x1 = 0;
    } else {
      x1 = x;
    }
    if(y==0){
      y1 == w->cols-1;
    } else if(y == w->cols-1){
      y1 = 0;
    } else {
      y1 = y;
    }
    return w->grid[y*w->cols + x];
  }
}
static inline uint32_t top_left(world *w, int x, int y){
  if(x == 0 || y == 0){
    return compute_edge(w,x,y);
  }
  return w->grid[(y-1)*w->cols + x-1];
}
static inline uint32_t top_mid(world *w, int x, int y){
  if(y == 0){
    return compute_edge(w,x,y);
  }
  return w->grid[(y-1)*w->cols + x];
}
static inline uint32_t top_right(world *w, int x, int y){
  if(x == w->cols-1 || y == 0){
    return compute_edge(w,x,y);
  }
  return w->grid[(y-1)*w->cols + x+1];
}
static inline uint32_t bottom_left(world *w, int x, int y){
  if(x <= 0 || y >= w->rows-1){
    return compute_edge(w,x,y);
  }
  return w->grid[(y+1)*w->cols + x-1];
}
static inline uint32_t bottom_mid(world *w, int x, int y){
  if(y >= w->rows-1){
    return compute_edge(w,x,y);
  }
  return w->grid[(y+1)*w->cols + x];
}
static inline uint32_t bottom_right(world *w, int x, int y){
  if(x >= w->cols-1 || y >= w->rows-1){
    return compute_edge(w,x,y);
  }
  return w->grid[(y+1)*w->cols + x+1];
}
static inline uint32_t mid_left(world *w, int x, int y){
  if(x <= 0){
    return compute_edge(w,x,y);
  }
  return w->grid[(y)*w->cols + x-1];
}
static inline uint32_t mid_right(world *w, int x, int y){
  if(x >= w->cols-1){
    return compute_edge(w,x,y);
  }
  return w->grid[(y)*w->cols + x+1];
}
uint32_t count_neighbors(world *w, int x, int y){
  if(x < 0 || y < 0 || x >= w->cols || y >= w->rows){
    raise(SIGTRAP);
  }
  int neighbors = top_left(w,x,y) + top_mid(w,x,y) + top_right(w,x,y) +
                  mid_left(w,x,y) + mid_right(w,x,y) +
                  bottom_left(w,x,y) + bottom_mid(w,x,y) + bottom_right(w,x,y);
  assert(neighbors < 9);
  return neighbors;
}
void step_world(world *w){
  //i don't know if it'd be more efficent to use a nested loop or not
  int i,j;
  FILE *f = fopen("neighbors", "a");
  for(i=0;i<w->rows;i++){
    for(j=0;j<w->cols;j++){
      int neighbors = count_neighbors(w, j, i);
      fprintf(f, "((%d,%d),(%d,%d)) ", w->grid[i*w->cols + j], neighbors,i,j);
      if(w->grid[i*w->cols + j] &&
         (neighbors < w->life_min || neighbors > w->life_max)){
        w->grid_step[i*w->cols + j] = 0;
      } else if (!w->grid[i*w->cols + j] &&
                 neighbors >= w->birth_min && neighbors <= w->birth_max){
        w->grid_step[i*w->cols + j] = 1;
      } else {
        w->grid_step[i*w->cols + j] = w->grid[i*w->cols +j];
      }
    }
    fprintf(f,"\n");
  }
  for(i=0;i<w->rows;i++){
    for(j=0;j<w->cols;j++){
      fputc(w->grid_step[i*w->rows + j] ? '#' : ' ',f);
    }
    fprintf(f,"\n");
  }
  fclose(f);
  if(w->edge_rule == Edge_Grow){
    //code to grow array
  }
  SWAP(w->grid, w->grid_step);
  //  dump_world_to_file(w, "dump");
}*/
void reset_world(world *w){
  memset(w->grid, 0, w->grid_size);
  memset(w->grid_step, 0, w->grid_size);
}
void randomize_grid(world *w){
  int i,j;
  memset(w->grid, 0, w->grid_size);
  memset(w->grid_step, 0, w->grid_size);
  for(i=1;i<w->rows-1;i++){
    for(j=1;j<w->cols-1;j++){
      if(life_rand(10) == 0){
        w->grid[i*w->cols + j] = 1;
      }
    }
  }
}
int count_neighbors(world *w, int x, int y){
  //the compiler should optimize these out
  uint8_t *g = w->grid;
  int cols = w->cols;
  int neighbors =
    g[(y-1)*cols + x-1] + g[(y-1)*cols + x] + g[(y-1)*cols + x+1] +
    g[y*cols + x-1]     + g[y*cols + x+1] +
    g[(y+1)*cols + x-1] + g[(y+1)*cols + x] + g[(y+1)*cols + x+1];
  return neighbors;
}
void step_world(world *w){
  int i,j;
  for(i=1;i<w->rows-1;i++){
    for(j=1;j<w->cols-1;j++){
      int neighbors = count_neighbors(w, j, i);
      //the two ifs check the cases where a cell might change state
      if(w->grid[i*w->cols + j] == 1 &&
         neighbors < 2 || neighbors > 3){
         //         (neighbors < w->life_min || neighbors > w->life_max)){
        w->grid_step[i*w->cols + j] = 0;
      } else if (w->grid[i*w->cols + j] == 0 && neighbors == 3){
                 //                 neighbors >= w->birth_min && neighbors <= w->birth_max){
        w->grid_step[i*w->cols + j] = 1;
      } else {
        w->grid_step[i*w->cols + j] = w->grid[i*w->cols +j];
      }
    }
  }
  SWAP(w->grid, w->grid_step);
}
world *read_world_from_file(char *filename){
  int fd = open(filename, O_RDONLY);
  if(fd <= 0){
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
  w->cols = (uint8_t*)memchr(mem_ptr, '\n', file_size) - mem;
  int i;
  for(i=0;i<file_size;i++){
    uint8_t *line_end = memchr(mem_ptr, '\n', chars_left);
    if(line_end - mem_ptr != w->cols){
      fprintf(stderr, "Error, Inconsistant row length in initial grid\n");
      goto cleanup;
    }
    w->rows++;
    memcpy(mem_ptr, grid_ptr, w->cols);
    grid_ptr += w->cols;
    mem_ptr = line_end + 1;
  }
  munmap(mem, file_size);
  return w;

 cleanup:
    free(w->grid);
    free(w);
    munmap(mem,file_size);
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
  memset(w->grid, '\0', w->grid_size);
  memset(w->grid_step, '\0', w->grid_size);
  w->rows = rows;
  w->cols = cols;
  set_world_rules(w, 2, 3, 2, 2, Edge_Dead);
  return w;
}
void write_hline(int width, FILE *f){
  int i;
  fputc('+', f);
  for(i=0;i<width;i++){
    fputc('-', f);
  }
  fputs("+\n", f);
} 
void write_neighbors(world *w, char *filename){
  FILE* f = fopen(filename, "w");
  int i,j;
  write_hline(w->cols, f);
  for(i=0;i<w->rows;i++){
    fputc('|', f);
    for(j=0;j<w->cols;j++){
      if(i==0 | j == 0 | i == w->rows-1 || j == w->cols -1){
        fputc('0', f);
        continue;
      }
      int neighbors = count_neighbors(w, j, i);
      if(neighbors < 0 || neighbors > 9){
        fprintf(stderr, "Error, impossible number of neighbors %d at (%d, %d)\n",
                neighbors, i, j);
        assert(neighbors < 9);
      }
      fputc(neighbors + 0x30, f);
    }
    fputs("|\n", f);
  }
  write_hline(w->cols, f);
  fclose(f);
}
void write_world_to_file(world *w, char *filename){
  FILE* f = fopen(filename, "w");
  int i,j;
  write_hline(w->cols, f);
  for(i=0;i<w->rows;i++){
    fputc('|',f);
    for(j=0;j<w->cols;j++){
      fputc(w->grid[i*w->rows + j] ? '1' : '0', f);
    }
    fputs("|\n", f);
  }
  write_hline(w->cols, f);
  fclose(f);
}
void run_life_debug(world *w){
  while(1){
    step_world(w);
  }
}
#ifdef LIFE_MAIN
void run_life_test(int rows, int cols, int steps){
  char *filename1, *filename2;
  int i,j,step;
  world *w = init_world(rows, cols);
  randomize_grid(w);
  asprintf(&filename1, "grid_step_0");
  asprintf(&filename2, "grid_neighbors_0");
  write_world_to_file(w, filename1);
  write_neighbors(w, filename2);
  for(step=0;step < steps; step++){
    step_world(w);
    asprintf(&filename1, "grid_step_%d",step);
    asprintf(&filename2, "grid_neighbors_%d", step);
    write_world_to_file(w, filename1);
    write_neighbors(w, filename2);
  }
  return;
}
int main(int argc, char **argv){
  int rows = 40, cols = 40, steps = 10;
  char *outdir = "life_output";
  srandom(0);
  if(argc >= 3){
    rows = strtol(argv[1], NULL, 0);
    cols = strtol(argv[2], NULL, 0);
    if(argc >=4){
      steps = strtol(argv[3], NULL, 0);
    }
    if(argc >= 5){
      outdir = argv[4];
    }
  }
  struct stat buf;
  int err = stat(outdir, &buf);
  if(err != -1){
    if(!S_ISDIR(buf.st_mode)){
      fprintf(stderr,"File %s exists and is not a directory\n",outdir);
      exit(1);
    }
  } else {
    if(errno != ENOENT){
      perror("stat");
      exit(1);
    }    
    mkdir(outdir, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
  }
  err = chdir(outdir);
  if(err == -1){
    perror("chdir");
    exit(1);
  }
  run_life_test(rows, cols, steps);
}
    
#endif
