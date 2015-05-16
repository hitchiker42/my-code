#include <stdint.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include "util.h"
//possible rules for neighbors for cells on the edge of the grid
//bitmask for speed/familarity 
enum edge_rules {
  Edge_Dead,//default, treat any cell outside the grid as dead
  Edge_Wrap,//treat the grid as a toroid
};
typedef struct world {
  int rows;//rows in the active grid
  int cols;//cols in the active grid
  uint8_t *grid;//memory allocated for the grid
  uint8_t *grid_step;//memory used in updating the grid
  int grid_size; //for convience, it should always be rows-1 * cols-1
  //for growing the world reallocate grid_step each step, it makes life
  //much eaiser for everyone else
  /*
    These parameters allow tweaking the rules of life.
    For the classic game of life: life_min = 2, life_max=3, birth_min = birth_max = 3
  */
  int life_min;//minimum number of neighbors alive to continue living
  int life_max;//maximum ""                                        ""
  int birth_min;//mimumum number of neighbors alive to bring a dead cell to life
  int birth_max;//maximum number of neighbors alive to bring a dead cell to life
  int edge_rule;
  void *userdata;
} world;
void step_world(world *w);
//step back one iteration, only works once, and only if the world hasn't
//been modified by the last call to step_world. Akin to unread_char
void step_world_back(world *w);
world *init_world(int rows, int cols);
//initialize the world by setting each point in pts as alive
void init_world_from_points(world *w, int *x, int *y, int num_pts);
//initialize the world by setting the grid according to a character array
//alive and dead are the characters use to represent each state
//(ex ('1','0'),('#',' '),etc...), default to 1,0
void init_world_from_array(world *w, char *arr, char alive, char dead);
void reset_world(world *w);
//destroys the current grid
void resize_world(world *w, int width, int height);
#define reset_grid(w) reset_world(w);
void randomize_grid(world *w);
void write_world_to_file(world *w, char *filename);
void run_life_debug(world *w);
/*
  Convert between x,y coordinates and memory offsets, the number of
  colums in the array needs to be known to do this conversion
*/
/*static inline uint32_t point_to_offset(point pt, int cols){
  return pt.y*cols + pt.x;
}
static inline point offset_to_point(int offset, int cols){
  return (point){.x = offset % cols, .y = offset/cols};
}
static inline uint32_t grid_point(world *w, point pt){
  int offset = point_to_offset(pt, w->cols);
  if(offset < 0 || offset > w->grid_size){
    raise(SIGTRAP);
    fprintf(stderr,"\noffset = %d, point = (%d,%d), grid_size = %d, grid_dims=%dx%d\n",
            offset, pt.x, pt.y,w->grid_size, w->rows, w->cols);
    assert(offset > 0 && offset < w->grid_size);
  }
  return w->grid[offset];
  }*/
