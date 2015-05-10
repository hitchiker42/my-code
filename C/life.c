#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "life.h"
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
//void reset_world(world *w){
  
