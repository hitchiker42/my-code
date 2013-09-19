#include <stdio.h>
typedef struct point point;
struct point{
  int x;
  int y;
  int z;
};
point incx(point loc){
  return (point){loc.x+1,loc.y,loc.z};
}
