//gcc -o cairo_tut -lcairo -g -O2 cairo_tut.c
#include <cairo/cairo.h>
#include <math.h>//for M_PI
#define rgb_hex_to_float(rgb_hex)               \
  ((double)rgb_hex/255.0)
static inline void cairo_circle(cairo_t *cr,double radius){
  double x,y;
  cairo_get_current_point(cr,&x,&y);
  cairo_new_sub_path(cr);
  cairo_arc(cr,x,y,radius,0.0,2*M_PI);
  cairo_move_to(cr,x,y);
}
static inline void cairo_circle_at_point(cairo_t *cr,double radius,double x,double y){
  cairo_new_sub_path(cr);
  cairo_arc(cr,x,y,radius,0.0,2*M_PI);
}
int main(int argc,char *argv[]){
  cairo_surface_t *surface=
    cairo_image_surface_create(CAIRO_FORMAT_ARGB32,600,600);
  cairo_t *cr=cairo_create(surface);
  cairo_scale(cr,600,600);
  double bg=rgb_hex_to_float(0x3f);
  cairo_set_source_rgb(cr,bg,bg,bg);
  cairo_paint(cr);
  double fg1=rgb_hex_to_float(0xdc);
  double fg2=rgb_hex_to_float(0xcc);
  cairo_set_source_rgb(cr,fg1,fg1,fg2);
  cairo_set_line_width(cr,0.05);
  cairo_move_to(cr,0.25,0);
  cairo_line_to(cr,0.75,0);
  cairo_move_to(cr,0.25,1);
  cairo_line_to(cr,0.75,1);
  cairo_move_to(cr,0.5,1);
  cairo_line_to(cr,0.5,0);
  cairo_circle_at_point(cr,0.4,0.5,0.5);
  cairo_stroke(cr);
  cairo_destroy(cr);
  cairo_surface_write_to_png(surface,"theta.png");
  cairo_surface_destroy(surface);
  return 0;
}
