#include "gl_types.h"
gl_shape *make_triangle(gl_vertex *vertices, GLenum usage, GLuint VAO){
  gl_shape *tri = xmalloc(sizeof(gl_shape));
  tri->vertices = vertices;
  tri->VAO = VAO;
  GLuint buffer = make_data_buffer(tri->vertices, 3*sizeof(gl_vertex), usage);
  tri->buffer = buffer;
}
gl_shape *make_shape(gl_vertex *vertices, int num_vertices,
                     GLenum usage, GLuint VAO){
  gl_shape *shape = xmalloc(sizeof(gl_shape));
  shape->vertices = vertices;
  shape->VAO = VAO;
  GLuint buffer = make_data_buffer(shape->vertices,
                                   num_vertices*sizeof(gl_vertex), usage);
  shape->buffer = buffer;
}
struct color cube_color_1 = {.r = 0.0, .g = 0.5, .b = 1.0, .a = 0.5};
struct color cube_color_2 = {.r = 0.0, .g = 1.0, .b = 0.5, .a = 0.5};
struct vertex cube[8] =
  {{.pos = {-0.25,0.25,0.5}, .color = cube_color_1},
   {.pos = {0.25,0.25,0.5}, .color = cube_color_1},
   {.pos = {-0.25,-0.25,0.5}, .color = cube_color_1},
   {.pos = {0.25,-0.25,0.5}, .color = cube_color_1},
   {.pos = {-0.125,-0.125,0}, .color = cube_color_2},
   {.pos = {0.375,-0.125,0}, .color = cube_color_2},
   {.pos = {-0.125,0.375,0}, .color = cube_color_2},
   {.pos = {0.375,0.375,0}, .color = cube_color_2}};
uint8_t cube_indices[17] =
  {0,1,2,3,4,5,6,7,0xff,2,4,0,6,1,7,3,5};
/*
      6 -- 7  (4 is hidden)
     /|   /|
    0----1 5
    |/   |/
    2----3

    This is the best I can do, I think we need one restart
    draw the front face: (0,1,2), (1,2,3)
    draw the bottom face: (2,3,4), (3,4,5)
    draw the rear face: (4,5,6), (5,6,7)
    restart:
    draw the left face: (2,4,0),(4,0,6)
    draw the top face: (0,6,1), (6,1,7)
    draw the right face: (1,7,3),(7,3,5)

*/
