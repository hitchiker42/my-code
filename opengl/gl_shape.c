#include "gl_types.h"p
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
