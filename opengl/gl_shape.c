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
/*void draw_shape(gl_shape *shape){
  //assume buffer data to upto date
  bind_vertex_attrib(buffer, 0, 3, GL_FLOAT, 0,
                     sizeof(struct vertex), NULL);
  bind_vertex_attrib(buffer, 1, 4, GL_FLOAT, 0,
                     sizeof(struct vertex), offsetof(struct vertex, color));
  glDrawArrays(GL_TRIANGLES, first, count, num_triangles);
  }*/
void draw_triangles(GLuint buffer, int num_triangles){
  bind_vertex_attrib(buffer, 0, 3, GL_FLOAT, 0,
                     sizeof(struct vertex), NULL);
  bind_vertex_attrib(buffer, 1, 4, GL_FLOAT, 0,
                     sizeof(struct vertex), offsetof(struct vertex, color));
  int *first = alloca(num_triangles), *count = alloca(num_triangles);
  int i;
  for(i=0;i<num_triangles;i++){
    first[i] = i*3;
    count[i] = 3;
  }
  glMultiDrawArrays(GL_TRIANGLES, first, count, num_triangles);
  unbind_vertex_attrib_2(0,1);
}
void draw_square(GLuint buffer, 
