#ifndef __GL_UTIL_H__
#define __GL_UTIL_H__
#ifndef __MY_GL_TYPES_H__
#error "gl_types.h must be included before gl_util.h\n" \
  "You should probably just include common.h instead\n"
#endif
#include "common.h"
#ifdef __cplusplus
extern "C" {
#endif
#if (defined DEBUG) && !(defined NDEBUG)
#define DEBUG_PRINTF(msg,args...) fprintf(stderr, msg, ##args)
#define IF_DEBUG(code) code
#else
#define DEBUG_PRINTF(msg,args...)
#define IF_DEBUG(code)
#endif
/*
  Since most gl numbers are floats define float versions of 
  some mathematical constants.
*/
#define __CAT(x,y) x ## y
#define CAT(x,y) __CAT(x,y)
#define M_PIf CAT(M_PI, f)
//make programs bind the same as every other kind of object
#define glBindProgram(p) glUseProgram(p)
/*
  functions to control glut/glfw
*/
/*
  Initialize glut/glfw, create a window and opengl context, then
  initialize glew and insure opengl3.3 is supported
*/
gl_window init_gl_context(int w, int h, const char* name);
void gl_swap_buffers(gl_window window);
void gl_poll_events();
int gl_window_should_close(gl_window window);
/*
  Arguments need to be null terminated strings.
  Could be eaisly modified to take more arguments.
*/
GLuint create_shader_program(const char *vertex_shader_source,
                             const char *fragment_shader_source,
                             int is_pathname);
/*
  Clear the screen, call draw, swap buffers and then poll events
*/
void glfw_main_loop(GLFWwindow *win, void(*draw)(void*), void *userdata);
//create a buffer on the gpu and bind the given data to it
GLuint make_data_buffer(GLenum buffer_type, void *data,
                        size_t size, int usage);
/*
  If you need more than one VAO it's probably better to explicitly
  call glGenVertexArray.
*/
static inline GLuint make_VAO(void){
  GLuint VAO;
  glGenVertexArrays(1, &VAO);
  glBindVertexArray(VAO);
  return VAO;
}
#define make_array_buffer(data,size,usage)              \
  make_data_buffer(GL_ARRAY_BUFFER, data, size, usage)
#define make_index_buffer(data,size,usage)                      \
  make_data_buffer(GL_ELEMENT_ARRAY_BUFFER, data, size, usage)

/*
  the opengl equlivent of this takes the offset as a pointer, not a pointer
  to an int, but the offset itself is a void*, which is weird.

  stride is the size of the structure the attributes are stored in.
*/
void bind_vertex_attrib(GLuint buffer, GLint loc, int size, GLenum type,
                        int normalized, size_t stride, size_t offset);
//this is a pretty trivial wrapper functione
static inline void unbind_vertex_attrib(GLint loc){
  glDisableVertexAttribArray(loc);
}
#define unbind_vertex_attrib_2(loc1, loc2)      \
  unbind_vertex_attrib(loc1);                   \
  unbind_vertex_attrib(loc2);
#define unbind_vertex_attrib_3(loc1, loc2, loc3)        \
  unbind_vertex_attrib_2(loc1, loc2);                   \
  unbind_vertex_attrib(loc3);
/*
  This should be evaluated at compile time, but if not the same think
  can be accomplished by macros, albeit in a really clunky way.
*/
static inline __attribute__((const, optimize(3)))
const gl_color rgba_float(uint32_t rgba){
  gl_color ret;
#ifdef __LITTLE_ENDIAN
  ret.a = (rgba & 0xff); rgba >>= 8;
  ret.b = (rgba & 0xff); rgba >>= 8;
  ret.g = (rgba & 0xff); rgba >>= 8;
  ret.r = (rgba & 0xff);
#else //big endian
  ret.r = (rgba & 0xff); rgba >>= 8;
  ret.g = (rgba & 0xff); rgba >>= 8;
  ret.b = (rgba & 0xff); rgba >>= 8;
  ret.a = (rgba & 0xff);
#endif
  return ret;
}
/*
  Info about drawing modes (1st argument to glDrawArrays)
  GL_LINES, every set of 2 vertices specifies a line
  GL_LINE_STRIP, a line is drawn between each adjecent vertex,
  i.e 1->2->3->4, vs 1->2,3->4 for GL_LINE
  GL_LINE_LOOP, same as GL_LINE_STRIP except an extra line is
  drawn between the 1st and last point (i.e 4->1->2->3->4)

  GL_TRIANGLES, every set of 3 vertices specifies a triangle
  GL_TRIANGLE_STRIP, every set of 3 adjecent vertices specifies a triangle,
  i.e ((1,2,3),(2,3,4),(3,4,5),(4,5,6)) vs ((1,2,3),(4,5,6)) for TRIANGLES
  GL_TRIANGLE_FAN, the first vertex is fixed and a triangle is drawn
  with the first vertex and each set of 2 adjecent vertices,
  i.e ((1,2,3),(1,3,4),(1,4,5),(1,5,6))

  GL_PATCH, uses user specified tessalation shader

  A notable function is glPrimitiveRestartIndex, which works with 
  indexed drawing functions, it sets a sentinel value which when
  encounteded in the index array restarts the set of vertices being
  used with the next vertex.

  It's probably best to use glDrawELementsxs
*/
#ifdef __cplusplus
}
#endif
#endif
