#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <GL/glew.h>
#include <GLFW/glfw3.h>

/*
  Combine several glut init steps into one.
*/
#if (defined DEBUG) && !(defined NDEBUG)
#define DEBUG_PRINTF(msg,args...) fprintf(stderr, msg, ##args)
#else
#define DEBUG_PRINTF(msg,args...)
#endif
/*
  Initialize glfw, create a window and opengl context, then
  initialize glew and insure opengl3.3 is supported
*/
GLFWwindow* init_gl_context(int w, int h, const char* name);
/*
  Arguments need to be null terminated strings.
  Could be eaisly modified to take more arguments.
*/
GLuint create_shader_program(const char *vertex_shader_source,
                             const char *fragment_shader_source);
void glfw_main_loop(GLFWwindow *win, void(*draw)(void*), void *userdata);
