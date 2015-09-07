#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <GL/glew.h>
#include <GLFW/glfw3.h>
//this should be fine since gl uses camelcase
typedef struct vertex gl_vertex;
//simple vertex, only xy, rgb
typedef struct svertex gl_svertex;
typedef struct position gl_position;//xyzw
typedef struct point gl_point; //just x and y
typedef struct rgba_f gl_rgba; //rgba, as floats
struct point {
  float x;
  float y;
};
struct position {
  float x;
  float y;
  float z;
  float w;
};
struct rgba_f {
  float r;
  float g;
  float b;
  float a;
};
struct vertex {
  union {
    struct position position;
    struct {
      float x;
      float y;
      float z;
      float w;
    };
  };
  union {
    struct rgba_f rgba;
    struct {
      float r;
      float g;
      float b;
      float a;
    };
  };
};
//This will probably be used more that the actual vertex struct
struct svertex {
  float x;
  float y; 
  float r;
  float g;
  float b;
  float a;
};
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
