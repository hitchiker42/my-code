#include "common.h"
struct global_context {
  gl_window *window;
  void *userdata;
  void(*update_userdata)(void*);
  gl_program_context *contexts;
  int num_contexts;
};
struct gl_program_context {
  global_context *ctx;
  GLuint program;
  GLuint VAO;
  gl_buffers *buffers;
  int num_buffers;
};
struct gl_buffer {
  gl_program_context *ctx;
  void *data;
  int *shape_offsets;
  //Since vertices might be shared between shapes shap sizes
  //can't just be calculated via offsets
  int *shape_sizes;
  int num_shapes;
  GLuint buffer;
  void(*update_buffer)(gl_buffer*,void*);
};

