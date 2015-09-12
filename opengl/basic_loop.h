#ifndef __BASIC_LOOP_H__
#define __BASIC_LOOP_H__
/*
  Needs a bettern name
*/
#ifdef __cplusplus
extern "C" {
#endif
typedef struct gl_scene gl_scene;
typedef struct global_context global_context;
typedef struct gl_buffer gl_buffer;
struct global_context {
  gl_window window;
  void *userdata;
  void(*update_userdata)(void*);
  gl_scene *scenes;
  int num_scenes;
};
struct gl_scene {
  global_context *ctx;
  GLuint program;
  GLuint VAO;
  //called with the program_context and userdata, mainly intended
  //to be used to update the uniform variables, program and VAO have
  //been bound by the time update_scene is called
  void(*update_scene)(gl_scene*,void*);
  gl_buffer *buffers;
  void *uniform_block;
  GLuint uniform_buffer;
  int uniform_block_index;
  int uniform_variable_size;
  int num_buffers;
  int scene_index;//used as the index for the uniform buffer binding
};
struct gl_buffer {
  gl_scene *scene;
  //called with the buffer and user data to update
  //the buffer data, if necessary
  void(*update_buffer)(gl_buffer*,void*);
  void *vertices;
  size_t index_offset;
  GLuint array_buffer;
  GLuint index_buffer;
  GLsizei draw_count;
  GLenum draw_mode;
  GLenum index_type;
};
void bind_scene(gl_scene *scene);
void bind_buffer(gl_buffer *buf);
void __attribute__((noreturn)) main_loop(global_context *ctx);
#ifdef __cplusplus
}
#endif
#endif /* __BASIC_LOOP_H__ */
