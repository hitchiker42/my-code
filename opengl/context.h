#ifndef __CONTEXT_H__
#define __CONTEXT_H__
#ifdef __cplusplus
extern "C" {
#endif
typedef struct gl_scene gl_scene;
typedef struct global_context global_context;
typedef struct gl_buffer gl_buffer;
//in general there should only be one of these
struct global_context {
  void *userdata;
  void(*update_userdata)(void*);
  gl_scene *scenes;
  gl_window window;//could be a int or a pointer
  int num_scenes;
};
struct gl_scene {
  global_context *ctx;
  void *scene_userdata;
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
};//68 bytes
struct gl_buffer {
  gl_scene *scene;
  void *buffer_userdata;
  //called with the buffer and user data to update
  //the buffer data, if necessary
  void(*update_buffer)(gl_buffer*,void*);
  void *vertices;
  void *indices;
  //Need to keep the buffers adjecent so they can both be generated
  //with one call to glGenBuffers
  union {
    struct {
      GLuint array_buffer;
      GLuint index_buffer;
    };
    GLuint buffers[2];
  };
  int vertices_size;
  int indices_size;
  size_t index_offset;
  GLsizei draw_count;
  GLenum draw_mode;
  GLenum index_type;
};//76 bytes
/*
  Functions to make the different structures, these allocate (and zero) the struct
  and initialize a few basic parts, most of the initializion is left up to the
  client program.
*/
/*
  Allocate and zero objects only
*/
global_context *allocate_global_context();
gl_scene *allocate_gl_scene();
gl_buffer *allocate_gl_buffer();
/*
  Allocates space for a global context, creates a window and sets the
  userdata pointer, the rest of the initialization is upto the program.
*/
global_context *make_global_context(int window_width, int window_height,
                                    void* userdata);
/*
  Allocates a gl_scene, compiles a program using the given vertex and
  fragment shader sources (which are strings containing the literal source)
  and initializes the VAO.
*/

gl_scene *make_simple_scene(const char *vertex_shader, const char *fragment_shader);
/*
  Allocates a gl_buffer, creates and sets the context of the array
  and index buffers.
*/
gl_buffer *make_gl_buffer(void *vertices, size_t vertices_size, GLenum vertex_usage,
                          void *indices, size_t indices_size, GLenum index_usage);
void bind_scene(gl_scene *scene);
void bind_buffer(gl_buffer *buf);
void __attribute__((noreturn)) main_loop(global_context *ctx);
#ifdef __cplusplus
}
#endif
#endif /* __BASIC_LOOP_H__ */
