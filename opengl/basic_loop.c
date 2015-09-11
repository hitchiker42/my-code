#include "common.h"
/*
  An opengl program is specified by a global context, containing the
  opengl context information (via a window), a pointer to opaque user
  data, a callback used to update that data once per iteration of
  the main loop, and an array of scene objects.

  A scene is specified by an opengl shader program; a vertex array object;
  an array of uniform variables; a callback called with the scene and
  userdata once per iteration, generally used to update the uniform variables;
  and an array of buffer objects which contain the data to be rendered

  A buffer object contains an array buffer and an array element buffer;
  an opaque pointer to the data contained in that buffer, and
  a callback used to update the contents of the array / array element buffer
  when necessary.

  Shapes are implicitly contained in buffer objects, the vertices are
  in the array buffer and the layout of the shapes is described by the
  array element buffer.

  The flow of a program can be described as follows:
  Initialize opengl context (which creates a window by necessity)
  Initialize event handling callbacks
  Compile shaders
  Initialize user data
  Create VAO's and bufers
    bind attributes to VAOs and data to bufers
  Create a scene object for each program
    create a buffer object for each data buffer used by the scene
  Enter main loop

  The main loop can be described by the following psuedocode:
  while(!window_should_close(global_context->window)){
    global_context->update_userdata(global_context->userdata);
    foreach scene s in global_context->scenes {
      bind_scene(s);
      s->update_scene(s, global_context->userdata);
      foreach buffer b in s->buffers {
        bind_buffer(b);
        b->update_buffer(b, global_context->userdata);
        glDrawElements(b->draw_mode, b->draw_count,
                       b->index_type, (void*)b->index_offset);
      }
    }
    poll_events(global_context->window);
  }
  //possibly call a cleanup function

*/
struct global_context {
  gl_window *window;
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
  //to be used to update the uniform variables
  void(*update_scene)(gl_scene*,void*);
  gl_buffer *buffers;
  void *uniform_block;
  GLuint uniform_buffer;
  int uniform_block_index;
  int uniform_variable_size;
  int num_buffers;
};
struct gl_buffer {
  gl_program_context *ctx;
  //called with the buffer and user data to update
  //the buffer data, if necessary
  void(*update_buffer)(gl_buffer*,void*);
  void *vertices;
  size_t index_offset;
  GLuint array_buffer;
  GLuint index_buffer;
  GLsizei draw_count;
  GLenum draw_mode;
  GLenum index_type
};
void bind_scene(gl_scene *scene){
  glBindProgram(scene->program);
  glBindVertexArray(scene->VAO);
  glBindBuffer(GL_UNIFORM, scene->uniform_buffer);
  return;
}
void bind_buffer(gl_buffer *buf){
  glBindBuffer(GL_ARRAY_BUFFER, buf->array_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buf->index_buffer);
}
void __attribute__((noreturn)) main_loop(global_context *ctx){
  int i,j,k;
  while(!gl_window_should_close(cxt->window)){
    ctx->update_userdata(cxt->userdata);
    for(i=0;i<ctx->num_scenes){
      gl_scene *scene = ctx->scenes + i*sizeof(gl_scene*);
      //or gl_scene scene = ctx->scenes[i];
      bind_scene(scene);
      if(scene->update_scene){
        scene->update_scene(scene, ctx->userdata);
      }
      //bind uniform blocks
      for(j=0;j<scene->num_buffers;j++){
        gl_buffer *buf = scene->buffers + j*sizeof(gl_buffer);
        bind_buffer(buf);
        buf->update_buffer(buf, ctx->userdata);
        glDrawElements(buf->draw_mode, buf->draw_count,
                       buf->index_type, buf->index_offset);
      }
    }
    gl_swap_buffers(ctx->window);
    gl_poll_events(ctx->window);
  }
  exit(0);//need a way to specify an exit value
}
