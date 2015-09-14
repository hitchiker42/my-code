#include "common.h"
#include "cblas_util.h"
#include "context.h"
const char *vertex_shader_source =
  "#version 330 core\n"
  "layout(location = 0) in vec3 position;\n"
  "layout(location = 1) in vec4 color;\n"
  "out vec4 v_color;\n"
  //  "uniform float scale;\n"
  "void main(){\n"
  "  gl_Position.xyz = position;\n"//*scale;\n"
  "  gl_Position.w = 1.0f;\n"
  "  v_color = color;\n"
  "}\n";
const char *cube_vertex_shader_source =
  "#version 330 core\n"
  "layout(row_major) uniform;\n"
  "layout(std_140) uniform;\n"
  "layout(location = 0) in vec3 position;\n"
  "uniform uniform_block {\n"
  "  uniform vec4 ucolor;\n"
  "  uniform mat4 transform;\n"
  "};\n"
  "out vec4 v_color;\n"
  "void main(){\n"
  "  gl_Position.xyzw = vec4(position,1.0f);\n"//*scale;\n"
  "  gl_Position *= transform;\n"
  "  v_color = ucolor;\n"
  "}\n";
const char *fragment_shader_source =
  "#version 330 core\n"
  "in vec4 v_color;\n"
  "out vec4 color;\n"
  "void main(){\n"
  "  color = v_color;\n"
  "}\n";
struct userdata {
  float theta;
  float cos_theta;
  float sin_theta;
};
static struct uniforms {
  float ucolor[4];
  float transform[16];
};
static struct vertex shapes[10] = //triangles + a Square
  {{.pos = {1.0, -1.0}, .color = {1.0, 1.0, 0.0, 0.5}},
   {.pos = {-1.0, -1.0}, .color = {0.0, 1.0, 1.0, 0.5}},
   {.pos = {0.0, 1.0}, .color = {1.0, 0.0, 1.0, 0.5}},
   {.pos = {-1.0, 1.0}, .color = {0.0, 1.0, 1.0, 0.5}},
   {.pos = {1.0, 1.0}, .color = {1.0, 0.0, 1.0, 0.5}},
   {.pos = {0.0, -1.0}, .color = {1.0, 1.0, 0.0, 0.5}},
   //square (more like a diamond
   {.pos = {-0.5,  0, 0.1}, .color = {0,0,0,0.5}},
   {.pos = {0.0, 0.5, 0.1}, .color = {1.0, 1.0, 1.0, 0.5}},
   {.pos = {0.0, -0.5, 0.1}, .color = {1.0, 1.0, 1.0, 0.5}},
   {.pos = {0.5,  0, 0.1}, .color = {0,0,0,0.5}}};
struct color cube_color_1 = {.r = 0.0, .g = 0.5, .b = 1.0, .a = 0.5};
struct color cube_color_2 = {.r = 0.0, .g = 1.0, .b = 0.5, .a = 0.5};
struct vertex cube[8] =
  {{.pos = {-0.25, 0.25, 0.5}, .color = cube_color_1},
   {.pos = {0.25, 0.25, 0.5}, .color = cube_color_1},
   {.pos = {-0.25, -0.25, 0.5}, .color = cube_color_1},
   {.pos = {0.25, -0.25, 0.5}, .color = cube_color_1},
   {.pos = {-0.125, -0.125, 0}, .color = cube_color_2},
   {.pos = {0.375, -0.125, 0}, .color = cube_color_2},
   {.pos = {-0.125, 0.375, 0}, .color = cube_color_2},
   {.pos = {0.375, 0.375, 0}, .color = cube_color_2}};
uint8_t cube_indices[17] =
  {0, 1, 2, 3, 4, 5, 6, 7, 0xff, 2, 4, 0, 6, 1, 7, 3, 5};
uint8_t shape_indices[12] =
  {0, 1, 2, 0xff, 3, 4, 5, 0xff, 6, 7, 8, 9}
void update_userdata(struct userdata *data){
  data->theta = fmod((data->theta  + (2*M_PIf/256)),(2*M_PIf));
  sincosf(data->theta, &data->cos_theta, &data->sin_theta);
}
void update_scene(gl_scene *scene, userdata *data){
  struct uniforms *uniforms = scene->uniform_block;
  glBufferSubData(GL_UNIFORM, 0, sizeof(struct uniforms), uniforms);
}
gl_buffer *init_shape_buffer(gl_scene *scene){
  gl_buffer *buf = make_gl_buffer(shapes, sizeof(shapes), GL_STATIC_DRAW,
                                  shape_indices, sizeof(shape_indices), GL_STATIC_DRAW);
  buf->index_type = GL_UNSIGNED_BYTE;
  buf->draw_mode = GL_TRIANGLE_STRIP;
  buf->draw_count = sizeof(shape_indices/sizeof(uint8_t));
  return buf;
}
gl_buffer *init_cube_buffer(){
  gl_buffer *buf = make_gl_buffer(cube, sizeof(cube), GL_STATIC_DRAW,
                                  cube_indices, sizeof(cube_indices), GL_STATIC_DRAW);
  buf->index_type = GL_UNSIGNED_BYTE;
  buf->draw_mode = GL_TRIANGLE_STRIP;
  buf->draw_count = sizeof(cube_indices/sizeof(uint8_t));
  return buf;
}

gl_scene *init_shape_scene(gl_scene *scene){
  scene->program = create_shader_program(vertex_shader_source, fragment_shader_source, 0);
  gl_bufer *buf = init_shape_buffer();
  buf->scene = scene;
  scene->buffers = buf;
  scene->num_buffers = 1;
  return scene;
}
gl_scene *init_cube_scene(gl_scene *scene){
  scene->program = create_shader_program(cube_vertex_shader_source, fragment_shader_source, 0);
  gl_bufer *buf = init_cube_buffer();
  buf->scene = scene;
  scene->buffers = buf;
  scene->num_buffers = 1;
  //this is the location of the uniform block in the program, akin to calling
  //glGetUniformLocation for an indivual uniform variable.
  scene->uniform_block_index = glGetUniformBlockIndex(scene->program, "uniform_block");
  //tell the program where to look for the data for the uniform block
  glUniformBlockBinding(scene->program, scene->uniform_block_index, scene->scene_index);
  struct uniforms *uniforms = zmalloc(sizeof(struct uniforms));
  uniforms->ucolor = {0.3,0.6,0.9,0.5}:
  uniforms->transform = I_4(1.0);
  scene->uniform_block = uniforms;
  scene->update_scene = update_scene;
  return scene;
}
global_context *init_global_context(){
  global_context *ctx = make_global_context(800,800,"test");
  //we only need one VAO
  GLuint VAO;
  glGenVertexArray(1,&VAO);
  glBindVertexArray(scene->VAO);
  ctx->scenes = zmalloc(2*sizeof(gl_scene));
  init_shape_scene(ctx->scenes);
  init_cube_scene(&ctx->scenes[1]);
  ctx->num_scenes = 2;
  ctx->userdata = zmalloc(sizeof(struct userdata));
  /*
    It's really annoying that Vertex attributes are tied to buffer data, this is fixed
    in the more recent versions of opengl, which makes it even worse.
  */
  bind_vertex_attrib(ctx->scenes->buffers[0], 0, 3, GL_FLOAT, 0,
                     sizeof(struct vertex), NULL);
  bind_vertex_attrib(ctx->scenes->buffers[0], 1, 4, GL_FLOAT, 0,
                     sizeof(struct vertex), offsetof(struct vertex, color));
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(0xff);
  return ctx;
}

int main(){
  global_context *ctx = init_global_context();
  main_loop(ctx);
}
