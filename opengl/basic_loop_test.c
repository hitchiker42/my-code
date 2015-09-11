#include "common.h"
#include "basic_loop.c"
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
const char *square_vertex_shader_source =
  "#version 330 core\n"
  "layout(row_major) uniform;\n"
  "layout(std_140) uniform;\n"
  "layout(location = 0) in vec3 position;\n"
  "uniform uniform_block {\n"
  "  uniform vec4 ucolor;\n"
  "  uniform mat2 rot;\n"
  "  uniform vec2 shift;\n"
  "  //8 bytes of padding\n"
  "};\n"
  "out vec4 v_color;\n"
  "void main(){\n"
  "  gl_Position.xyz = position;\n"//*scale;\n"
  "  gl_Position.w = 1.0f;\n"
  "  gl_Position.xy *= rot;\n"
  "  gl_Position.xy += shift;\n"
  "  v_color = ucolor;\n"
  //"  v_color = vec4(1.0,0.0,0.5,0.5);\n"
  "}\n";
const char *fragment_shader_source =
  "#version 330 core\n"
  "in vec4 v_color;\n"
  "out vec4 color;\n"
  "void main(){\n"
  "  color = v_color;\n"
  "}\n";
struct userdata {
  uint8_t i;
  float theta;
  float cos_theta;
  float sin_theta;
};
static struct uniforms {
  float ucolor[4];
  float rot[4];
  float scale[2];
  uint32_t padding[2];
};
struct uniforms uniforms = {{0.25,0.5,1.0,0.5},{0},{0}};
static struct vertex data[6] = //triangle
  {{.pos = {1.0, -1.0}, .color = {1.0, 1.0, 0.0, 0.5}}, 
   {.pos = {-1.0, -1.0}, .color = {0.0, 1.0, 1.0, 0.5}}, 
   {.pos = {0.0, 1.0}, .color = {1.0, 0.0, 1.0, 0.5}}, 
   {.pos = {-1.0, 1.0}, .color = {0.0, 1.0, 1.0, 0.5}}, 
   {.pos = {1.0, 1.0}, .color = {1.0, 0.0, 1.0, 0.5}}, 
   {.pos = {0.0, -1.0}, .color = {1.0, 1.0, 0.0, 0.5}}};
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
static struct position square[4] =
  {{.x=-0.5, .y=-0.5f}, {.x=0.5, .y=-0.5}, {.x=-0.5, .y=0.5}, {.x=0.5, .y=0.5}}
void update_userdata(struct userdata *data){
  data->i++;
  data->theta = 2*M_PI*(i/255.0f);
  sincosf(data->theta, &data->cos_theta, &data->sin_theta);
}
void update_buffer_1(gl_buffer *buf, struct userdata *data){
  uint8_t i = data->i;
  struct vertex *verts = buf->vertices;
  verts[0].r = i/255.0f; verts[1].g = i/255.0f; verts[2].b = i/255.0f;
  verts[3].g = i/255.0f; verts[4].b = i/255.0f; verts[5].r = i/255.0f;
  glBufferSubData(GL_ARRAY_BUFFER, 0, 6*sizeof(struct vertex), verts);
}
void update_scene_2(gl_scene *scene, userdata *data){
  struct uniforms *unis = scene->uniform_block;
  unis->rot[0] = unis->rot[3] = data->cos_theta;
  unis->rot[1] = data->sin_theta; unis->rot[2] = -data->sin_theta;
  unis->shift[0] = cos_theta; unis->shift[1] = sin_theta;
  glBufferSubData(GL_UNIFORM, 4*sizeof(float), 6*sizeof(float), &unis->rot);
  glUniformBlockBinding(scene->program, scene->uniform
  
int main(){
  GLFW *win = init_gl_context(800, 800, "basic_test");
  GLuint progams[2], VAO[2], buffers[3];
  
  
