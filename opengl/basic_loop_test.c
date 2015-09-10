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
  "layout(location = 0) in vec3 position;\n"
  //  "uniform vec4 rot;\n"
  "uniform vec4 ucolor;\n"
  "uniform vec2 shift;\n"
  "out vec4 v_color;\n"
  "void main(){\n"
  "  gl_Position.xyz = position;\n"//*scale;\n"
  "  gl_Position.w = 1.0f;\n"
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
static struct vertex data[6] = //triangle
  {{.x = -1.0f, .y = -1.0f, .r = 1.0f, .g = 1.0f, .b = 0.0f, .a = 0.5f},
   {.x = 1.0f, .y = -1.0f, .r = 0.0f, .g = 1.0f, .b = 1.0f, .a = 0.5f},
   {.x = 0.0f, .y = 1.0f, .r = 1.0f, .g = 0.0f, .b = 1.0f, .a = 0.5f},
   {.x = -1.0f, .y = 1.0f, .r = 0.0f, .g = 1.0f, .b = 1.0f, .a = 0.5f},
   {.x = 1.0f, .y = 1.0f, .r = 1.0f, .g = 0.0f, .b = 1.0f, .a = 0.5f},
   {.x = 0.0f, .y = -1.0f, .r = 1.0f, .g = 1.0f, .b = 0.0f, .a = 0.5f}};
static struct vertex data[6] = //triangle
  {{.pos = {1.0, -1.0}, .color = {1.0,1.0,0.0,0.5}},
   {.pos = {1.0, -1.0}, .color = {0.0,1.0,1.0,0.5}},
   {.pos = {0.0, 1.0}, .color = {1.0,0.0,1.0,0.5}},
   {.pos = {-1.0, 1.0}, .color = {0.0,1.0,1.0,0.5}},
   {.pos = {1.0, 1.0}, .color = {1.0,0.0,1.0,0.5}},
   {.pos = {0.0, -1.0}, .color = {1.0,1.0,0.0,0.5}};
static struct position square[4] =
  {{.x=-0.5,.y=-0.5f},{.x=0.5,.y=-0.5},{.x=-0.5,.y=0.5},{.x=0.5,.y=0.5}}
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
  
int main(){
  GLFW *win = init_gl_context(800,800,"basic_test");
  GLuint progams[2], VAO[2], buffers[3];
  
  
