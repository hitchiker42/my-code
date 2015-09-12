#include "common.h"
#include "basic_loop.h"
#include <cblas.h>
const char *vertex_shader =
  "#version 330 core\n"
  "layout(row_major) uniform;\n"
  "layout(location = 0) in vec3 position;\n"
  "layout(location = 1) in vec4 color;\n"
  "uniform mat4 transform;\n"
  "out vec4 v_color;\n"
  "void main(){\n"
  "  gl_Position.xyzw = vec4(position,1.0f);\n"
  "  gl_Position *= transform;\n"
  "  v_color = color;\n"
  "}\n";
const char *fragment_shader =
  "#version 330 core\n"
  "in vec4 v_color;\n"
  "out vec4 f_color;\n"
  "void main(){\n"
  "  f_color = v_color;\n"
  "}\n";
struct userdata {
  float theta;
  float cos_theta;
  float sin_theta;
};
//non c++ compatible initializers
struct userdata rot_angle = {0,1,0};
float rotation_matrix[16] = I_4;
float shift_matrix[16] = I_4;
float transform_matrix[16];
/*
      6 -- 7  (4 is hidden)
     /|   /|
    0----1 5
    |/   |/
    2----3

    This is the best I can do, I think we need one restart
    draw the front face: (0,1,2), (1,2,3)
    draw the bottom face: (2,3,4), (3,4,5)
    draw the rear face: (4,5,6), (5,6,7)
    restart:
    draw the left face: (2,4,0),(4,0,6)
    draw the top face: (0,6,1), (6,1,7)
    draw the right face: (1,7,3),(7,3,5)

*/
struct vertex cube[9] =
  {{.pos = {-0.25,0.25,0.25},   .color = {0.0,1.0,0.125,0.5}},
   {.pos = {0.25,0.25,0.25},    .color = {0.0,0.875,0.25,0.5}},
   {.pos = {-0.25,-0.25,0.25},  .color = {0.0,0.75,0.375,0.5}},
   {.pos = {0.25,-0.25,0.25},   .color = {0.0,0.625,0.5,0.5}},
   {.pos = {-0.25,0.25,-0.25},  .color = {0.0,0.5,0.0,625.5}},
   {.pos = {0.25,0.25,-0.25}, .  color = {0.0,0.375,0.75,0.5}},
   {.pos = {-0.25,-0.25,-0.25}, .color = {0.0,0.25,0.875,0.5}},
   {.pos = {0.25,-0.25,-0.25},  .color = {0.0,0.125,1.0,0.5}}};//end of cube
struct vertex triangle[3] = 
  {{.pos = {-1.0,-1.0,0.0},     .color = {1.0,1.0,1.0,0.25}},
   {.pos = {1.0,-1.0,0.0},     .color = {1.0,1.0,1.0,0.25}},
   {.pos = {0,1.0,0.0},     .color = {1.0,1.0,1.0,0.25}}};
/*  {{.pos = {-0.25,0.25,0.25}, .color = {0.0,0.0,0.0,0.5}},
   {.pos = {0.25,0.25,0.25}, .color = {0.5,0.0,0.0,0.5}},
   {.pos = {-0.25,-0.25,0.25}, .color = {0.0,0.5,0.0,0.5}},
   {.pos = {0.25,-0.25,0.25}, .color = {0.0,0.0,0.5,0.5}},
   {.pos = {-0.125,-0.125,-0.25}, .color = {0.5,0.5,0.0,0.5}},
   {.pos = {0.375,-0.125,-0.25}, .color = {0.0,0.5,0.5,0.5}},
   {.pos = {-0.125,0.375,-0.25}, .color = {0.5,0.0,0.5,0.5}},
   {.pos = {0.375,0.375,-0.25}, .color = {0.5,0.5,0.5,0.5}}};*/
uint8_t cube_indices[21] =
  {0,1,2,3,4,5,6,7,0xff,2,4,0,6,1,7,3,5,0xff,8,9,10};
void main_loop(global_context *ctx){
  gl_scene scene = ctx->scenes[0];
  bind_scene(&scene);
  GLint trans_loc = glGetUniformLocation(scene.program, "transform");
  while(!gl_window_should_close(ctx->window)){
    gl_buffer buf = scene.buffers[0];
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    scene.update_scene(&scene, ctx->userdata);
    bind_buffer(&buf);
    glUniformMatrix4fv(trans_loc, 1, 0, transform_matrix);
    glDrawElements(buf.draw_mode, buf.draw_count,
                   buf.index_type, (void*)buf.index_offset);
    gl_swap_buffers(ctx->window);
    gl_poll_events();
  }
  exit(EXIT_SUCCESS);
}
#define sgemm_square(n,a,A,B,b,C)               \
  cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, n, n, n, a,  \
              A, n, B, n, b, C, n);
//just do the updating of userdata here
void update_scene(gl_scene *scene, struct userdata *data){
  data->theta = fmod((data->theta  + (2*M_PIf/360)),(2*M_PIf));
  sincosf(data->theta, &data->sin_theta, &data->cos_theta);
  /* We're rotating around the y-z axis so the rotation matrix looks like:
     |1|0      |0     |0|
     |0|cos(θ) |sin(θ)|0|
     |0|-sin(θ)|cos(θ)|0|
     |0|0      |0     |1|
   */
  
  rotation_matrix[5] = rotation_matrix[10] = data->cos_theta;
  rotation_matrix[6] = data->sin_theta;
  rotation_matrix[9] = -data->sin_theta;

  shift_matrix[3] = data->cos_theta;
  shift_matrix[7] = data->sin_theta;
//  shift_matrix[11] = (data->theta - M_PIf)/M_PIf;
  sgemm_square(4, 1.0, shift_matrix, rotation_matrix, 0, transform_matrix);
}
gl_scene* init_scene(void){
  gl_scene *scene = xmalloc(sizeof(gl_scene));
  gl_buffer *buf = zmalloc(sizeof(gl_buffer));
  GLuint VAO, program, buffer[2];
  program = create_shader_program(vertex_shader, fragment_shader, 0);
  VAO = make_VAO();
  buffer[0] = make_array_buffer(cube, sizeof(cube), GL_STATIC_DRAW);
  buffer[1] = make_index_buffer(cube_indices, sizeof(cube_indices), GL_STATIC_READ);
  bind_vertex_attrib(buffer[0], 0, 3, GL_FLOAT, 0,
                     sizeof(struct vertex), NULL);
  bind_vertex_attrib(buffer[0], 1, 4, GL_FLOAT, 0,
                     sizeof(struct vertex), offsetof(struct vertex, color));
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(0xff);
  scene->program = program;
  scene->VAO = VAO;
  scene->num_buffers = 1;
  scene->buffers = buf;
  scene->update_scene = (void*)update_scene;

  //init buffer
  buf->scene = scene;
  buf->vertices = cube;
  buf->draw_mode = GL_TRIANGLE_STRIP;
  buf->index_type = GL_UNSIGNED_BYTE;
  buf->array_buffer = buffer[0]; buf->index_buffer = buffer[1];
  buf->draw_count = 17;
  return scene;
}
static global_context ctx[1];
int main(){
  gl_window win =  init_gl_context(800, 800, "cube");
  ctx->window = win;
  ctx->userdata = (void*)&rot_angle;
  ctx->scenes = init_scene();
  ctx->num_scenes = 1;
  main_loop(ctx);
}
