#include "common.h"
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
static struct vertex data[6] = //triangle
  {{.x = -1.0, .y = -1.0, .r = 1.0, .g = 1.0, .b = 0.0, .a = 0.5},
   {.x = 1.0, .y = -1.0, .r = 0.0, .g = 1.0, .b = 1.0, .a = 0.5},
   {.x = 0.0, .y = 1.0, .r = 1.0, .g = 0.0, .b = 1.0, .a = 0.5},
   {.x = -1.0, .y = 1.0, .r = 0.0, .g = 1.0, .b = 1.0, .a = 0.5},
   {.x = 1.0, .y = 1.0, .r = 1.0, .g = 0.0, .b = 1.0, .a = 0.5},
   {.x = 0.0, .y = -1.0, .r = 1.0, .g = 1.0, .b = 0.0, .a = 0.5}};
static struct position square[4] =
  {{.x=-0.25,.y=-0.25f},{.x=0.25,.y=-0.25},{.x=-0.25,.y=0.25},{.x=0.25,.y=0.25}};
static float sq_color[4] = {0.0f, 0.5f, 0.5f, 0.5f};
static float rot_matrix[4];
static float shift_matrix[2];
static uint8_t indices[] = {0,1,2,3,4,5,6};
void quit_on_esc(GLFWwindow *window, int key, int scancode, int action, int mods){
  if(key == GLFW_KEY_ESCAPE && action == GLFW_RELEASE){
    glfwSetWindowShouldClose(window,0);
  }
  return;
}
  //specify the location of a uniform variable
  //this variable doesn't really do anything, but that's ok
  //  glUniform1f(loc[2], 1.0f);//set uniform variable to a single float
void draw_triangles(GLuint buffer, int num_triangles){
  int *first = alloca(num_triangles), *count = alloca(num_triangles);
  int i;
  for(i=0;i<num_triangles;i++){
    first[i] = i*3;
    count[i] = 3;
  }
  glMultiDrawArrays(GL_TRIANGLES, first, count, num_triangles);
  //  unbind_vertex_attrib_2(0,1);
}
void main_loop(GLFWwindow *window, GLuint program, GLuint program2){
  GLuint VAO[2];
  GLuint buffers[3];
  glGenVertexArrays(2, VAO);
  glBindVertexArray(VAO[0]);
  buffers[2] = make_index_buffer(indices, sizeof(indices), GL_STATIC_READ);
  buffers[0] = make_array_buffer(data, sizeof(data), GL_STATIC_DRAW);
  bind_vertex_attrib(buffers[0], 0, 3, GL_FLOAT, 0,
                     sizeof(struct vertex), NULL);
  bind_vertex_attrib(buffers[0], 1, 4, GL_FLOAT, 0,
                     sizeof(struct vertex), offsetof(struct vertex, color));
  glBindVertexArray(VAO[1]);
  buffers[1] = make_array_buffer(square, sizeof(square), GL_STATIC_DRAW);
  bind_vertex_attrib(buffers[1], 0, 3, GL_FLOAT, 0, 0, NULL);
  //  GLint rot_loc = glGetUniformLocation(program2, "rot");
  uint8_t i=0;
  float theta = 0, sin_theta, cos_theta;
  GLint color_loc = glGetUniformLocation(program2, "ucolor");
  GLint shift_loc = glGetUniformLocation(program2, "shift");
  GLint rot_loc = glGetUniformLocation(program2, "rot");
  while(!glfwWindowShouldClose(window)){
    //modify data
    i++;//implicitly wraps arroud at i=256
    data[0].r = i/255.f; data[1].g = i/255.f; data[2].b = i/255.f;
    data[3].g = i/255.f; data[4].b = i/255.f; data[5].r = i/255.f;
    theta = 2*M_PI*(i/255.0f);
    sincosf(theta, &sin_theta, &cos_theta);
    rot_matrix[0] = cos_theta, rot_matrix[1] = sin_theta;
    rot_matrix[2] = -sin_theta, rot_matrix[3] = cos_theta;
    shift_matrix[0] = cos_theta/2, shift_matrix[1] = sin_theta/2;
    
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);//clear the old bufer
    glBindVertexArray(VAO[0]);
    glUseProgram(program);
    glBindBuffer(GL_ARRAY_BUFFER, buffers[0]);    
    glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(data), data);    
    glDrawElements(GL_TRIANGLES,6,GL_UNSIGNED_BYTE, (void*)0);
    
    glBindVertexArray(VAO[1]);
    glUseProgram(program2);
    glBindBuffer(GL_ARRAY_BUFFER, buffers[1]);
    glUniform4fv(color_loc, 1, sq_color);
    glUniform2fv(shift_loc, 1, shift_matrix);
    glUniformMatrix2fv(rot_loc, 1,0, rot_matrix);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);    
    glfwSwapBuffers(window);//switch buffers, we're using double buffering
    glfwPollEvents();
  }
}
int main(int argc, char *argv[]){
  GLFWwindow *win = init_gl_context(800,800,"basic_test");
  //compile shaders into program
  GLuint program = create_shader_program(vertex_shader_source,
                                         fragment_shader_source, 0);
  GLuint square_program = create_shader_program(square_vertex_shader_source,
                                                fragment_shader_source, 0);
  glfwSetKeyCallback(win, quit_on_esc);
  main_loop(win, program, square_program);//draw stuff
}
