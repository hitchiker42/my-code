#include "common.h"
const char *vertex_shader_source =
  "#version 330 core\n"
  "layout(location = 0) in vec2 position;\n"
  "layout(location = 1) in vec4 color;\n"
  "out vec4 v_color;\n"
  //  "uniform float scale;\n"
  "void main(){\n"
  "  gl_Position.xy = position;\n"//*scale;\n"
  "  gl_Position.zw = vec2(0.0f, 1.0f);\n"
  "  v_color = color;\n"
  "}\n";
const char *square_vertex_shader_source =
  "#version 330 core\n"
  "layout(location = 0) in vec3 position;\n"
  //  "uniform vec4 rot;\n"
  "uniform vec4 ucolor;\n"
  "out vec4 v_color;\n"
  "void main(){\n"
  "  gl_Position.xyz = position;\n"//*scale;\n"
  "  gl_Position.w = 1.0f;\n"
  "  v_color = ucolor;\n"
  "}\n";
const char *fragment_shader_source =
  "#version 330 core\n"
  "in vec4 v_color;\n"
  "out vec4 color;\n"
  "void main(){\n"
  "  color = v_color;\n"
  "}\n";
static struct vertex data[6] = //triangle
  {{.x = -1.0f, .y = -1.0f, .r = 1.0f, .g = 1.0f, .b = 0.0f, .a = 0.5f},
   {.x = 1.0f, .y = -1.0f, .r = 0.0f, .g = 1.0f, .b = 1.0f, .a = 0.5f},
   {.x = 0.0f, .y = 1.0f, .r = 1.0f, .g = 0.0f, .b = 1.0f, .a = 0.5f},
   {.x = -1.0f, .y = 1.0f, .r = 0.0f, .g = 1.0f, .b = 1.0f, .a = 0.5f},
   {.x = 1.0f, .y = 1.0f, .r = 1.0f, .g = 0.0f, .b = 1.0f, .a = 0.5f},
   {.x = 0.0f, .y = -1.0f, .r = 1.0f, .g = 1.0f, .b = 0.0f, .a = 0.5f}};
static struct position square[4] =
  {{.x=-0.5,.y=-0.5f},{.x=0.5,.y=-0.5},{.x=-0.5,.y=0.5},{.x=0.5,.y=0.5}};
static float rot[4] = {0.0f,1.0f,1.0f,0.0f};
static float sq_color[4] = {1.0f, 0.0f, 0.5f, 0.5f};
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
  GLuint buffers[2];
  glGenVertexArrays(2, VAO);
  glBindVertexArray(VAO[0]);
  buffers[0] = make_data_buffer(data, sizeof(data), GL_STATIC_DRAW);
  bind_vertex_attrib(buffers[0], 0, 3, GL_FLOAT, 0,
                     sizeof(struct vertex), NULL);
  bind_vertex_attrib(buffers[0], 1, 4, GL_FLOAT, 0,
                     sizeof(struct vertex), offsetof(struct vertex, color));
  glBindVertexArray(VAO[1]);
  buffers[1] = make_data_buffer(square, sizeof(square), GL_STATIC_DRAW);
  bind_vertex_attrib(buffers[1], 0, 3, GL_FLOAT, 0, 0, NULL);
  //  GLint rot_loc = glGetUniformLocation(program2, "rot");
  GLint color_loc = glGetUniformLocation(program2, "vcolor");
  uint8_t i=0;
  while(!glfwWindowShouldClose(window)){
    //modify data
    i++;//implicitly wraps arroud at i=256
    data[0].r = i/255.f; data[1].g = i/255.f; data[2].b = i/255.f;
    data[3].g = i/255.f; data[4].b = i/255.f; data[5].r = i/255.f;
    
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);//clear the old bufer
    glBindVertexArray(VAO[0]);
    glUseProgram(program);
    glBindBuffer(GL_ARRAY_BUFFER, buffers[0]);
    glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(data), data);    
    draw_triangles(buffers[0], 2);
    
    glBindVertexArray(VAO[1]);
    glUseProgram(program2);
    glBindBuffer(GL_ARRAY_BUFFER, buffers[1]);
    glUniform4fv(color_loc, 4, sq_color);
    glDrawArrays(GL_TRIANGLES, 0, 4);
    
    glfwSwapBuffers(window);//switch buffers, we're using double buffering
    glfwPollEvents();
  }
}
int main(int argc, char *argv[]){
  GLFWwindow *win = init_gl_context(1024,768,"basic_test");
  //compile shaders into program
  GLuint program = create_shader_program(vertex_shader_source,
                                         fragment_shader_source, 0);
  GLuint square_program = create_shader_program(square_vertex_shader_source,
                                                fragment_shader_source, 0);
  glfwSetKeyCallback(win, quit_on_esc);
  main_loop(win, program, square_program);//draw stuff
}
