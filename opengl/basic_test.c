#include "gl_util.h"
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
const char *fragment_shader_source =
  "#version 330 core\n"
  "in vec4 v_color;\n"
  "out vec4 color;\n"
  "void main(){\n"
  "  color = v_color;\n"
  "}\n";
static struct vertex data[3] = //triangle
  {{.x = -1.0f, .y = -1.0f, .r = 1.0f, .g = 1.0f, .b = 0.0f, .a = 0.5f},
   {.x = 1.0f, .y = -1.0f, .r = 0.0f, .g = 1.0f, .b = 1.0f, .a = 0.5f},
   {.x = 0.0f, .y = 1.0f, .r = 1.0f, .g = 0.0f, .b = 1.0f, .a = 0.5f}};
static struct vertex data_2[3] = //upside down triangle
  {{.x = -1.0f, .y = 1.0f, .r = 0.0f, .g = 1.0f, .b = 1.0f, .a = 0.5f},
   {.x = 1.0f, .y = 1.0f, .r = 1.0f, .g = 0.0f, .b = 1.0f, .a = 0.5f},
   {.x = 0.0f, .y = -1.0f, .r = 1.0f, .g = 1.0f, .b = 0.0f, .a = 0.5f}};
void draw_triangle(GLuint buffer){
  bind_vertex_attrib(buffer, 0, 3, GL_FLOAT, 0,
                     sizeof(struct vertex), NULL);
  bind_vertex_attrib(buffer, 1, 4, GL_FLOAT, 0,
                     sizeof(struct vertex), offsetof(struct vertex, color));
  //specify the location of a uniform variable
  //this variable doesn't really do anything, but that's ok
  //  glUniform1f(loc[2], 1.0f);//set uniform variable to a single float
  glDrawArrays(GL_TRIANGLES, 0, 3);
  unbind_vertex_attrib_2(0,1);
}
void main_loop(GLFWwindow *window, GLuint program){
  GLuint buffers[2];
  buffers[0] = make_data_buffer(data, sizeof(data), GL_STATIC_DRAW);
  buffers[1] = make_data_buffer(data_2, sizeof(data_2), GL_STATIC_DRAW);
  while(!glfwWindowShouldClose(window)){
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);//clear the old bufer
    glUseProgram(program);
    draw_triangle(buffers[0]);
    draw_triangle(buffers[1]);
    glfwSwapBuffers(window);//switch buffers, we're using double buffering
    glfwPollEvents();
  }
}
int main(int argc, char *argv[]){
  GLFWwindow *win = init_gl_context(1024,768,"basic_test");
  //dunno why I need to do this but I'm pretty sure I do
  GLuint VAO_id;
  glGenVertexArrays(1, &VAO_id);
  glBindVertexArray(VAO_id);
  //compile shaders into program
  GLuint program = create_shader_program(vertex_shader_source,
                                         fragment_shader_source);
  main_loop(win, program);//draw stuff
}
