#include "gl_util.h"
const char *vertex_shader_source =
  "#version 330 core\n"
  "in vec2 positon\n"
  "in vec4 color\n"
  "out vec4 v_color\n"
  "uniform float scale\n"
  "void main(){\n"
  "  gl_Position = (position*scale, 0.0f, 1.0f);\n"
  "  v_color = color\n"
  "}\n";
const char *fragment_shader_source =
  "#version 330 core\n"
  "in vec4 v_color\n"
  "out vec4 out_color\n"
  "void main(){\n"
  "  out_color = v_color;"
  "}\n";
static struct svertex data[3] = //triangle
  {{.x = -1.0f, .y = -1.0f, .r = 1.0f, .g = 0.0f, .b = 0.0f, .a = 1.0f},
   {.x = 1.0f, .y = -1.0f, .r = 0.0f, .g = 1.0f, .b = 0.0f, .a = 1.0f},
   {.x = 0.0f, .y = 1.0f, .r = 0.0f, .g = 0.0f, .b = 1.0f, .a = 1.0f}};
/*
static struct svertex data[4] = //it's a square
  {{.x = -1.0f, .y = -1.0f, .r = 1.0f, .g = 0.0f, .b = 0.0f, .a = 1.0f},
   {.x = -1.0f, .y = 1.0f, .r = 0.0f, .g = 1.0f, .b = 0.0f, .a = 1.0f},
   {.x = 1.0f, .y = -1.0f, .r = 0.0f, .g = 0.0f, .b = 1.0f, .a = 1.0f},
   {.x = 1.0f, .y = 1.0f, .r = 1.0f, .g = 1.0f, .b = 1.0f, .a = 1.0f}};

const char *vertex_shader_source =
  "#version 330 core\n"
  "uniform float scale;\n"
  "attribute vec2 position;\n"
  "attribute vec4 color;\n"
  "varying vec4 v_color;\n"
  "\n"
  "void main(){\n"
  "  gl_Position = vec4(position*scale, 0.0, 1.0);\n"
  "  v_color = color;\n"
  "}\n";

const char *fragment_shader_source =
  "#version 330 core\n"
  "varying vec4 v_color;\n"
  "void main(){\n"
  "  gl_FragColor = v_color;\n"
  "}";*/
//describe how to find the data that gets passed to the shaders
void bind_vertix_attrib(GLuint program, GLuint buffer,
                        const char *name, int size, GLenum type,
                        int normalized, size_t stride, size_t offset){
  GLint loc = glGetAttribLocation(program, name);
  glEnableVertexAttribArray(loc);
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
  glVertexAttribPointer(loc, size, type, normalized, stride, &offset);
}
void create_and_init_data(GLuint program){
  GLuint buffer;
  glGenBuffers(1,&buffer);
  glBindBuffer(GL_ARRAY_BUFFER, buffer);//make buffer current
  glBufferData(GL_ARRAY_BUFFER, 3*sizeof(struct vertex), data, GL_STATIC_DRAW);
  size_t offset = offsetof(struct vertex, r);
  
  bind_vertix_attrib(program, buffer, "position", 2, GL_FLOAT, 1,
                     sizeof(struct vertex), 0);
  bind_vertix_attrib(program, buffer, "color", 4, GL_FLOAT, 1,
                     sizeof(struct vertex), offset);
  //specify the location of a uniform variable
  //this variable doesn't really do anything, but that's ok
  GLint scale_loc = glGetUniformLocation(program, "scale");
  glUniform1f(scale_loc, 1.0f);//set uniform variable to a single float
  return;
}

void main_loop(GLFWwindow *window, GLuint program){
  glClearColor(0.0f,0.4f,0.0f,1.0f);
  while(!glfwWindowShouldClose(window)){
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);//clear the old bufer
    glUseProgram(program);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glfwSwapBuffers(window);//switch buffers, we're using double buffering
    glfwPollEvents();
  }
}

int main(int argc, char *argv[]){
  GLFWwindow *win = init_gl_context(600,600,"basic_test");
  GLuint program = create_shader_program(vertex_shader_source,
                                  fragment_shader_source);//compile our shaders
  create_and_init_data(program);//initialize data used by the shaders
  main_loop(win, program);//run
}

