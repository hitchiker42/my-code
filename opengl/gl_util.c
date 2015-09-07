#include "gl_util.h"
/*
  Basic error handling function to help diganose glfw errors.
*/
static void handle_error(int err_code, const char *err_string){
  fprintf(stderr,"Glfw error %d:\n%s\n",err_code, err_string);
  return;
}
GLFWwindow* init_gl_context(int w, int h, const char* name){
  if(!glfwInit()){
    fprintf(stderr, "Error, failed to initialize glfw\n");
    exit(EXIT_FAILURE);
  }
  atexit(glfwTerminate);
  glfwSetErrorCallback(handle_error);
  //  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, 1);
  GLFWwindow *win = glfwCreateWindow(w, h, name, NULL, NULL);
  if(!win){
    fprintf(stderr, "Error creating window\n");
    exit(EXIT_FAILURE);
  }
  glfwMakeContextCurrent(win);
  //  glfwSwapInterval(1);
  glewExperimental = 1;
  GLenum err = glewInit();
  if(err != GLEW_OK){
    fprintf(stderr, "Error, glew init failure\n");
    exit(EXIT_FAILURE);
  }
  if(!(GLEW_VERSION_3_3)){
    fprintf(stderr, "Error, OpenGL 3.3 not supported\n");
    exit(EXIT_FAILURE);
  }
  return win;
}
static inline GLuint compile_shader(GLenum type, const char *source, int len){  
  GLint status;
  GLuint shader = glCreateShader(type);
  glShaderSource(shader, 1, &source, &len);
  glCompileShader(shader);
  glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
  if(!status){
    GLint log_size;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_size);
    char *buf = alloca(log_size*sizeof(char));
    glGetShaderInfoLog(shader, log_size, NULL, buf);
    fprintf(stderr, "Error when compiling shader.\nShader info log:\n%s",buf);
    exit(EXIT_FAILURE);
  }
  return shader;
}
GLuint create_shader_program(const char *vertex_shader_source,
                             const char *fragment_shader_source){
  DEBUG_PRINTF("creating shaders\n");
  
  GLuint vertex_shader = compile_shader(GL_VERTEX_SHADER,
                                        vertex_shader_source, 0);
  GLuint fragment_shader = compile_shader(GL_FRAGMENT_SHADER,
                                          fragment_shader_source, 0);
  GLuint program = glCreateProgram();

  glAttachShader(program, vertex_shader);
  glAttachShader(program, fragment_shader);

  glLinkProgram(program);
  glValidateProgram(program);

  glDetachShader(program, vertex_shader);
  glDetachShader(program, fragment_shader);
  glDeleteShader(vertex_shader);
  glDeleteShader(fragment_shader);
  return program;
}
void glfw_main_loop(GLFWwindow *window,
                    void(*draw)(void*), void *userdata){
  while(!glfwWindowShouldClose(window)){
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);//clear the old bufer
    draw(userdata);
    glfwSwapBuffers(window);//switch buffers, we're using double buffering
    glfwPollEvents();
  }
}
