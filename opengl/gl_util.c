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
  GLFWwindow *win = glfwCreateWindow(w, h, name, NULL, NULL);
  if(!win){
    fprintf(stderr, "Error creating window\n");
    exit(EXIT_FAILURE);
  }
  glfwMakeContextCurrent(win);
  //  glfwSwapInterval(1);
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
GLuint create_shader_program(const char *vertex_shader_source,
                             const char *fragment_shader_source){
  DEBUG_PRINTF("creating shaders\n");
  
  GLuint vertex_shader = glCreateShader(GL_VERTEX_SHADER);
  GLuint fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);  

  DEBUG_PRINTF("setting shader sources\n");
  
  glShaderSource(vertex_shader, 1, (const char**)&vertex_shader_source, NULL);
  glShaderSource(fragment_shader, 1, (const char**)&fragment_shader_source, NULL);

  DEBUG_PRINTF("set shader sources\n");
  
  glCompileShader(vertex_shader);
  glCompileShader(fragment_shader);

  DEBUG_PRINTF("Compiled shaders\n");
    
  DEBUG_PRINTF("creating gl program\n");
  GLuint prog = glCreateProgram();

  DEBUG_PRINTF("Created gl program\n");

  glAttachShader(prog, vertex_shader);
  glAttachShader(prog, fragment_shader);


  glLinkProgram(prog);
  glValidateProgram(prog);

  glDetachShader(prog, vertex_shader);
  glDetachShader(prog, fragment_shader);
  glDeleteShader(vertex_shader);
  glDeleteShader(fragment_shader);
  return prog;
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
