#include "common.h"
/*
  Basic error handling function to help diganose glfw errors.
*/
static void handle_error(int err_code, const char *err_string){
  fprintf(stderr,"Glfw error %d:\n%s\n",err_code, err_string);
  return;
}
static inline void init_glew(void){
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
  if(GLEW_ARB_multisample){
    glEnable(GL_MULTISAMPLE);
  }
  /*
    Enable basic depth testing
   */
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);
  /*
    Enable basic color blending
  */
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  return;
}
#if (defined USE_GLFW) || !(defined USE_GLUT)
int gl_window_should_close(gl_window window){
  return glfwWindowShouldClose(window);
}
void gl_swap_buffers(gl_window win){
  glfwSwapBuffers(win);
}
void gl_poll_events(){
  glfwPollEvents();
}
gl_window init_gl_context(int w, int h, const char* name){
  if(!glfwInit()){
    fprintf(stderr, "Error, failed to initialize glfw\n");
    exit(EXIT_FAILURE);
  }
  atexit(glfwTerminate);
  glfwSetErrorCallback(handle_error);
  glfwWindowHint(GLFW_SAMPLES, 4);//4xAA
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
  init_glew();
  glfwSwapInterval(1);
  return win;
}
#else
int glut_window_should_close = 0;
static void glut_set_window_should_close(void){
  glut_window_should_close = 1;
}
int gl_window_should_close(gl_window window){
  return glut_window_should_close;
}
void gl_swap_buffers(gl_window win){
  glutSwapBuffers();
}
void gl_poll_events(){
  glutMainLoopEvent();
}
gl_window init_gl_context(int w, int h, const char* name){
  glutInit(0,0);//might need to pass a non-null pontier as first arg
  glutInitContextVersion(3,3);
  glutInitContextFlags(GLUT_FORWARD_COMPATIBLE);
  glutSetOption(GLUT_MULTISAMPLE, 4);//4xAA
  glutSetOption(GLUT_ACTION_ON_WINDOW_CLOSE,
                GLUT_ACTION_GLUTMAINLOOP_RETURNS);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA |
                      GLUT_DEPTH | GLUT_MULTISAMPLE);
  glutInitWindowSize(w,h);
  glutCreateWindow(name);
  atexit(glutExit);
  glutCloseFunc(glut_set_window_should_close);
}
#endif  
static inline __attribute__((pure))
char *shader_type_to_string(GLenum type){
  switch(type){
    case(GL_FRAGMENT_SHADER):
      return "fragment shader";
    case(GL_VERTEX_SHADER):
      return "vertex shader";
    default:
      return "";
  }
}
//doesn't print empty logs
#define print_gl_info_log(objtype, obj, msg)                    \
  ({GLint log_size;                                             \
    glGet##objtype##iv(obj, GL_INFO_LOG_LENGTH, &log_size);     \
    if(log_size > 0){                                           \
      char *buf = alloca(log_size*sizeof(char));                \
      glGet##objtype##InfoLog(obj, log_size, NULL, buf);        \
      fprintf(stderr, "%s"#objtype" info log:\n%s", msg, buf);  \
    };})
#define print_shader_info_log(shader, msg)\
  print_gl_info_log(Shader, shader, msg)
#define print_program_info_log(program, msg)\
  print_gl_info_log(Program, program, msg)

static inline GLuint compile_shader(GLenum type, const char *source, int len){
  GLint status;
  GLuint shader = glCreateShader(type);
#if (defined DEBUG) && !(defined NDEBUG)
  DEBUG_PRINTF("Compiling %s\n", shader_type_to_string(type));
#endif
  if(len == 0){
    len = strlen(source);
  }
  glShaderSource(shader, 1, &source, &len);
  glCompileShader(shader);
  glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
  if(!status){
    print_shader_info_log(shader, "Error when compiling shader.\n");
    exit(EXIT_FAILURE);
  }
  //always print shader info when debbuing is enabled#
#if (defined DEBUG) && !(defined NDEBUG)
  print_shader_info_log(shader, "");
#endif
  return shader;
}
//IDEA: change this to also create the program and return it as the return value
static inline void link_program(GLuint program, GLuint *shaders, int nshaders){
  int i, status;
  //the compiler should be able to unroll this loop
  for(i=0;i<nshaders;i++){
    glAttachShader(program, shaders[i]);
  }
  glLinkProgram(program);
  glGetProgramiv(program, GL_LINK_STATUS, &status);
  if(!status){
    print_program_info_log(program, "Error when linking program.\n");
    exit(EXIT_FAILURE);
  }
  IF_DEBUG(print_program_info_log(program, "");)

  glValidateProgram(program);
  glGetProgramiv(program, GL_VALIDATE_STATUS, &status);
  if(!status){
    print_program_info_log(program, "Error when validating program.\n");
  }
  IF_DEBUG(print_program_info_log(program, "");)
  for(i=0;i<nshaders;i++){
    glDetachShader(program, shaders[i]);
  }
  return;
}

GLuint create_shader_program(const char *vertex_shader_source,
                             const char *fragment_shader_source,
                             int is_pathname){
  DEBUG_PRINTF("creating shaders\n");
  GLuint vertex_shader, fragment_shader;
  if(is_pathname){
    size_t vlen = 0, flen = 0;
    //could allocate these with alloca
    char *vsource = read_file_to_string(vertex_shader_source, &vlen);
    char *fsource = read_file_to_string(fragment_shader_source, &flen);
    vertex_shader = compile_shader(GL_VERTEX_SHADER,
                                          vsource, vlen);
    fragment_shader = compile_shader(GL_FRAGMENT_SHADER,
                                            fsource, flen);
    free(vsource); free(fsource);
  } else {
    vertex_shader = compile_shader(GL_VERTEX_SHADER,
                                   vertex_shader_source, 0);
    fragment_shader = compile_shader(GL_FRAGMENT_SHADER,
                                     fragment_shader_source, 0);
  }
  GLuint program = glCreateProgram();
  //this should be optimized out, but I may need to rewrite things to
  //just use this array if not
  GLuint shaders[2] = {vertex_shader, fragment_shader};

  link_program(program, shaders, 2);

  glDeleteShader(vertex_shader);
  glDeleteShader(fragment_shader);
  glUseProgram(program);
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

/*
  Binds the data/vertex layout for vertex attribute loc to the
  current VAO
*/
void bind_vertex_attrib(GLuint buffer, GLint loc, int size, GLenum type,
                        int normalized, size_t stride, size_t offset){
  glEnableVertexAttribArray(loc);
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
  glVertexAttribPointer(loc, size, type, normalized, stride, (void*)offset);
}
void bind_vertex_attribs(GLuint buffer, struct vertex_attrib *attribs,
                         int num_attribs){
  int i;
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
  for(i=0;i<num_attribs;i++){
    gl_vertex_attrib attrib = attribs[i];
    glEnableVertexAttribArray(attrib.location);
    glVertexAttribPointer(attrib.location, attrib.size, attrib.type,
                          attrib.normalized, attrib.stride,
                          (void*)attrib.offset);
  }
}

/*
  Creates a gl buffer and binds the data given in data to it.
*/
GLuint make_data_buffer(GLenum buffer_type, void *data,
                        size_t size, int usage){
  GLuint buffer;
  glGenBuffers(1,&buffer);
  glBindBuffer(GL_ARRAY_BUFFER, buffer);//make buffer current
  glBufferData(GL_ARRAY_BUFFER, size, data, usage);
  return buffer;
}
//gl_positon compute_normal(void *triangle, int offset, int stride){
  
/*
  A basic vertex and fragment shader.
*/
const char *basic_vertex_shader =
  "#version 330 core\n"
  "layout(location = 0) in vec3 position;\n"
  "layout(location = 1) in vec4 color;\n"
  "out vec4 v_color;\n"
  "void main(){\n"
  "  gl_Position.xyz = position;\n"
  "  gl_Position.w = 1.0f;\n"
  "  v_color = color;\n"
  "}\n";
const char *basic_fragment_shader =
  "#version 330 core\n"
  "in vec4 v_color;\n"
  "layout(location = 2) out vec4 f_color;\n"
  "void main(){\n"
  "  f_color = v_color;\n"
  "}\n";
