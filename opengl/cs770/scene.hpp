/*
  Class which encapsulates the state used with a gl shader program.
  Each distinct gl shader program requires its own scene instance.

  Uniform variables for a scene are encapsulated inside a uniform
  block, meaning the shader programs must define all uniform variables
  that will be bound by the scene in a uniform block, and in C(++) all
  uniform variabes for the program must be in a contigous buffer.
*/
#include "gl770.h"
//I like having a seperate namespace for structs in C, stupid C++
//and its automatic typedefs
typedef struct Scene gl_Scene;
typedef struct gl_Buffer gl_Buffer;
//I don't do classes
struct Scene {
  GLuint program;
  GLuint VAO;
  gl_Buffer *buffers;//pointer to array of buffers using this program
  int num_buffers;
  void *uniform_block;//opaque pointer to block of uniform variables
  int uniform_block_size;
  int uniform_block_index;//obtained via glGetUniformBlockIndex
  int scene_index;//used as the index for the uniform buffer binding
