/*
  Types for use in opengl code, including a makeshift object framwork.
*/
#ifndef __MY_GL_TYPES_H__
#define __MY_GL_TYPES_H__
#include <GL/glew.h>
#include "common.h"
/*
  GL code uses camel case so using a gl_ prefix should avoid
  namespace clashes. Since this is C structs have their own namespace
  so they have fairly simple names.
*/
typedef union gl_obj gl_obj;//The base of our makeshift object framwork
//Normally I would make this a uint64_t, but I need it to contain a float
//which would violate strict aliasing
typedef struct gl_imm gl_imm;//type of 32 bit objects
typedef struct vertex gl_vertex;
//simple vertex, only xy, rgb
typedef struct svertex gl_svertex;
typedef struct position gl_position;//xyzw
typedef struct point gl_point; //just x and y
typedef struct color gl_color; //rgba, as floats
typedef struct gl_string gl_string; //string + length
typedef struct gl_vec gl_vec;//1D vector
typedef struct gl_mat gl_mat;//2D matrix
typedef struct shape gl_shape;
typedef struct vertex_attrib gl_vertex_attrib;
/*
  Enums use a capatilized prefix to avoid namespace collisions
*/
enum GL_type {
  GL_imm_type_begin = 0,
  GL_uint_type = 0,
  GL_int_type,
  GL_float_type,
  GL_byte_type,//uint8
  GL_byte4_type,//uint8_t[4]
  GL_imm_type_end = GL_byte4_type,
  GL_vec_type,
  GL_arr_type,
  GL_vertex_type,
};
/*
  Gcc (and clang, and most other compilers) allocate objects on 8 bytes
  boundaries, this means the low 3 bits of any pointer will be 0, so
  we can store information there.
*/
enum GL_obj_tag {
  //allow 62 bit immediate integers, accessible with a shift
  GL_int0_tag = 0x3,//011
  GL_int1_tag = 0x7,//111
  GL_ptr_tag = 0x0,//000, fast access since no shift is needed
  GL_imm_tag = 0x1,//001
  GL_arr_tag = 0x2,//010
  GL_4_tag = 0x4,//100
  GL_5_tag = 0x5,//101
  GL_6_tag = 0x6,//110
};
struct gl_imm {
  uint8_t tag;//really only the low 3 bits are relevent
  uint8_t type;//gl_imm_type_begin <= type <= gl_imm_type_end
  uint16_t padding;
  union {
    uint32_t uint;
    int32_t sint;
    float float32;
    uint8_t byte;
    uint8_t byte4[4];
  };
};
union gl_obj {
  uint64_t obj;//generic basic object / integer
  void *ptr;//generic pointer
  struct gl_imm imm;//has a float in it, which is why we need this union
};
_Static_assert(sizeof(gl_obj) == sizeof(uint64_t),
               "Error, gl_obj is too large, ensure gl_imm is the right size\n");
/*
  Macros to get values from a gl_obj
*/
//immediate objects can be accessed without any shifting
#define XGLINT(obj) (obj.imm.sint)
#define XGLUINT(obj) (obj.imm.uint)
#define XGLFLOAT(obj) (obj.imm.float32)
#define XGLBYTE(obj) (obj.imm.byte)
#define XGLBYTE4(obj) (obj.imm.byte4)
#define XINT62(obj) (obj.obj >> 2)
#define XPTR(obj) (obj.ptr & ~(0x7))
#define XSTRING(obj) ((gl_string*)(XPTR(obj)))
/*
  Macros to set gl_objs
*/
#define XSETGLINT(obj, glint) (obj.imm.sint = glint)
#define XSETGLUINT(obj, gluint) (obj.imm.uint = gluint)
#define XSETGLFLOAT(obj, flt) (obj.imm.float32 = flt)
#define XSETGLBYTE(obj, byt) (obj.imm.byte = byt)
#define XSETGLBYTE4(obj, byt4) (obj.imm.byte4 = byt4)
#define XSETINT62(obj, int62) (obj.obj = ((int62 << 2) | 0x3))
#define XSETPTR(obj, pointer) (obj.ptr = pointer)
struct gl_string {
  int32_t length;//-1 = unknown, but null terminated
  int32_t size;//-1 = unknown
  const char *restrict str;
};
struct gl_vec {
  uint32_t length;
  uint8_t type;
  union {
    float *float_vec;
    double *double_vec;
    int32_t *int_vec;
    uint32_t *uint_vec;
    uint8_t *byte_vec;//can be used for strings
    gl_obj *obj_vec;
    gl_svertex *svertex_vec;
    gl_vertex *vertex_vec;
  };
};
struct gl_mat {
  uint32_t width;
  uint32_t height;
  uint32_t size;
  uint8_t type;
  union {
    float *float_arr;
    double *double_arr;
    int32_t *int_arr;
    uint32_t *uint_arr;
    uint8_t *byte_arr;
    gl_obj *obj_arr;
    gl_vertex *vertex_arr;
  };
};
struct position {
  float x;
  float y;
  float z;
};
struct color {
  float r;
  float g;
  float b;
  float a;
};
#define inline_struct_color(name)               \
  union{                                        \
    struct color name;                          \
    struct {                                    \
      float r;                                  \
      float g;                                  \
      float b;                                  \
      float a;                                  \
    };                                          \
  };
#define inline_struct_position(name)               \
  union{                                        \
    struct position name;                          \
    struct {                                    \
      float x;                                  \
      float y;                                  \
      float z;                                  \
    };                                          \
  };
struct vertex {
  inline_struct_position(pos);
  inline_struct_color(color);
};
struct vertex_attrib {
  GLuint location;
  GLuint size;
  GLenum type;
  int normalized;
  int stride;
  size_t offset;
};
  
  
typedef struct vertex gl_triangle[3];
struct shape {
  struct vertex *vertices;
  int num_vertices;
  GLuint buffer;
  GLuint VAO;
};
struct solid_shape {
  gl_position *points;
  int num_points;
  gl_color color;
  GLuint buffer;
  GLuint VAO;
};
#endif
