#define _GNU_SOURCE
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#define MZ_PRECISE_GC
#include <racket/scheme.h>
#include <racket/escheme.h>


#if (SIZEOF_VOID_P == 8)
typedef __int128 int128; 
typedef unsigned __int128 uint128;
typedef uint128 exfixnum;
#else
typedef uint64_t exfixnum;
#endif

typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef uint64_t uint64;

static Scheme_Type scheme_exfixnum_type;
typedef struct Scheme_Exfixnum {
  Scheme_Object type;//16 or 32 bits depending on gc mode
  union {
    struct {uintptr_t fix1; uintptr_t fix2;};
    exfixnum ex;
  };
} Scheme_Exfixnum;
/* Scheme_Object *make_exfixnum(exfixnum num){ */
/*   Scheme_Object *obj = scheme_alloc_object(); */
/*   obj->type = scheme_exfixnum_type; */
/*   *(obj + offsetof(Scheme_Fixnum, ex)) = num; */
/*   return obj; */
/* } */
#define add +
#define sub -
#define mul *
#define div /
#define lshift <<
#define rshift >>
#define xor ^
#define and &
#define or |
#define not ~

/*
  x,y,x_ptr and y_ptr need to be defined when calling this.
  Hooray for non hygenic macros.
*/
#define do_bytevector_primitive(type, op)                               \
  uint8_t *x_ptr = ((uint8_t*)SCHEME_BYTE_STR_VAL(x_bv))+SCHEME_INT_VAL(x_idx); \
  uint8_t *y_ptr = ((uint8_t*)SCHEME_BYTE_STR_VAL(y_bv))+SCHEME_INT_VAL(y_idx); \
  type x = *((type*)(x_ptr));                                           \
  type y = *((type*)(y_ptr));                                           \
  type z = x op y;
#define do_bytevector_not(type)                                         \
  uint8_t *x_ptr = ((uint8_t*)SCHEME_BYTE_STR_VAL(x_bv))+SCHEME_INT_VAL(x_idx); \
  type x = *((type*)(x_ptr));                                           \
  type z = ~(x);

/*
  The functions generated by this don't do typechecking
*/
#define make_bytevector_primitive(type, op)                             \
  void scheme_bytevector_##type##_##op##_in_place(Scheme_Object *x_bv,  \
                                                  Scheme_Object *x_idx, \
                                                  Scheme_Object *y_bv,  \
                                                  Scheme_Object *y_idx){ \
    do_bytevector_primitive(type, op);                                  \
    *((type*)(x_ptr)) = z;                                              \
    return;                                                             \
  }                                                                     \
                                                                        \
  Scheme_Object *bytevector_##type##_##op##_in_place_prim(int argc,     \
                                                          Scheme_Object **argv){ \
    scheme_bytevector_##type##_##op##_in_place(argv[0], argv[1], argv[2], argv[3]); \
    return scheme_void;                                                 \
  }                                                                     \
  type scheme_bytevector_##type##_##op(Scheme_Object *x_bv,             \
                                       Scheme_Object *x_idx,            \
                                       Scheme_Object *y_bv,             \
                                       Scheme_Object *y_idx){           \
    do_bytevector_primitive(type, op);                                  \
    return z;                                                           \
  }                                                                     \
  Scheme_Object *bytevector_##type##_##op##_prim(int argc,              \
                                                 Scheme_Object **argv){ \
    type tmp = scheme_bytevector_##type##_##op(argv[0], argv[1], argv[2], argv[3]); \
    return scheme_make_integer(tmp);                                    \
  }
#define make_bytevector_not(type)                                       \
  void scheme_bytevector_##type##_not_in_place(Scheme_Object *x_bv,     \
                                               Scheme_Object *x_idx){   \
    do_bytevector_not(type);                                            \
    *((type*)(x_ptr)) = z;                                              \
    return;                                                             \
  }                                                                     \
  Scheme_Object* bytevector_##type##_not_in_place_prim(int argc,         \
                                                      Scheme_Object **argv){ \
    scheme_bytevector_##type##_not_in_place(argv[0], argv[1]);          \
    return scheme_void;                                                 \
  }                                                                     \
  type scheme_bytevector_##type##_not(Scheme_Object *x_bv,              \
                                      Scheme_Object *x_idx){            \
    do_bytevector_not(type);                                            \
    return z;                                                           \
  }                                                                     \
  Scheme_Object* bytevector_##type##_not_prim(int argc,                  \
                                             Scheme_Object **argv){     \
    type tmp = scheme_bytevector_##type##_not(argv[0], argv[1]);        \
    return scheme_make_integer(tmp);                                    \
  }
#define make_bytevector_primitives(type)        \
  make_bytevector_primitive(type, add);         \
  make_bytevector_primitive(type, sub);         \
  make_bytevector_primitive(type, mul);         \
  make_bytevector_primitive(type, div);         \
  make_bytevector_primitive(type, lshift);      \
  make_bytevector_primitive(type, rshift);      \
  make_bytevector_primitive(type, xor);         \
  make_bytevector_primitive(type, and);         \
  make_bytevector_primitive(type, or);          \
  make_bytevector_not(type);

make_bytevector_primitives(uint8);
make_bytevector_primitives(uint16);
make_bytevector_primitives(uint32);
make_bytevector_primitives(uint64);
static Scheme_Object *
output_port_p (int argc, Scheme_Object *argv[]){
  return (SCHEME_OUTPUT_PORTP(argv[0]) ? scheme_true : scheme_false);
}
static Scheme_Object* current_output_port(){
  return scheme_param_config2("current-output-port",
                              scheme_make_integer(MZCONFIG_OUTPUT_PORT),
                              0, NULL, -1, output_port_p, "output-port?", 0);
}
static const char hello[] = "Hello, World!\n";
Scheme_Object *hello_world_prim(int argc, Scheme_Object **argv){
  Scheme_Object *out = current_output_port(0, NULL);
  int fd = scheme_get_port_fd(out);
  if(fd <= 0){
    fd = STDOUT_FILENO;
  }
  write(fd, hello, sizeof(hello)-1);
  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env){
  Scheme_Object *hello_world_proc = scheme_make_prim_w_arity(hello_world_prim,
                                                             "hello-world",0,-1);
  scheme_add_global("hello-world", hello_world_proc, env);
  return scheme_void;
}
Scheme_Object *scheme_reload(Scheme_Env *env){
  return scheme_void;
}
Scheme_Object* scheme_module_name(void){
  return scheme_false;
}
