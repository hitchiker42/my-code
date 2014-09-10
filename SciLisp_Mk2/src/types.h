/*
  Definations of lisp types, and typechecking macros.
  Is included by common.h, so sholdn't have to be included by
  any other file
*/
#ifndef SL_TYPES_H
#define SL_TYPES_H
/*
  The 3 least significant bits of a pointer will always be 0, because
  pointers are aligned to 8 byte boundries.
  Furthermore because the garbage collecter is compiled with 
  ALL_INTERIOR_POINTERS defined those bits can be set and the
  object will still be kept.

  This allows us to use the least significant 3 bits of a sl_obj to store
  type information, for 8 different types.

  In general there are 3 catagories of values that an sl_obj can hold.

  -A 62/61 bit integer, this should hold the majority of integer values
  used in a normal program.
  -A pointer, the type of value being pointed to is given by either
  the 3 lsb for common types (e.g conses, strings) or by a type field
  in the object being pointed to.
  -A non integer immediate, i.e an immediate value which can be held
  in ~48 bits.
*/
/*
  An explaination of the naming rules used in the scilisp code:
  -Any defined type should have a leading sl_
  -enum members should have a leading SL_
  -Macros should generally be all upper case (though there are exceptions)
  
  -functions need three names:
  --the name used in calling the function from lisp,
  --the name used in calling the function from C,
  --the name of the symbol holding the function in C    
  --The name in C is F followed by the lisp name converted to C
  --The symbol name is S followed by the lisp name converted to C

  -As there are more acceptable characters for names in lisp than C
  there are rules to translate lisp names to C
  --Non leading/trailing '-'s are converted to '_'s
  (this imples that variable-name -> variable_name but 1- doesn't go to 1_)
  --leading numbers are transated to the number spelled out 
  (i.e 8-name (a horrible variable name but eh) goes to eight_name)
  --A trailing '?' is translated to a trailing 'p'
  (i.e cons? -> consp)
  --A trailing '!' is translated to ... something
*/
/*All typedefs should go here (all typedefs in this file) */
/* core lisp type, defined as an unsigned 64 bit integer for simplicity*/
typedef uint64_t sl_obj;
/* any object that isn't part of the sl_type enum */
typedef struct sl_misc sl_misc;
/* Vectors, strings and vectorlike objects (arrays, hashtables, etc) */
/* Actual vectors have the type (sl_)svector */
typedef struct sl_vector sl_vector;
/* Numbers other than <62 bit integers or 32 bit reals */
typedef struct sl_number sl_number;
/* lisp symbols */
typedef struct sl_symbol sl_symbol;
/* non integer immediates */
typedef struct sl_imm sl_imm;
/* cons cells */
typedef struct sl_cons sl_cons;
/* strings, same structure as a vector */
typedef struct sl_string sl_string;

/* types for real numbers (i.e floats and doubles)*/
typedef double real64_t;
typedef float real32_t;
/*NOTE: this is not typedefed so sl_type and enum sl_type are different things.
  However enum sl_type will probably be rarely used
*/
enum sl_type {
  /* Integers take up two tags so that they can be 62 bits instead of 61 */
  SL_int0 = 2,
  SL_int1 = 6,
  //by having a symbol have no bits set NULL can be identicial to NIL
  SL_symbol = 0, //pointer to a symbol   
  SL_cons = 1, //pointer to a cons cell  
  SL_number = 3, //pointer to a number (rational, complex or large int/real)
  SL_vector = 4, //pointer to a vectorlike object
  SL_imm = 5, //non integer immediate
  SL_misc = 7, //pointer to a struct sl_heap_obj
};
#define SL_int_mask 0x2
enum sl_imm_type {
  SL_nil = 0,
  SL_T = 0xff,
  SL_float = 0x80,
  SL_int32 = 0x81,
  SL_int16 = 0x82,
  SL_int8 = 0x83,
  SL_char = 1,
};

#define sl_type_bits(obj) (obj & 0x7)
/* Extract the int value from an sl_obj*/
#define XINT(obj) (obj >> 2)
/* Extract the pointer from an sl_obj */
#define XPTR(obj) (obj & (~0x7))
#define XCONS(obj) ((sl_cons*)XPTR(obj))
#define XVECTOR(obj) ((sl_vector*)XPTR(obj))
#define XSYMBOL(obj) ((sl_symbol*)XPTR(obj))
#define XNUMBER(obj) ((sl_number*)XPTR(obj))
/* Macros to check the type of an sl_obj */
/*the 2 least significant bits of an int must be 0*/
#define INTP(obj) (obj & SL_int_mask)
#define CONSP(obj) (sl_type_bits(obj) == SL_cons)
#define VECTORP(obj) (sl_type_bits(obj) == SL_vector)
#define IMMP(obj) (sl_type_bits(obj) == SL_imm)
#define SYMBOLP(obj) (sl_type_bits(obj) == SL_symbol)
/* indirect number (i.e non immediate number */
#define IND_NUMBERP(obj) (sl_type_bits(obj) == SL_number)
/* Macros to check the type of a non integer immediate */
/* non integer immediate that is a number */
#define IMM_NUMBERP(obj) (IMMP(obj) && (((sl_imm)obj).tag & 0x80))
#define NUMBERP(obj) (INTP(obj) || IND_NUMBERP(obj) || IMM_NUMBERP(obj))
#define CHARP(obj) (IMMP(obj) && (((sl_imm)obj).tag == 1))
#define NILP(obj) (IMMP(obj) && (((sl_imm)obj).tag == 0))
#define NIL NULL
static const sl_obj nil = NULL;
//trivial function to make an sl_object, used because it
//hides the internal implementation of sl_obj so it can be
//changed without effecting other code
SL_inline sl_obj make_sl_obj(uint64_t val, enum sl_type type){
  return (sl_obj)(val|type);
}
SL_inline sl_obj make_int(uint64_t val){
  return (sl_obj)((val <<2) | SL_int0);
}
struct sl_cons {
  sl_obj car;
  sl_obj cdr;
};
struct sl_imm {
  uint8_t tag;
  uint16_t padding; /* can be removed if the space is needed*/
  union {
    float real32;
    uint32_t sl_char; /*since char is a reserved word in C*/
    /* exact sized integers*/
    uint32_t uint32;
    uint16_t uint16;
    uint8_t uint8;
  };
  uint8_t sl_type; //these are the type bits from sl_obj
};
struct sl_vector {
  uint8_t tag;
  //it wouldn't be hard to define an int48_t type, or make this a union
  //to enable longer lengths
  int padding :24;
  uint32_t len;
  sl_obj *value;
};

struct sl_string {
  uint8_t tag;
  uint8_t encoding;
  uint8_t c_string; //is str a null terminated string
  uint8_t ascii; //does str only use characters 0-127
  uint32_t len;
  const char *str;
};
  
struct sl_symbol {
  sl_string name;
  sl_obj val;
  uint8_t type; //optional, currently unused
  uint8_t sl_const; //is the symbol a constant
  uint8_t sl_special; //is the symbol dynamically scoped
  //  sl_obj prop_list; may add, but I'm not sure it's totally necessary
};
#endif
