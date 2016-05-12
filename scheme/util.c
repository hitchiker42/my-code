#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <guile/2.0/libguile.h>
#define scm_raise_error(key, msg)                               \
  ({SCM scm_key = scm_from_utf8_symbol(key);                    \
    SCM scm_msg = scm_from_utf8_string(msg);                    \
    scm_error_scm(scm_key, scm_from_utf8_string(__func__),      \
                  scm_msg, SCM_BOOL_F, SCM_BOOL_F);});
/*
  Code for a dynamically extensible bytevector. 
*/
typedef struct dyn_bv dyn_bv;
/*
  The type pointed to by SCM values is (probably) the following.
*/
struct dyn_bv_smob {
  uint16_t smob_tag;
  uint16_t smob_flags;
  uint32_t unused;//this might come before the previous 2
  uint8_t *bytes;
  size_t len;
  size_t sz;
};
struct dyn_bv {
  uint8_t *bytes;
  size_t len;
  size_t sz;
};
static scm_t_bits dyn_bv_tag;

//I think this should work, assuming (double) smobs work how I think they do
#define SCM_TO_DYN_BV(x)                        \
  (scm_assert_smob_type(dyn_bv_tag, x),         \
   (dyn_bv*)&(SCM_SMOB_DATA(x)))
//Check if bv can hold 'bytes' more...bytes
static inline void dyn_bv_check_size(dyn_bv *bv, size_t bytes){
  //this will almost never loop, but it doesn't hurt (much) to be careful 
  while(bv->len + bytes > bv->sz){
    scm_gc_realloc(bv->bytes, bv->sz, b->sz*2, SCM_GC_BYTEVECTOR);
    bv->sz *= 2;
  }
}

//I don't think I'm actually going to use this
SCM scm_dyn_bv_equalp(SCM dyn_bv_smob1, SCM dyn_bv_smob2){
  dyn_bv *bv1 = SCM_TO_DYN_BV(dyn_bv_smob1);
  dyn_bv *bv2 = SCM_TO_DYN_BV(dyn_bv_smob2);
  return (bv1 == bv2 ||
          (bv1->bytes == bv2->bytes &&
           bv1->len == bv2->len &&
           bv1->sz == bv2->sz) ? SCM_BOOL_T : SCM_BOOL_F);
}
SCM make_dyn_bv_smob(size_t len, size_t sz, int fill){
  uint8_t *bytes = scm_gc_malloc_pointerless(sz, SCM_GC_BYTEVECTOR);
  if(len > 0){
    //assert(len <= sz)
    memset(bytes, fill, len);
  }  
  return scm_new_double_smob(dyn_bv_tag, bytes, len, sz);
}
//This uses an internal function, but it's in the public header
//so it should  be fair game to use
SCM scm_dyn_bv_to_bv(SCM dyn_bv_smob){
  dyn_bv *bv = SCM_TO_DYN_BV(dyn_bv_smob);
  //Bytevector contents are allocated via gc so we don't need to
  //do anything special to insure the memory doesn't get collected.
  SCM scm_bv = scm_c_take_gc_bytevector(bv->bytes, bv->len, SCM_BOOL_F);
  return scm_bv;
}
enum slice_method {
  slice_copy,
  slice_shared,
  slice_readonly,
};
static __attribute__((always_inline))
SCM dyn_bytevector_slice(SCM dyn_bv_smob, SCM start, SCM end,
                         enum slice_method how){
  dyn_bv *bv = SCM_TO_DYN_BV(dyn_bv_smob);
  size_t start_c = scm_to_size_t(start);
  size_t end_c;
  if(scm_is_false(end)){
    end_c = bv->len;
  } else {
    end_c = scm_to_size_t(end);
  }
  if(end_c > bv->len){
    scm_raise_error("out-of-range",
                    "end must be less than the length of the bytevector");
  }
  void *bytes = bv->bytes + start_c;
  size_t len = end_c - start_c;
  SCM ret;
  //all of the cases fallthrough, this is deliberate
  switch(how){
    case slice_copy:{
      void *bytes_copy = scm_gc_malloc_pointerless(len);
      memcpy(bytes_copy, bytes, len);
      bytes = bytes_copy;
    } //fallthrough
    case slice_shared:
      ret = scm_new_smob(dyn_bv_tag, bytes, len, len);
      //fallthrough
    case slice_readonly://not currently implemented
      SCM_SET_SMOB_FLAGS(ret, 1);
  }
  return ret;
}
SCM dyn_bytevector_slice_copy(SCM dyn_bv_smob, SCM start, SCM end){
  return dyn_bytevector_slice(dyn_bv_smob, start, end, slice_copy);
}
SCM dyn_bytevector_slice_shared(SCM dyn_bv_smob, SCM start, SCM end){
  return dyn_bytevector_slice(dyn_bv_smob, start, end, slice_shared);
}
//not currently avaliable in scheme
SCM dyn_bytevector_slice_readonly(SCM dyn_bv_smob, SCM start, SCM end){
  return dyn_bytevector_slice(dyn_bv_smob, start, end, slice_readonly);
}
      
  
void init_util(void){
  dyn_bv_tag = scm_make_smob_type("dynamic-bytevector", 0);
