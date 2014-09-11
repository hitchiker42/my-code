#ifndef _SL_COMMON_H_
#error "Don't include alloc.h directly, use common.h"
#endif
//Allocation Macros
#ifdef CUSTOM_ALLOC
#ifndef SL_MALLOC
#error "CUSTOM_ALLOC defined, but SL_MALLOC undefined"
#endif
#ifndef SL_FREE
#error "CUSTOM_ALLOC defined, but SL_FREE undefined"
#endif
#ifndef SL_REALLOC
#define SL_REALLOC(ptr, size)\
  ({__typeof ptr temp = SL_MALLOC(size);        \
  memcpy(ptr, temp, size);                      \
  SL_FREE(ptr);                                 \
  ptr=temp;})
#endif
#ifndef SL_MALLOC_ATOMIC
#define SL_MALLOC_ATOMIC SL_MALLOC
#endif
#ifndef SL_STRDUP
#define SL_STRDUP(str)                          \
  ({int len = strlen(str);                      \
    char *new_str = SL_MALLOC_ATOMIC((len+1)*sizeof(char));     \
    memcpy(new_str, str, len+1);                                \
    new_str;})
#endif
#ifndef SL_STRNDUP
#define SL_STRNDUP(str, n)                      \
  ({char *new_str = SL_MALLOC_ATOMIC((n+1)*sizeof(char));       \
  memcpy(new_str, str, n+1);                                    \
  new_str;})
#endif
#ifndef SL_MALLOC_BIG
#define SL_MALLOC_BIG SL_MALLOC
#endif
#define sl_malloc SL_MALLOC
#define sl_free SL_FREE
#define sl_realloc SL_REALLOC
#define sl_malloc_atomic SL_MALLOC_ATOMIC
#define sl_strdup SL_STRDUP
#define sl_strndup SL_STRNDUP
#define sl_malloc_big SL_MALLOC_BIG
#else /* ! CUSTOM_ALLOC*/
#define sl_malloc GC_MALLOC
#define sl_free GC_FREE
#define sl_realloc GC_REALLOC
#define sl_malloc_atomic GC_MALLOC_ATOMIC
#define sl_strdup GC_STRDUP
#define sl_strndup GC_STRNDUP
#define sl_malloc_big GC_MALLOC_IGNORE_OFF_PAGE
#endif
