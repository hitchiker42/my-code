#include <stdint.h>
void arena_init();
void arena_reset();
void *arena_alloc(uint64_t sz);
//because cords use GC_NEW internaly
#define arena_new(objtype)                      \
  (arena_alloc(sizeof(objtype)))
void *arena_calloc(uint64_t sz);
