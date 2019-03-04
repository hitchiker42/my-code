#ifndef __SMALL_ALLOC_H__
#define __SMALL_ALLOC_H__
#ifdef __cplusplus
extern "C" {
#endif
#if (defined __linux__)
#include <alloca.h>
#elif (defined _MSC_VER)
#include <malloc.h>
#define alloca _alloca
#endif
#define SMALL_ALLOC_LIMIT 4096
#define SMALL_ALLOC(sz) (sz > SMALL_ALLOC_LIMIT ? malloc(sz) : alloca(sz))
#define SMALL_ALLOC_FREE(ptr, sz) (sz > SMALL_ALLOC_LIMIT ? free(ptr) : ;)
#ifdef __cplusplus
}
#endif
#endif /* __SMALL_ALLOC_H__ */
