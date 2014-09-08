#include <stdlib.h>
static char *xstrdup (char const *string){
  return xmemdup (string, strlen (string) + 1);
}
#define FUNCALL_VOID(f,args...) (*(void(*)())f)(args)
#define NIL NULL
#define XCAR(cons) cons->car
#define XCDR(cons) cons->cdr
#define XCAAR(_cons) ((cons*)_cons->car)->car
#define XCADR(_cons) ((cons*)_cons->car)->cdr
#define PUSH(obj,cons)                          \
  ({cons->cdr=cons;                             \
    cons->car=obj;                              \
    cons;})
#define POP(cons) \
  ({void *retval=XCAR(cons);                    \
    cons=XCDR(cons);                            \
    retval;})
#define Acons(_car,_cdr)                        \
  ({cons *retval=alloca(sizeof(cons));          \
    retval->car=_car;                           \
    retval->cdr=_cdr;                           \
    retval;})
//desired is a pointer
#define atomic_compare_exchange(ptr,expected,desired)           \
  __atomic_compare_exchange(ptr,expected,desired,0,             \
                            __ATOMIC_SEQ_CST,__ATOMIC_SEQ_CST)
//desired isn't a pointer
#define atomic_compare_exchange_n(ptr,expected,desired)           \
  __atomic_compare_exchange_n(ptr,expected,desired,0,             \
                            __ATOMIC_SEQ_CST,__ATOMIC_SEQ_CST)
#define atomic_add(ptr,val)                     \
  __atomic_add_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_sub(ptr,val)                     \
  __atomic_sub_fetch(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_fetch_add(ptr,val)               \
  __atomic_fetch_add(ptr,val,__ATOMIC_SEQ_CST)
#define atomic_load_n(ptr)                     \
p  __atomic_load_n(ptr,__ATOMIC_SEQ_CST)
#define atomic_load(ptr,loc)                    \
  __atomic_load(ptr,loc,__ATOMIC_SEQ_CST)
#define atomic_store(ptr,loc)                 \
  __atomic_store_n(ptr,loc,__ATOMIC_SEQ_CST)
#define atomic_store_n(ptr,val)                 \
  __atomic_store_n(ptr,val,__ATOMIC_SEQ_CST)
typedef void (*handler_fn)(void *);
typedef struct event_loop_data *event_loop_handle;
typedef struct tail_queue tail_queue;
typedef struct queue_node queue_node;
typedef struct cons cons;
struct cons {
  void *car;
  void *cdr;
};
static const cons nil={NIL,NIL};
struct tail_queue {
  cons *head;
  cons *tail;
};
struct event_loop_data {
  tail_queue handler_queue;//queue of cons cells (handler_fn . client_data)
  cons *event_alist;//list of the form ((event . handler)...)
  sem_t semaphore;//announce event increments this 
  //and it is decremented when a handler is run
  pthread_spinlock_t queue_lock;
  pthread_rwlock_t alist_lock;
  int state;//running or not 1=created, 2 = running, 0=stopped
};
static inline cons* Fcons(void *car, void *cdr){
  cons *retval=xcalloc(sizeof(cons));
  retval->car=car;
  retval->cdr=cdr;
  return retval;
}
static inline tail_queue *queue_push(tail_queue *queue,void *data){
  cons *new_head=xcalloc(sizeof(cons));
  XCAR(new_head)=data;
  if(queue->head){
    XCDR(new_head)=queue->head;
  } else {
    queue->tail=new_head;
  }
  queue->head=new_head;
  return queue;
}
static inline cons *queue_pop(tail_queue *queue){
  cons *retval=queue->head;
  if(queue->head==queue->tail){
    queue->head=NULL;
    queue->tail=NULL;
  } else {
    queue->head=XCDR(queue->head);
  }
  return retval;
}
static inline tail_queue* queue_append(tail_queue *queue,void *data){
  cons *new_tail=xcalloc(sizeof(cons));
  XCAR(new_tail)=data;
  if(queue->tail){
    XCDR(queue->tail)=new_tail;
  }
  if(!queue->head){
    queue->head=new_tail;
  }
  queue->tail=new_tail;
  return queue;
}
//takes a literal cons cell, so 
static cons* assoc(cons* list,void *key,int (*cmp_fn)(void*,void*)){
  while(list != NIL){
    if(cmp_fn(XCAAR(list),key)){
      return list;
    }
    list=list->cdr;//no typechecking or anything
  }
  return NIL;
}
static int event_cmp(void *a,void *b){
  return !strcmp(a,b);
}
//here event is (const char *name . void *client_data)
static handler_fn get_event_handler(event_loop_handle handle,cons *event){
  cons *handler_cons;
  if((handler_cons=assoc(handle->event_alist,event->car,event_cmp))){
    return XCADR(handler_cons);
  }
  return NIL;
}
static void run_handler(event_loop_handle handle){
  pthread_spin_lock(&handle->queue_lock);
  cons *handler=queue_pop(&handle->handler_queue);
  pthread_spin_unlock(&handle->queue_lock);
  FUNCALL_VOID(XCAAR(handler),XCADR(handler));
  __asm__ volatile("mfence");
  free(XCAR(handler));
  free(handler);

}
