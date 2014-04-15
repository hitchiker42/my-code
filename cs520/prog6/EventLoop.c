/*
  Interface to an EventLoop
  A handler function can be registered for an event, as events occur
  handler functions are put into an fifo queue and executed asynchronously.
  Events will be accepted event when handler functions are being run. If
  there are no functions left to run the loop will wait until an event occurs

  events are identified by strings

  the event loop does NOT start a new thread to execute the event loop
  
  I'm not a fan of camelCase so I defined all my functions with underscore
  seperators, but I aliased the actual functions to my functions so externally
  this should be unoticeable 
*/

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <semaphore.h>
#include <pthread.h>
#include "EventLoop.h"
#define NDEBUG
#if (defined DEBUG) && !(defined NDEBUG)
#define HERE() fprintf(stderr,"here at %s,line %d\n",__FILE__,__LINE__)
#define PRINT_MSG(string) fprintf(stderr,string);
#define PRINT_FMT(string,fmt...) fprintf(stderr,string,##fmt);
#define PRINT_LN(string) fprintf(stderr,"%s%s",string,"\n")
#else
#define HERE()
#define PRINT_MSG(string)
#define PRINT_FMT(string,fmt...)
#define PRINT_LN()
#endif
//do this instead of making everything static
//I didn't write these versions of the checked memory allocation I got
//them from gnulib, but really they're pretty much the same functions everywhere
/* Allocate N bytes of memory dynamically, with error checking.  */
static void *xmalloc (size_t n){
  void *p = malloc (n);
  if (!p && n != 0){
    perror("out of memory");
    exit(1);
  }
  return p;
}

/* Change the size of an allocated block of memory P to N bytes,
   with error checking.  */
/*
static void *xrealloc (void *p, size_t n){
  if (!n && p){
    free (p);
    return NULL;
  }
  p = realloc (p, n);
  if (!p && n){
    perror("out of memory");
    exit(1);
  }
  return p;
}
*/
static void *xcalloc (size_t s){
  return memset(xmalloc (s), 0, s);
}
static void *xmemdup (void const *p, size_t s){
  return memcpy (xmalloc (s), p, s);
}

/* Clone STRING.  */

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
  __atomic_load_n(ptr,__ATOMIC_SEQ_CST)
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
void main_loop(event_loop_handle handle){
  while(atomic_load_n(&handle->state) > 0){
    sem_wait(&handle->semaphore);
    run_handler(handle);
  }
}
  
/* create an EventLoop
 * returns a "handle" for the EventLoop or NULL on failure
 * defined as
 */
event_loop_handle create_event_loop(){
  struct event_loop_data *new_event_loop =
    xcalloc(sizeof(struct event_loop_data));
  sem_init(&new_event_loop->semaphore,0,0);
  pthread_spin_init(&new_event_loop->queue_lock,0);
  pthread_rwlock_init(&new_event_loop->alist_lock,NULL);
  atomic_store_n(&new_event_loop->state,1);
  return new_event_loop;
};  
void *createEventLoop(void)
  __attribute__((alias("create_event_loop")));

/* start an EventLoop
   the behavior is undefined if an EventLoop is started more than once
*/
void start_event_loop(event_loop_handle handle, handler_fn fn, void* user_data){
  if(atomic_load_n(&handle->state) != 1){
    fprintf(stderr,"Error, Trying to start an already started event loop\n");
    abort();
  }
  atomic_store_n(&handle->state,2);
  fn(user_data);
  main_loop(handle);
}
void startEventLoop(void *handle, handler_fn initialFunc,
                    void *initialArgument)
  __attribute__((alias("start_event_loop")));

/* register a handler for an event
 * if a handler is already defined for the event then it is overridden
 */
//since I'm using an alist I can just push the new handler 
//onto the front of the list
void register_event(event_loop_handle handle, const char *name, handler_fn fn){
  cons *event=Fcons(xstrdup(name),fn);
  pthread_rwlock_wrlock(&handle->alist_lock);
  handle->event_alist=Fcons(event,handle->event_alist);
  pthread_rwlock_unlock(&handle->alist_lock);
  return;
}

void registerEvent(void *handle,const char *eventName, handler_fn handler)
  __attribute__((alias("register_event")));
/* announce that an event has occurred
 * if an handler is registered for it, the handler function will be
 * enqueued for execution along with its "info" argument
 * if no handler is registered for it, then the EventLoop prints
 * "unhandled event: " followed by the event name and a newline, and
 * then continues
*/
void unhandled_event(const char *event_name){
  fprintf(stderr,"unhandled event: %s\n",event_name);
}
void announce_event(event_loop_handle handle ,char *event_name,
                    void *client_data){
  cons *event=Acons(event_name,client_data);
  pthread_rwlock_rdlock(&handle->alist_lock);
  handler_fn event_handler=get_event_handler(handle,event);
  pthread_rwlock_unlock(&handle->alist_lock);
  if(event_handler == NIL){
    unhandled_event(XCAR(event));
  } else {
    pthread_spin_lock(&handle->queue_lock);
    queue_append(&handle->handler_queue,Fcons(event_handler,XCDR(event)));
    pthread_spin_unlock(&handle->queue_lock);
    sem_post(&handle->semaphore);
  }
  return;
}
void announceEvent(void *handle,const char *eventName,void *info)
  __attribute__((alias("announce_event")));;

/* stop an EventLoop
 * the event loop stops when the current running handler, if any, finishes
 * the behavior is undefined if the EventLoop is not running
 * the behavior is also undefined if this function is not called by a
 * handler run by the EventLoop
*/
void stop_event_loop(event_loop_handle handle){
  if(atomic_load_n(&handle->state) != 2){
    fprintf(stderr,"Error, Trying to stop an already stopped event loop\n");
    abort();
  } else {
    atomic_store_n(&handle->state,0);
  }
}
void stopEventLoop(void *handle) __attribute__((alias("stop_event_loop")));

/* cleanup any allocated memory for an EventLoop
 * the behavior is undefined if the EventLoop is not stopped
 */
void cleanup_event_loop(event_loop_handle handle){
  HERE();
  if(atomic_load_n(&handle->state)){
    HERE();
    abort();
  }
  pthread_spin_lock(&handle->queue_lock);
  pthread_rwlock_wrlock(&handle->alist_lock);
  //we can assume this will only even be called when
  //no other thread is modifying handle
  cons *temp;
  cons *alist=handle->event_alist;
  HERE();
  while(alist){
    temp=XCDR(alist);
    if(XCAR(alist)){
      free(XCAAR(alist));
      free(XCAR(alist));
    }
    free(alist);
    alist=temp;
  }
  HERE();
  cons *node=handle->handler_queue.head;
  while(node && (temp=XCDR(node))){
    free(node);
    node=temp;
  }
  HERE();
  pthread_spin_unlock(&handle->queue_lock);
  pthread_rwlock_unlock(&handle->alist_lock);
  pthread_spin_destroy(&handle->queue_lock);
  pthread_rwlock_destroy(&handle->alist_lock);
  sem_destroy(&handle->semaphore);
  free(handle);
}
void cleanupEventLoop(void *handle)
  __attribute__((alias("cleanup_event_loop")));
