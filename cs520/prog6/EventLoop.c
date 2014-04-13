/*
* Interface to an EventLoop
* A handler function can be registered for an event, as events occur
* handler functions are put into an fifo queue and executed asynchronously.
* Events will be accepted event when handler functions are being run. If
* there are no functions left to run the loop will wait until an event occurs

* events are identified by strings
*
* the event loop does NOT start a new thread to execute the event loop
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
//do this instead of making everything static
//#pragma GCC visibility push("hidden")
//I didn't write these versions of the checked memory allocation I got
//them from gnulib, but really they're pretty much the same functions everywhere
/* Allocate N bytes of memory dynamically, with error checking.  */
void *xmalloc (size_t n){
  void *p = malloc (n);
  if (!p && n != 0){
    perror("out of memory");
    exit(1);
  }
  return p;
}

/* Change the size of an allocated block of memory P to N bytes,
   with error checking.  */

void *xrealloc (void *p, size_t n){
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
void *xcalloc (size_t s){
  return memset(xmalloc (s), 0, s);
}
void *xmemdup (void const *p, size_t s){
  return memcpy (xmalloc (s), p, s);
}

/* Clone STRING.  */

char *xstrdup (char const *string){
  return xmemdup (string, strlen (string) + 1);
}

#define FUNCALL_VOID(f,args...) (*(void(*)())f)(args)
#define NIL NULL
#define XCAR(cons) cons->car
#define XCDR(cons) cons->cdr
#define PUSH(obj,cons) (cons->cdr=cons,cons->car=obj,cons)
#define POP(cons) \
  ({void *retval=XCAR(cons);                    \
    cons=XCDR(cons);                            \
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
struct queue_node {
  queue_node *prev;//NULL for head node
  queue_node *next;//NULL for tail node
  void *data;//this is probably a pointer to a 
  //struct {hanlder_fn handler; void *client_data};
};
struct cons {
  void *car;
  void *cdr;
};
static const cons nil={NIL,NIL};
struct tail_queue {
  queue_node *head;
  queue_node *tail;
};
struct event_loop_data {
  tail_queue handler_queue;//queue of cons cells (handler_fn . client_data)
  cons *event_alist;//list of the form ((event . handler)...)
  sem_t semaphore;//announce event increments this 
  //and it is decremented when a handler is run
  pthread_spinlock_t queue_lock;
  pthread_rwlock_t alist_lock;
  int state;//running or not
};
static inline cons* Fcons(void *car, void *cdr){
  cons *retval=xmalloc(sizeof(cons));
  retval->car=car;
  retval->cdr=cdr;
  return retval;
}
static void queue_cleanup_recursive(tail_queue *queue){
  queue_node *current_node=queue->head,*last_node;
  while(current_node){
    last_node=current_node;
    current_node=last_node->next;
    free(last_node->data);
    free(last_node);
  }
  return;
}
static inline tail_queue *queue_push(tail_queue *queue,void *data){
  queue_node *new_head=xcalloc(sizeof(queue_node));
  new_head->data=data;
  new_head->next=queue->head;
  if(queue->head){
    queue->head->prev=new_head;
  }
  queue->head=new_head;
  return queue;
}
static inline void *queue_pop(tail_queue *queue){
  queue_node *retval=queue->head;
  queue->head=queue->head->next;
  if(queue->head){
    queue->head->prev=NULL;
  }
  return retval->data;
}
static inline tail_queue* queue_append(tail_queue *queue,void *data){
  queue_node *new_tail=xcalloc(sizeof(queue_node));
  new_tail->data=data;
  new_tail->prev=queue->tail;
  if(queue->tail){
    queue->tail->next=new_tail;
  }
  queue->tail=new_tail;
  return queue;
}
static inline void*queue_poplast(tail_queue *queue){
  queue_node *retval=queue->tail;
  queue->tail=queue->tail->prev;
  if(queue->tail){
    queue->tail->next=NULL;
  }
  return retval->data;
}
//takes a literal cons cell, so 
static cons* assoc(cons* list,void *key,int (*cmp_fn)(void*,void*)){
  while(list->cdr != NIL){
    if(cmp_fn(list->car,key)){
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
    return XCDR(handler_cons);
  }
  return NIL;
}
static void run_handler(event_loop_handle handle){
  pthread_spin_lock(&handle->queue_lock);
  cons *handler=queue_poplast(&handle->handler_queue);  
  pthread_spin_unlock(&handle->queue_lock);
  FUNCALL_VOID(XCAR(handler),XCDR(handler));
}
void main_loop(event_loop_handle handle){
  while(handle->state > 0){
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
    xmalloc(sizeof(struct event_loop_data));
  new_event_loop->state=1;
  sem_init(&new_event_loop->semaphore,0,0);
  pthread_spin_init(&new_event_loop->queue_lock,0);
  pthread_rwlock_init(&new_event_loop->alist_lock,NULL);
  return new_event_loop;
};  
void *createEventLoop(void)
  __attribute__((alias("create_event_loop")));

/* start an EventLoop
   the behavior is undefined if an EventLoop is started more than once
*/
void start_event_loop(event_loop_handle handle, handler_fn fn, void* user_data){
  if(atomic_load_n(&handle->state) != 1){
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
  cons *event=Fcons(event_name,client_data);
  pthread_rwlock_rdlock(&handle->alist_lock);
  handler_fn event_handler=get_event_handler(handle,event);
  pthread_rwlock_unlock(&handle->alist_lock);
  if(event_handler == NIL){
    unhandled_event(XCAR(event));
  } else {
    pthread_spin_lock(&handle->queue_lock);
    queue_push(&handle->handler_queue,Fcons(event_handler,XCDR(event)));
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
  if(atomic_load_n(&handle->state)){
    abort();
  } else {
    handle->state=0;
  }
}
void stopEventLoop(void *handle) __attribute__((alias("stop_event_loop")));

/* cleanup any allocated memory for an EventLoop
 * the behavior is undefined if the EventLoop is not stopped
 */
void cleanup_event_loop(event_loop_handle handle){
  if(handle->state){
    abort();
  }
  //we can assume this will only even be called when
  //no other thread is modifying handle
  cons *handler;
  while((handler=POP(handle->event_alist))){
    free(XCAR(handler));
    free(handler);
  }
  queue_cleanup_recursive(&handle->handler_queue);
  free(handle);
}
  
void cleanupEventLoop(void *handle)
  __attribute__((alias("cleanup_event_loop")));
