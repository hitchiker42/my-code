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

#ifndef EVENTLOOP_H
#define EVENTLOOP_H
#include <stdint.h>
typedef struct tail_queue fifo_queue;
typedef struct queue_node queue_node;
struct queue_node {
  queue_node *prev;//NULL for head node
  queue_node *next;//NULL for tail node
  void *data;//this is probably a pointer to a 
  //struct {hanlder_fn handler; void *client_data};
};
struct tail_queue {
  queue_node *head;
  queue_node *tail;
};
typedef void (*handler_fn)(void *);
typedef struct event_loop_data *event_loop_handle;
struct event_loop_data {
  fifo_queue handler_queue;//queue of cons cells (handler_fn . client_data)
  cons *event_alist;//list of the form ((event . handler)...)
  sem_t semaphore;//announce event increments this 
  int state;
  //and it is decremented when a handler is run
};
typedef struct cons cons;
#define NIL NULL
struct cons {
  void *car;
  void *cdr;
};
#define XCAR(cons) cons->car
#define XCDR(cons) cons->cdr
static void queue_cleanup_recursive(fifo_queue *queue){
  queue_node *current_node=queue->head,*last_node;
  while(current_node){
    last_node=current_node;
    current_node=last_node->next;
    xfree(last_node->data);
    xfree(last_node);
  }
  return;
}
static inline fifo_queue *queue_push(fifo_queue *queue,void *data){
  queue_node *new_head=xcalloc(sizeof(queue_node));
  new_head->data=data;
  new_head->next=queue->head;
  if(queue->head){
    queue->head->prev=new_head;
  }
  queue->head=new_head;
  return queue;
}
static inline queue_node *queue_pop(fifo_queue *queue){
  queue_node *retval=queue->head;
  queue->head=queue->head->next;
  if(queue->head){
    queue->head->prev=NULL;
  }
  return retval;
}
static inline queue_append(fifo_queue *queue,void *data){
  queue_node *new_tail=xcalloc(sizeof(queue_node));
  new_tail->data=data;
  new_tail->prev=queue->tail;
  if(queue->tail){
    queue->tail->next=new_tail;
  }
  queue->tail=new_tail;
  return queue;
}
static inline queue_node *queue_poplast(fifo_queue *queue){
  queue_node *retval=queue->last;
  queue->last=queue->last->prev;
  if(queue->last){
    queue->last->next=NULL;
  }
  return retval;
}
cons* Fcons(void *car, void *cdr){
  cons *retval=xmalloc(sizeof(cons));
  cons->car=car;
  cons->cdr=cdr;
  return retval;
}
cons *assoc(cons *list,void *key,int (*cmp_fn)(void*,void*)){
  while(list->cdr != NIL){
    if(cmp_fn(list->car,key)){
      return list;
    }
    list=list->cdr;//no typechecking or anything
  }
  return NIL;
}
int event_cmp(const char *a,const char *b){
  return !strcmp(a,b);
}
//here event is (const char *name . void *client_data)
handler_fn get_event_handler(cons *handler_cons,cons *event){
  if((handler_cons=assoc(handle->event_alist,event->car,event_cmp))){
    return XCDR(handler_cons);
  }
  return NIL;
}
void run_handler(event_loop_handle handle){
  cons *handler=queue_poplast(handle->event_queue);  
  (*XCAR(handler))(XCDR(handler));
}
void announce_event(event_loop_handle handle ,char *event_name,
                    void *client_data){
  cons *event=Fcons(event_name,client_data);
  handler_fn event_handler=get_event_handler(handle->event_alist,event);
  if(handler_fn == NIL){
    unhandled_event(XCAR(event));
  } else {
    queue_push(handle->handler_queue,Fcons(handler_fn,XCDR(event)));
    sem_post(handler->semaphore);
  }
  return;
}
void main_loop(event_loop_handle handle){
  while(handle->state > 0){
    sem_wait(handle->semaphore);
    run_handler(handle);
  }
}
void stop_event_loop(event_loop_handle handle){
  //reads/writes are atomic, this just adds an mfence after
  //moving the data
  atomic_store_n(&handle->state,0);//0 = normal_termination;
  return;
}
  
/* create an EventLoop
 * returns a "handle" for the EventLoop or NULL on failure
 * defined as
 */
event_loop_handle create_event_loop();
void *createEventLoop(void)
  __attribute__((alias("create_event_loop")));

/* start an EventLoop
   the behavior is undefined if an EventLoop is started more than once
*/
void start_event_loop(event_loop_handle,handler_fn,void*);
void startEventLoop(void *handle, handler_fn initialFunc,
                    void *initialArgument)
  __attribute__((alias("start_event_loop")));

/* register a handler for an event
 * if a handler is already defined for the event then it is overridden
 */
void register_event(event_loop_handle,const char *,handler_fn);
void registerEvent(void *handle,const char *eventName, handler_fn handler)
  __attribute__((alias("register_event")));
/* (let ((old-event (assoc (slot-value handler event_alist) eventName)))
   (if (not (null old-event))
   (set-cdr old-event handler)
   (setf (slot-value handler event_alist) 
   (cons (cons eventName handler) (slot-value handler event_alist)))))
*/

/* announce that an event has occurred
 * if an handler is registered for it, the handler function will be
 * enqueued for execution along with its "info" argument
 * if no handler is registered for it, then the EventLoop prints
 * "unhandled event: " followed by the event name and a newline, and
 * then continues
*/
void announce_event(event_loop_handle,char *,void *);
void announceEvent(void *handle,const char *eventName,void *info)
  __attribute__((alias("announce_event")));;

/* stop an EventLoop
 * the event loop stops when the current running handler, if any, finishes
 * the behavior is undefined if the EventLoop is not running
 * the behavior is also undefined if this function is not called by a
 * handler run by the EventLoop
*/
void stop_event_loop(event_loop_handle);
void stopEventLoop(void *handle) __attribute__((alias("stop_event_loop")));

/* cleanup any allocated memory for an EventLoop
 * the behavior is undefined if the EventLoop is not stopped
 */
void cleanup_event_loop(event_loop_handle);
void cleanupEventLoop(void *handle)
  __attribute__((alias("cleanup_event_loop")));

#endif
