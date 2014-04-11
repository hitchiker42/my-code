/*
 * Interface to an event-driven generic Stream mechanism.
 *
 * A Stream will generate a series of "data" events, followed
 * by one "end" event.
 *
 * The Stream requires an EventLoop to be active to which it
 * will announce events.
 *
 * The Stream is generic in that a "produce" function must be
 * supplied when the Stream is created. The Stream will call
 * this function to generate the next piece of data. When this
 * function returns NULL, the Stream will announce the "end"
 * event.
 *
 * A second function must be supplied when the Stream is created,
 * which will be called by the Stream when it is started in order
 * to initialize the "producer". This function returns an opaque
 * handle that the Stream will always pass to the "produce" function.
 *
 * The Stream does not concern itself with allocation/deallocation
 * of any buffers that may be passed through the stream. This must
 * be coordinated by the producer (implementing the "produce" function)
 * and the consumer (responding to the "data" events).
 */

#ifndef STREAM_H
#define STREAM_H
pthread_attr_t stream_thread_attr;
pthread_once_t stream_once=PTRHEAD_ONCE_INIT;
void stream_attr_init(){
  pthread_attr_init(&stream_thread_attr);
  pthread_attr_setdetachstate(&stream_thread_attr,PTHREAD_CREATE_DETACHED);
}
typedef void*(produce_fn)(void*);
typedef void*(init_fn)(void*);
typedef struct stream_data *stream_handle;
struct stream_data {
  const char *data_event_name;
  const char *end_event_name;
  produce_fn *produce;
  init_fn *init;
  event_loop_handle handle;
};
stream_handle create_stream(const char *data_event_name,const char *end_event_name,
                            produce_fn produce,init_fn init,event_loop_handle handle){
  struct stream_data *new_stream=xmalloc(sizeof(struct stream_data));
  char *data_event_copy=xmalloc(strlen(data_event_name));//inefficent use of strlen, but eh
  strcpy(data_event_copy,data_event_name);
  char *end_event_copy=xmalloc(strlen(end_event_name));//inefficent use of strlen, but eh
  strcpy(end_event_copy,end_event_name);
  *new_stream=(struct stream_data){.data_event_name=data_event_copy,.produce=produce
                                   .end_event_name=end_event_copy,.init=init,
                                   .handle=handle};
  return new_stream;
}
/* create a Stream
 * returns a "handle" to be passed into startStream and cleanupStream
 * returns NULL if the create fails
 * internal copies are made of the event-name strings
 */
stream_handle create_stream(const char*,const char*,produce_fn,init_fn,
                            event_loop_handle);
void * createStream(const char *dataEventName,const char *endEventName,
                    produce_fn *produce,init_fn *initialize,
                    void *eventLoopHandle)
  __attribute__((alias("create_stream")));

/* start a Stream
 * first calls the "initialize" function
 * if it returns NULL, then this function returns 0
 * then starts a thread that will repeatedly and asynchronously call "produce"
 * returns 1
 */
int start_stream(stream_handle handle,void *client_data){
  void *init_retval=handle->init_fn(client_data);
  if(init_retval==NULL){
    return 0;
  } else {
    pthread_once(&stream_once,stream_attr_init);
    pthread_t new_thread;
    if(pthread_create(&new_thread,&stream_attr_init,
                      handle->produce,client_data)!=0){
      perror("pthread create failed");
      exit(1);
    }
    return 1;
  }
}
    
  
int start_stream(stream_handle,void*);
int startStream(void *handle,void *initializeArg)
  __attribute__((alias("start_stream")));

/* cleanup a Stream
 * cleans up any memory used by the Stream
 */
void cleanup_stream(stream_handle handle){
  xfree(handle->data_event_name);
  xfree(handle->end_event_name);
  xfree(handle);
  return;
}
void cleanup_stream(stream_handle);
void cleanupStream(void *handle )
  __attribute__((alias("cleanup_stream")));

#endif

