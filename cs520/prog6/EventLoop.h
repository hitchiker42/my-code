//
// Interface to an EventLoop
//
// a FIFO queue of function pointers is maintained
//
// when an event occurs its handler function, if one is defined, is put on the
// end of the queue
//
// when a function comes to the front of the queue it is removed and called
//
// if the queue of functions is empty, then the event loop blocks to wait
// for the next event.
//
// events are identified by strings
//
// the event loop does NOT start a new thread to execute the event loop
//

#ifndef EVENTLOOP_H
#define EVENTLOOP_H

// create an EventLoop
//   returns a "handle" for the EventLoop or NULL on failure
//
void *createEventLoop(void);

// start an EventLoop
//   the behavior is undefined if an EventLoop is started more than once
//
void startEventLoop(
  void *handle,                      // "handle" for EventLoop
  void (*initialFunc)(void *),       // initial function to execution
  void *initialArgument              // argument for initial function call
);

// register a handler for an event
//   if a handler is already defined for the event then it is overridden
//
void registerEvent(
  void *handle,                      // "handle" for EventLoop
  const char *eventName,             // name of the event
  void (*handler)(void *)            // handler for the event
);

// announce that an event has occurred
//   if an handler is registered for it, the handler function will be
//     enqueued for execution along with its "info" argument
//   if no handler is registered for it, then the EventLoop prints
//     "unhandled event: " followed by the event name and a newline, and
//     then continues
//
void announceEvent(
  void *handle,                      // "handle" for EventLoop
  const char *eventName,             // name of the event
  void *info
);

// stop an EventLoop
//   the event loop stops when the current running handler, if any, finishes
//   the behavior is undefined if the EventLoop is not running
//   the behavior is also undefined if this function is not called by a
//     handler run by the EventLoop
//
void stopEventLoop(
  void *handle                       // "handle" for EventLoop
);

// cleanup any allocated memory for an EventLoop
//   the behavior is undefined if the EventLoop is not stopped
//
void cleanupEventLoop(
  void *handle                       // "handle" for EventLoop
);

#endif
