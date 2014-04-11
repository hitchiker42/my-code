//
// this is is a single-threaded test of an EventLoop
//

#include <stdio.h>
#include <stdlib.h>

#include "EventLoop.h"

void printString(void *arg)
{
  printf("%s\n", (char *) arg);
}

void terminate(void *arg)
{
  stopEventLoop(arg);
}

void initialFunction(void *handle)
{
  registerEvent(handle, "printString", printString);
  registerEvent(handle, "terminate", terminate);

  announceEvent(handle, "printString", "hey");
  announceEvent(handle, "printString", "you");
  announceEvent(handle, "terminate", handle);
}

int main()
{
  // create an EventLoop
  void *handle = createEventLoop();
  if (handle == NULL)
  {
    fprintf(stderr, "createEventLoop failed!");
    exit(-1);
  }

  // start it
  //   trick is to pass the EventLoop handle in
  startEventLoop(handle, initialFunction, handle);

  cleanupEventLoop(handle);

  printf("main exiting....\n");

  return 0;
}

