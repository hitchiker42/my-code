#ifndef __ECHO_H__
#define __ECHO_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "util.h"
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netdb.h>
//wrapper around getaddrinfo which basically acts the same as gethostbyname
static struct addrinfo *getaddrbyname(const char *name){
  struct addrinfo *ret;
  int err = getaddrinfo(name, NULL, NULL, &ret);
  if(err){
    const char *errstr = gai_strerror(err);
    fprintf(stderr, "Error looking up host name %s:\n%s\n", name, errstr);
    return NULL;
  }
  return ret;
}

#ifdef __cplusplus
}
#endif
#endif /* __ECHO_H__ */
