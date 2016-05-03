#ifndef __ECHO_H__
#define __ECHO_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "util.h"
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netdb.h>
static const char *default_host = "localhost";
static const uint16_t default_port = 21012;
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
void __attribute__((noreturn)) run_server(const char *hostname, uint16_t port);
void __attribute__((noreturn)) run_client(const char *server, uint16_t port,
                                         FILE *in, FILE *out);
#ifdef __cplusplus
}
#endif
#endif /* __ECHO_H__ */
