#include "util.h"
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netdb.h>
//wrapper around getaddrinfo which basically acts the same as gethostbyname
struct addrinfo *getaddrbyname(const char *name){
  struct addrinfo *ret;
  int err = getaddrinfo(name, NULL, NULL, &ret);
  if(err){
    const char *errstr = gai_strerror(err);
    fprintf(stderr, "Error looking up host name %s:\n%s\n", name, errstr);
    return NULL;
  }
  return ret;
}

int connect_client(const char *server, uint16_t port){
  int sock = socket(PF_INET, SOCK_STREAM, 0/*TCP*/);
  struct addrinfo *server_addr = getaddrbyname(server);
  if(!server_addr){
    exit(EXIT_FAILURE);
  }

  struct sockaddr_in addr = {.sin_family = AF_INET,
                            .sin_port = htons(port),
                            .sin_addr = BITCAST(server_addr->ai_addr, struct in_addr)};
  if(bind(sock, BITCAST(&addr, struct sockaddr *), sizeof(addr)) < 0){
    perror("bind");
    exit(EXIT_FAILURE);
  }
  return sock;
}
  
void ATRIBUTE_NORETURN run(const char *server, uint16_t port){
  
