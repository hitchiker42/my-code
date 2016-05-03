#include "echo.h"
#include <pthread.h>
static const size_t msgbufsz = 4096;
/*
  echo all lines in buf to sock, there must be at least one line in buf, which
  starts at the start of buf and ends at line_end, line_end < SVECTOR_LEN(buf).
  If there is an unterminated line left in buf it is copied to the begining
  of buf.
*/
void echo_lines(int sock, svector buf, size_t line_end){
  char *bufptr = SVECTOR_BYTES(buf);
  size_t buflen = SVECTOR_LEN(buf);
  do {
    send(sock, bufptr, line_end + 1, 0);
    bufptr += (line_end+1);
    char *newline = memchr(bufptr, '\n', buflen-(line_end+1));
    if(!newline){break;}
    line_end += newline - bufptr;
  } while(1);
  SVECTOR_LEN(buf) = 0;
  if(buflen > line_end){
    SVECTOR_MULTIPUSH_BYTES(buf, bufptr, buflen - line_end);
  }
  return;
}
void handle_client(int sock){
  //We read at most 4096 bytes at a time from the socket, then look for a
  //newline in the bytes we've read, and if we find one echo back any lines
  //afterwards we copy any excess bytes to buf and try to read more bytes.
  svector buf = make_svector(msgbufsz * 2);
  char *msgbuf[msgbufsz];
  while(1){
    ssize_t nbytes = recv(sock, msgbuf, msgbufsz, 0);
    //Not that anyone is going to read this exit status
    if(nbytes < 0){
      perror("recv");
      pthread_exit(EXIT_FAILURE);
    } else if(nbytes == 0){//0 means sock was closed cleanly.
      pthread_exit(EXIT_SUCCESS);
    }
    //This probably does more copying that necessary
    SVECTOR_MULTIPUSH_BYTES(buf, msgbuf, nbytes);
    char *newline = memchr(SVECTOR_BYTES(buf), '\n', SVECTOR_LEN(buf));
    if(newline){
      size_t line_end = newline - SVECTOR_BYTES(buf);
      echo_lines(sock, buf, line_end);
    }
  }
}

int init_server(const char *hostname, uint16_t port){
  struct addrinfo *server_addr = getaddrbyname(server);
  if(!server_addr){
    return -1;
  }
  struct sockaddr_in addr = {.sin_family = AF_INET,
                            .sin_port = htons(port),
                            .sin_addr = BITCAST(server_addr->ai_addr, struct in_addr)};
  if((sock = socket(PF_INET, SOCK_STREAM, 0/*TCP*/)) < 0){
    perror("socket");
    return -1;
  }
  //Bind the socket
  if(bind(sock, BITCAST(&addr, struct sockaddr *), sizeof(addr)) < 0){
    perror("bind");
    return -1;
  }
  return sock;
}
void __attribute__((noreturn)) run_server(const char *hostname, uint16_t port){
  int serv = init_server(hostname, port);
  int err = listen(serv, 128);//we need to give a limit, 128 should be enough
  pthread_attr_t *attr;
  pthread_t client;
  struct sockaddr client_addr;
  socklen_t client_len;
  pthread_attr_init(attr);
  pthread_attr_set_detachstate(PTHREAD_CREATE_DETACHED);
  while(1){
    int client_sock = accept(serv, &client_addr, &client_len);
    if(client_sock < 0){
      perror(accept);
      exit(EXIT_FAILURE);
    }
    pthread_create(&client, attr, void*(*)(void*)handle_client,
                   (void*)(intptr_t)client_sock);
  }
}
