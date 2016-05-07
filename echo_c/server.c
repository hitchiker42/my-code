#include "echo.h"
#include <pthread.h>
static const size_t msgbufsz = 4096;
/*
  echo all lines in buf to sock, there must be at least one line in buf, which
  starts at the start of buf and ends at line_end, line_end < SVECTOR_LEN(buf).
  If there is an unterminated line left in buf it is copied to the begining
  of buf.
*/
void echo_lines(int sock, svector *buf, size_t line_end){
  uint8_t *bufptr = svector_bytes(buf);
  size_t buflen = svector_len(buf);
  svector_push_byte('\0', buf);
  DEBUG_PRINTF("Echoing from buffer %s\n", svector_bytes(buf));
  do {
    send(sock, bufptr, line_end + 1, 0);
    bufptr += (line_end+1);
    uint8_t *newline = memchr(bufptr, '\n', buflen-(line_end+1));
    if(!newline){break;}
    line_end += newline - bufptr;
  } while(1);
  svector_empty(buf);
  if(buflen > (line_end+1)){
    svector_multipush_bytes(buf, bufptr, buflen - line_end);
    svector_push_byte('\0', buf);
    DEBUG_PRINTF("buflen = %d, line_end = %d, buffer = %s",
                 buflen, line_end, svector_bytes(buf));
  }
  return;
}
void handle_client(int sock){
  //We read at most 4096 bytes at a time from the socket, then look for a
  //newline in the bytes we've read, and if we find one echo back any lines
  //afterwards we copy any excess bytes to buf and try to read more bytes.
  svector buf = make_svector(msgbufsz * 2);
  uint8_t *msgbuf[msgbufsz];
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
    uint8_t *newline = memchr(SVECTOR_BYTES(buf), '\n', SVECTOR_LEN(buf));
    if(newline){
      size_t line_end = newline - SVECTOR_BYTES(buf);
      echo_lines(sock, &buf, line_end);
    }
  }
}

int init_server(uint16_t port){
  int sock;
  struct sockaddr_in addr;
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  DEBUG_PRINTF("Creating server socket\n");
  if((sock = socket(PF_INET, SOCK_STREAM, 0/*TCP*/)) < 0){
    perror("socket");
    exit(EXIT_FAILURE);
  }
  //Bind the socket
  DEBUG_PRINTF("Binding server socket\n");
  if(bind(sock, BITCAST(&addr, struct sockaddr *), sizeof(addr)) < 0){
    perror("bind");
    exit(EXIT_FAILURE);
  }
  return sock;
}
void print_exit(){
  fprintf(stderr, "Exiting server\n");
}
void __attribute__((noreturn)) run_server(uint16_t port){
  atexit(print_exit);
  int serv = init_server(port);
  DEBUG_PRINTF("Listening on port %d\n", port);
  int err = listen(serv, 128);//we need to give a limit, 128 should be enough
  if(err < 0){
    perror("listen");
    exit(EXIT_FAILURE);
  }
  pthread_attr_t attr;
  pthread_t client;
  struct sockaddr client_addr;
  socklen_t client_len;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  DEBUG_PRINTF("Accepting clients\n");
  while(1){
    int client_sock = accept(serv, &client_addr, &client_len);
    if(client_sock < 0){
      perror("accept");
      exit(EXIT_FAILURE);
    }
    pthread_create(&client, &attr, (void*(*)(void*))handle_client,
                   (void*)(intptr_t)client_sock);
  }
}
