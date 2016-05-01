#include "echo.h"
#include <pthread.h>
static const size_t msgbufsz = 4096;
//echo all lines in buf to sock, there must be at least one line in buf, which
//starts at the start of buf and ends at line_end, line_end < buflen.
//If is an unterminated line left in buf it is copied to the begining of buf and
//its size is returned, otherwise 0 is returned.
size_t echo_lines(int sock, char *buf, size_t buflen, size_t line_end){
  char *bufptr = buf;
  do {
    send(sock, bufptr, line_end + 1, 0);
    bufptr += (line_end+1);
    char *newline = memchr(bufptr, '\n', buflen-(line_end+1));
    if(!newline){break;}
    line_end += newline - bufptr;//noop on the first iteration
  } while(1);
  if(buflen > line_end){
    //copy any unused bytes to the begining of the buffer
    memcpy(buf, buf+line_end, buflen - line_end);
    return buflen - line_end;
  } else {
    return 0;
  }
}
void handle_client(int sock){
  //We read at most 4096 bytes at a time from the socket, if there are
  //more than 4096 bytes than we store the bytes in buf until we've read
  //the entire message.
  size_t bufsz = msgbufsz;
  size_t buflen = 0;
  char *buf = xmalloc(bufsz);
  char *msgbuf[msgbufsz];
  while(1){
    ssize_t nbytes = recv(sock, msgbuf, msgbufsz, 0);
    //Not that anyone is going to read this exit status
    if(nbytes < 0){
      perror("recv");
      pthread_exit(EXIT_FAILURE);
    } else if(nbytes == 0){
      pthread_exit(EXIT_SUCCESS);
    }
    //This probably does more copying that necessary
    char *newline = memchr(msgbuf, '\n', msgbufsz);
    if(newline){
      if(buflen != 0){


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
