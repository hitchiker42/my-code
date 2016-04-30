#include "echo.h"
  //Bind the socket
  if(bind(sock, BITCAST(&addr, struct sockaddr *), sizeof(addr)) < 0){
    perror("bind");
    return -1;
  }
int connect_client(const char *server, uint16_t port, double maxwait){
  //Create a sockaddr_in struct representing the desired server
  struct addrinfo *server_addr = getaddrbyname(server);
  if(!server_addr){
    return -1;
  }
  struct sockaddr_in addr = {.sin_family = AF_INET,
                            .sin_port = htons(port),
                            .sin_addr = BITCAST(server_addr->ai_addr, struct in_addr)};
  //Attempt to connect to the server, if maxwait is non zero retry the connection
  //for upto maxwait seconds
  double cur_wait = 0.0, time_waited = 0.0;  
  int sock = -1;
  //I don't know why, but I prefer to use a label/goto when retrying something
  //rather than a for or while loop.
 retry:
  //Be portable, on linux we don't need to create a new socket each time,
  //but posix says that a socket is undefined after a failed call to connect
  if((sock = socket(PF_INET, SOCK_STREAM, 0/*TCP*/)) < 0){
    perror("socket");
    return -1;
  }
  int err = connect(sock, addr, server_addr->ai_addrlen);
  if(!err){
    return sock;
  }
  /*
    Something of an optimization, if the error is something that will
    prevent us from ever connecting, then don't bother retrying
    if(err == EBADF || err == EAFNOSUPPORT || err = EINVAL){
    return -1;
    }
  */
  if(time_waited < maxwait){
    float_sleep(cur_wait);
    time_waited += cur_wait;
    cur_wait *= 2;
    goto retry;
  }
  return -1;
}
  
void __attrtibute__(noreturn) run(const char *server, uint16_t port){
  int sock = connect_client(server, port, 0);//Don't try more that once
  if(sock < 0){
    exit(EXIT_FAILURE);
  }
  char *buf, *outbuf;
  size_t bufsz, outbufsz;
  ssize_t buflen;
  while((buflen = getline(&buf, &buflen, stdin)) > 0){
    buflen -= 1;//ignore the newline
    ssize_t nbytes = send(sock, buf, buflen-1);
    if(nbytes != buflen-1){
      fprintf(stderr, "Falied to write all bytes to socket\n");
      if(nbytes < 0){
        exit(EXIT_FAILURE);
      } else {
        continue;
      }
    }
    /*
      We could just reuse the buffer from getline, but that feels like cheating
     */
    if(bufsz > outbufsz){
      xrealloc(outbuf, bufsz);
      outbufsz = bufsz;
    }
    nbytes = recv(sock, outbuf, buflen, MSG_WAITALL);
    //since we pass MSG_WAITALL it's an error if we don't read
    //all the bytes
    if(nbytes != buflen){
      fprintf(stderr, "Error reading from socket\n");
      exit(EXIT_FAILURE);
    }
    fwrite(buf, 1, buflen, stdout);
    fputs("\n", stdout);
  }
  exit(EXIT_SUCCESS);
}
  
