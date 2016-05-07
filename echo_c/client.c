#include "echo.h"
void print_family(struct addrinfo *aip){
  printf(" family: ");
  switch (aip->ai_family) {
    case AF_INET:
      printf("inet");
      break;
    case AF_INET6:
      printf("inet6");
      break;
    case AF_UNIX:
      printf("unix");
      break;
    case AF_UNSPEC:
      printf("unspecified");
      break;
    default:
      printf("unknown");
  }
}
void print_type(struct addrinfo *aip){
  printf(" type: ");
  switch (aip->ai_socktype) {
    case SOCK_STREAM:
      printf("stream");
      break;
    case SOCK_DGRAM:
      printf("datagram");
      break;
    case SOCK_SEQPACKET:
      printf("seqpacket");
      break;
    case SOCK_RAW:
      printf("raw");
      break;
    default:
      printf("unknown (%d)", aip->ai_socktype);
  }
}
void print_protocol(struct addrinfo *aip){
  printf(" protocol: ");
  switch (aip->ai_protocol) {
    case 0:
      printf("default");
      break;
    case IPPROTO_TCP:
      printf("TCP");
      break;
    case IPPROTO_UDP:
      printf("UDP");
      break;
    case IPPROTO_RAW:
      printf("raw");
      break;
    default:
      printf("unknown (%d)", aip->ai_protocol);
  }
}
void print_addrinfo(struct addrinfo *aip){
  print_family(aip);
  print_type(aip);
  print_protocol(aip);
  if(aip->ai_family == AF_INET){
    printf(" ip-addr: %ld", ((struct sockaddr_in*)aip->ai_addr)->sin_addr.s_addr);
  }
  fputs("\n", stdout);
}

int connect_client(const char *server, uint16_t port){
  //Create a sockaddr_in struct representing the desired server
  struct addrinfo hint = {0};
  hint.ai_family = AF_INET;
  hint.ai_socktype = SOCK_STREAM;
  struct addrinfo *server_addr = getaddrbyname(server, &hint);
  if(!server_addr){
    exit(EXIT_FAILURE);
  }
  int sock, err;
  struct addrinfo *aip;
  for(aip = server_addr; aip != NULL; aip = aip->ai_next){
    print_addrinfo(aip);
    if((sock = socket(PF_INET, SOCK_STREAM, 0/*TCP*/)) < 0){
      perror("socket");
      exit(EXIT_FAILURE);
    }
    if(connect(sock, aip->ai_addr, aip->ai_addrlen) < 0){
      err = errno;
    } else {
      return sock;
    }
  }
  errno = err;
  perror("connect");
  exit(EXIT_FAILURE);
}
ssize_t svector_getline(svector *vec, FILE *in){
  int c;
  ssize_t start_len = svector_len(vec);
  while((c = fgetc(in)) != EOF){
    svector_push_byte((unsigned char)c, vec);
    if(c == '\n'){
      break;
    }
  }
  return svector_len(vec) - start_len;
}

#define BUFLEN 4096
//Read lines from the interactive input 'in' until EOF,
//send them to the server, read the same line back and write it to out
void __attribute__((noreturn)) echo_interactive(int sock, FILE *in, FILE *out){
  svector outbuf = make_svector(BUFLEN);
  svector inbuf = make_svector(BUFLEN);
  while(svector_getline(&inbuf, in) > 0){
    //We need to send the newline to allow the server to detect the end of the message
    ssize_t nbytes = send(sock, SVECTOR_BYTES(inbuf), SVECTOR_LEN(inbuf), 0);
    if(nbytes != SVECTOR_LEN(inbuf)){
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
    SVECTOR_RESERVE(outbuf, nbytes);
    //Since we get back the same message we sent it's
    //obviously going to be the same size
    nbytes = recv(sock, SVECTOR_BYTES(outbuf),
                  SVECTOR_LEN(outbuf), MSG_WAITALL);
    //since we pass MSG_WAITALL it's an error if we don't read all the bytes
    if(nbytes != SVECTOR_LEN(outbuf)){
      fprintf(stderr, "Error reading from socket\n");
      exit(EXIT_FAILURE);
    }
    //Remember we still have the newline, so no need to add another one
    fwrite(SVECTOR_DATA(outbuf), 1, SVECTOR_LEN(outbuf), stdout);
    SVECTOR_EMPTY(outbuf);
    SVECTOR_EMPTY(inbuf);
  }
  exit(EXIT_SUCCESS);
}
//For each line in the regular file 'in', send it to the server,
//get the same line back and write it to out.
void __attribute__((noreturn)) echo_file(int sock, FILE *in, FILE *out,
                                       size_t insz){
  fprintf(stderr, "Error echo_file not implemented\n");
  exit(EXIT_FAILURE);
  /*
  int fd = fileno(in);
  size_t bufsz = insz;
  //I'd normally use mmap, but we can reuse the buffer to recv messages
  //from the server without needing to know how long the longest line
  //in the file is.
  void *buf = xmalloc(insz);
  syscall_checked(read, fd, buf, bufsz);

  ssize_t nbytes = send(sock, buf, bufsz, 0);
  if(nbytes < bufsz){
    perror("send");
    exit(EXIT_FAILURE);
  }

  void *buf = buf_start;
  void *bufptr = buf;
  size_t buflen = insz;
  while((bufptr = memchr(buf, '\n', buflen))){
    size_t msglen = bufptr - buf;
    buflen -= msglen;
    ssize_t nbytes = send(sock, buf, buflen, 0);
  }
  */
}

void __attribute__((noreturn)) run_client(const char *server, uint16_t port,
                                         FILE *in, FILE *out){
  DEBUG_PRINTF("Connecting to %s on port %d\n", server, port);
  int sock = connect_client(server, port);
  DEBUG_PRINTF("Connected to %s on port %d", server, port);
  struct stat buf;
  fstat(fileno(in), &buf);
  int isreg_in = S_ISREG(buf.st_mode);
  if(isreg_in){
    echo_file(sock, in, out, buf.st_size);
  } else {
    echo_interactive(sock, in, out);
  }
  __builtin_unreachable;
}
