
static inline void gai_perror(int err){
  if(err == EAI_SYSTEM){
    perror("getaddrinfo error");
  } else {
    fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(err));
  }
}
alarm_state *alarmd_init(const char *addr, const char *port){
  int sock,error;
  struct addrinfo *results, *res;
  struct addrinfo hints = {.ai_socktype = SOCK_STREAM,
                           .ai_protocol = tcp_proto_number};
  if(!addr){
    addr = "localhost";
  }
  if(!port){
    port = STRINGIFY(DEFAULT_PORT);
  }
  error = getaddrinfo(addr, port, NULL, &results);
  if(error){
    gai_perror(error);
    return NULL;
  }
  for(res = results; res != NULL; res=res->ai_next){
    sock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
    if(sock == -1){continue;}
    if(bind(sock, res->ai_addr, res->ai_addrlen) == 0){
      break;
    }
    close(sock);
    sock=-1;
  }
  freeaddrinfo(results);
  if(sock == -1){
    fprintf(stderr, "unable to connect to %s:%s\n", addr, port);
    return NULL;
  }
