#include "echo.h"
void __attribute__((noreturn)) start_server(const char *hostname, uint16_t port){
  pid_t pid = fork();
  if(pid == 0){
    fprintf(stderr, "Running server as process %ld\n", getpid());
    char buf[128];
    sprintf(buf, "echo_%d.log", getpid());
    if(!freopen(buf, "w", stderr)){
      perror("freopen");
      exit(EXIT_FAILURE);
    }
    setvbuf(stderr, NULL, _IOLBF, 0);
    run_server(port);
  } else {
    exit(EXIT_SUCCESS);
  }
}
void usage(){
  printf("Usage: echo [options] [outfile] [infile]\n"
         "Options:\n"
         "\t--help: Display this help and exit\n"
         "\t-h|--host hostname: Specify the hostname of the server\n"
         "\t-p|--port port: Specify the port of the server\n"
         "\t-s|--server: Start the server (default is to connect as a client\n"
         "\t-i|--input infile: Explicitly specify the input file\n"
         "\t-o|--output outfile: Explicitly specify the output file\n");
  exit(0);
}
int main(int argc, char *argv[]){
  if(argc > 1){
    start_server(default_host, default_port);
  } else {
    run_client(default_host, default_port, stdin, stdout);
  }
}
