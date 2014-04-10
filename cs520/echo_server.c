/*Accept connections from clients, for each client asynchronously
  read data until some sentinal value then asynchronously write that 
  data back to the same client.
*/
#include <sys/socket.h>
#include <stdio.h>
#include <aio.h>
#include <stdint.h>
#include <stlib.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>
#include <netinet/in.h>//what kind of name is that
#include <endian.h>
#ifndef QUEUE_LENGTH
#define QUEUE_LENGTH 32
#endif
#define SOCK_READ (O_READ|O_ASYNC)
#define SOCK_WRITE (O_WRITE|O_ASYNC)
int make_inet_socked(uint16_t port){
  int sock;
  struct sockaddr_in name={.sin_family=AF_INET,.sin_port=htobe16(port),
                           .sin_addr=htobe32(INADDR_ANY)};
  if((sock = socket(PF_INET,SOCK_STREAM,0))==-1){
    perror("socket failure");
    exit(1);
  }
  if (bind (sock, (struct sockaddr *) &name, sizeof (name)) < 0){
    perror ("bind failure");
    exit (1);
  }
  return sock;
}

/*
Asynchronous I/O control block.
struct aiocb
{
  int aio_fildes;		// File desriptor.  
  int aio_lio_opcode;		// Operation to be performed.  
  int aio_reqprio;		// Request priority offset.  
  volatile void *aio_buf;	// Location of buffer.  
  size_t aio_nbytes;		// Length of transfer.  
  struct sigevent aio_sigevent;	// Signal number and value.  

  // Internal members.  
  struct aiocb *__next_prio;
  int __abs_prio;
  int __policy;
  int __error_code;
  __ssize_t __return_value;

#ifndef __USE_FILE_OFFSET64
  __off_t aio_offset;		// File offset.  
  char __pad[sizeof (__off64_t) - sizeof (__off_t)];
#else
  __off64_t aio_offset;		// File offset.  
#endif
  char __glibc_reserved[32];
};
*/
void server_loop(int server_sock){
  fd_set clients_rd,clients_wr;
  int num_clients=0;
  FD_ZERO(&clients);
  FD_ZERO(&clients_wr);
  FD_SET(&clients,server_sock);
  if(listen(server_sock,QUEUE_LENGTH)<0){
    perror("listen failure");
    exit(1);
  }
  while(1){
    if(select(num_clients+1,&clients_rd,&clients_wr,NULL,NULL)<0){
      perror("select failure");
      exit(1);
    }
    
