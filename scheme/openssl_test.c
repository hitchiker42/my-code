#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <openssl/bio.h>
#include <openssl/ssl.h>
#include <openssl/err.h>

#ifndef NDEBUG
#define DEBUG_PRINTF(fmt, ...) fprintf(stderr, fmt, ##__VA_ARGS__)
#else
#define DEBUG_PRINTF(fmt, ...)
#endif
//This is a file with a set of root certificates, I don't really intend
//for anyone but me to use this so I'm just hardcoding the path, it shouldn't
//be to hard to change it should the need arise.
const char *certificates_location = "/etc/ssl/cert.pem";
/*
  list of ciphers to use. HIGH = use ciphers with at least 128 bits
  !aNULL = remove ciphers which don't require authentication
  !kRSA = don't use rsa for key exchange (doesn't offer forward security
  !SRP = not sure, not in the man page, but it does remove some ciphers
  !PSK = don't use pre shared keyring ciphers
  This leaves ~64 ciphers, which are all secure, it could be cut down for
  performance (less information to transfer to the server) but that's not
  all that important.
*/

const char *prefered_ciphers = "HIGH:!aNULL:!kRSA:!SRP:!PSK";
char err_str_buf[128];//only needs to be 120, but why not round to a power of 2

//use a single static context, this should be fine for what we need
static SSL_CTX *ctx;

void print_errors(){
  unsigned long err, line;
  char *file;
  while((err = ERR_get_error_line((const char**)&file, (int*)&line))){
    ERR_error_string_n(err, err_str_buf, 128);
    fprintf(stderr, "%s in %s at line %ld", err_str_buf, file, line);
  }
}
    
  
void init_ssl_lib(){
  SSL_library_init();
  SSL_load_error_strings();
}
SSL_CTX *init_ssl_ctx(){
  //Allow ssl3, tls1, 1.1 or 1.2  
  const SSL_METHOD* method = SSLv23_method();
  if(method == NULL){goto err;}
  ctx = SSL_CTX_new(method);
  if(ctx == NULL){goto err;}
  DEBUG_PRINTF("Initialized ssl context\n");
  //Disable ssl2, ssl3 and forbid compression, ssl2 should already be disabled
  //but there's no harm in making sure
  const long flags = SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3 | SSL_OP_NO_COMPRESSION;
  SSL_CTX_set_options(ctx, flags);//can't fail
  //make sure that we actually verify the certificate
  //this uses the default verification prodecure (thus the NULL)
  SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
  DEBUG_PRINTF("Set ssl context options\n");
  //Load the CA certificates file
  if(SSL_CTX_load_verify_locations(ctx, certificates_location, NULL) == 0){
    goto err;
  }
  DEBUG_PRINTF("Loaded CA certificates\n");
  if(SSL_CTX_set_cipher_list(ctx, prefered_ciphers) == 0){
    goto err;
  }
  DEBUG_PRINTF("Set prefered ciphers\n");
  return ctx;
 err:
  print_errors();//this might not actually print anything
  //There's no function to explicitly free an SSL_METHOD, I'm assuming they're
  //probably static anyway and we just get a pointer.
  SSL_CTX_free(ctx);
  return NULL;
}

/*
  Connect to host on the given port using tls and return the BIO object
  representing the connection.
*/
BIO* connect_tls(const char *host, const char *port){
  init_ssl_lib();
  //the assignment is redundant, but it's weird not having it
  ctx = init_ssl_ctx();
  if(!ctx){return NULL;}
  
  BIO *bio = BIO_new_ssl_connect(ctx);
  if(!bio){goto err;}
  DEBUG_PRINTF("Created ssl_connect bio\n");

  if(BIO_set_conn_hostname(bio, host) == 0){
    goto err;
  }
  if(BIO_set_conn_port(bio, port) == 0){
    goto err;
  }
  DEBUG_PRINTF("Set bio hostname info\n");

  SSL *ssl;
  BIO_get_ssl(bio, &ssl);
  //Handle renegotiation transparently
  SSL_set_mode(ssl, SSL_MODE_AUTO_RETRY);
  //this is to let the server know what name we used to connect to it
  SSL_set_tlsext_host_name(ssl, host);
  DEBUG_PRINTF("Connecting...");
  if(BIO_do_connect(bio) == 0){
    goto err;
  }
  DEBUG_PRINTF("Connected to host\n");
  if(BIO_do_handshake(bio) == 0){
    goto err;
  }
  DEBUG_PRINTF("Completed handshake\n");
  //make sure that the certificate was valid
  if(SSL_get_verify_result(ssl) != X509_V_OK){
    goto err;
  }
  DEBUG_PRINTF("Verified certificate\n");

  return bio;  
 err:
  print_errors();
  BIO_free_all(bio);
  SSL_CTX_free(ctx);
  return NULL;
}
int main(){
  BIO *bio = connect_tls("api.vndb.org","19535");
  char *login_msg =
    "login {\"protocol\" : 1, \"client\" : \"test\", \"clientver\" : 0.1}\x4";
  char responce_buf[128];
  if(!bio){
    exit(EXIT_FAILURE);
  }
  BIO_puts(bio, login_msg);
  DEBUG_PRINTF("Sent message\n");
  int len = BIO_read(bio, responce_buf, 128);
  printf("responce = %.*s\n", len, responce_buf);
  BIO_free_all(bio);
  return 0;
}
