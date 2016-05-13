#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*
  Openssl utility functions
*/
#include <openssl/bio.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
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
  //Disable ssl2, ssl3 and forbid compression, ssl2 should already be disabled
  //but there's no harm in making sure
  const long flags = SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3 | SSL_OP_NO_COMPRESSION;
  SSL_CTX_set_options(ctx, flags);//can't fail
  //make sure that we actually verify the certificate
  //this uses the default verification prodecure (thus the NULL)
  SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
  //Load the CA certificates file
  if(SSL_CTX_load_verify_locations(ctx, certificates_location, NULL) == 0){
    goto err;
  }
  if(SSL_CTX_set_cipher_list(ctx, prefered_ciphers) == 0){
    goto err;
  }
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

  if(BIO_set_conn_hostname(bio, host) == 0){
    goto err;
  }
  if(BIO_set_conn_port(bio, port) == 0){
    goto err;
  }

  SSL *ssl;
  BIO_get_ssl(bio, &ssl);
  //Handle renegotiation transparently
  SSL_set_mode(ssl, SSL_MODE_AUTO_RETRY);
  //this is to let the server know what name we used to connect to it
  SSL_set_tlsext_host_name(ssl, host);
  if(BIO_do_connect(bio) == 0){
    goto err;
  }
  if(BIO_do_handshake(bio) == 0){
    goto err;
  }
  //make sure that the certificate was valid
  if(SSL_get_verify_result(ssl) != X509_V_OK){
    goto err;
  }

  return bio;
 err:
  print_errors();
  BIO_free_all(bio);
  SSL_CTX_free(ctx);
  return NULL;
}  
//Scheme stuff
#include <guile/2.0/libguile.h>
//Wrapper around code to raise a scheme error, handles conversion of
//c strings to symbols/scm strings
#define scm_raise_error(key, msg)                               \
  ({SCM scm_key = scm_from_utf8_symbol(key);                    \
    SCM scm_msg = scm_from_utf8_string(msg);                    \
    scm_error_scm(scm_key, scm_from_utf8_string(__func__),      \
                  scm_msg, SCM_BOOL_F, SCM_BOOL_F);});
//We make a bio scheme type to avoid worrying about the bio closing
//the socket or anything like that, just make sure to hang on to the
//bio object as long as the connection is active. We call it tls rather
//than bio, since it's not really used for io or anything.
static scm_t_bits tls_tag;
size_t scm_tls_free(SCM tls_smob){
  scm_assert_smob_type(tls_tag, tls_smob);
  BIO *bio = (BIO*)SCM_SMOB_DATA(tls_smob);
  BIO_free_all(bio);
  return 0;
}
SCM scm_tls_get_fd(SCM tls_smob){
  scm_assert_smob_type(tls_tag, tls_smob);
  BIO *bio = (BIO*)SCM_SMOB_DATA(tls_smob);
  int fd = BIO_get_fd(bio, NULL);
  SCM port = scm_fdopen(scm_from_int(fd),
                        scm_from_utf8_string("r+"));
  scm_set_port_encoding_x(port, scm_from_utf8_string("UTF-8"));
  return port;
}
SCM scm_tls_get_cipher_info(SCM tls_smob){
  scm_assert_smob_type(tls_tag, tls_smob);
  BIO *bio = (BIO*)SCM_SMOB_DATA(tls_smob);
  SSL *ssl;
  BIO_get_ssl(bio, &ssl);

  //I'm not sure if scheme copies c strings or not, so make this static
  //so it stays valid regardless.
  static char cipher_buf[128];
  SSL_CIPHER *cipher = SSL_get_current_cipher(ssl);
  SSL_CIPHER_description(cipher, cipher_buf, 128);
  return scm_from_utf8_string(cipher_buf);
}
SCM scm_tls_send(SCM tls_smob, SCM msg){
  scm_assert_smob_type(tls_tag, tls_smob);
  BIO *bio = (BIO*)SCM_SMOB_DATA(tls_smob);
  size_t msglen = scm_c_string_length(msg);
  char *buf = alloca(msglen+1);
  size_t buflen = scm_to_locale_stringbuf(msg, buf, msglen);
  buf[buflen] = '\0';
  return scm_from_int(BIO_puts(bio, buf));
}
SCM scm_tls_recv_x(SCM tls_smob, SCM buf){
  scm_assert_smob_type(tls_tag, tls_smob);
  BIO *bio = (BIO*)SCM_SMOB_DATA(tls_smob);
  size_t buflen = scm_c_bytevector_length(buf);
  char *bufptr = (char*)SCM_BYTEVECTOR_CONTENTS(buf);
  return scm_from_int(BIO_read(bio, bufptr, buflen));
}
  


//host is a string, port can be either a string or an integer
//This just returns #f on error, a better version would raise errors

//get scm symbols: scm_from_utf8_symbol(name)
SCM scm_connect_tls(SCM host, SCM port){
  char hostbuf[256], portbuf[16];
  //Assume the current locale is utf8, as the only function that lets
  //use use our own buffers implicitly uses the current locale
  if(!scm_is_string(host)){
    scm_raise_error("wrong-type-arg", "expected string in position 1");
  } else {
    size_t len = scm_to_locale_stringbuf(host, hostbuf, 256);
    if(len >= 256){
      scm_raise_error("too-long", "hostname too long");
    } else {
      hostbuf[len] = '\0';
    }
  }
  if(scm_is_string(port)){
    //make sure port looks like a number
    if(scm_is_false(scm_string_to_number(port, scm_from_int(10)))){
      scm_raise_error("wrong-type-arg",
                      "expected number or number as string in position 2");
    }
    size_t len = scm_to_locale_stringbuf(port, portbuf, 32);
    if(len >= 16){
      scm_raise_error("out-of-range", "Maximum port number is 65535");
    } else {
      portbuf[len] = '\0';
    }
  } else if(scm_is_integer(port)){
    uint16_t portno = scm_to_uint16(port);
    snprintf(portbuf, 16, "%d", portno);
  } else {
    scm_raise_error("wrong-type-arg",
                    "expected number or number as string in position 2");
  }
  BIO *bio = connect_tls(hostbuf, portbuf);
  if(!bio){
    scm_raise_error("system-error", "Failed to make tls connection");
  }
  return scm_new_smob(tls_tag, (scm_t_bits)bio);
}

SCM scm_gunzip_buf(SCM scm_buf, SCM scm_outlen){
  //this should typecheck buf for us
  size_t buflen = scm_C_bytevector_length(buf);
  uint8_t *buf = (uint8_t*)SCM_BYTEVECTOR_CONTENTS(buf);
  size_t outlen = scm_to_size_t(scm_outlen);
  uint8_t *out = scm_gc_malloc_pointerless(outlen);
  
  z_stream stream = {.next_in = buf, .avail_in = buflen,
                     .next_out = out, .avail_out = outlen,
                     .zalloc = NULL, .zfree = NULL, .opaque = NULL};
  inflateInit(&stream);
  int status = inflate(&stream, Z_FINISH);
  if(status != Z_STREAM_END){ //the output buffer was too small
    //Do something useful here, for now this just makes sure that
    //we don't cause any errors
    free(out);
    return SCM_BOOL_F;
  }
  
  SCM bv = scm_pointer_to_bytevector(out, stream.total_out);
  return bv;
}

void init_openssl(){
  tls_tag = scm_make_smob_type("tls", sizeof(BIO*));
  scm_set_smob_free(tls_tag, scm_tls_free);
  scm_c_define_gsubr("tls-connect", 2, 0, 0, scm_connect_tls);
  scm_c_define_gsubr("tls-get-fd", 1, 0, 0, scm_tls_get_fd);
  scm_c_define_gsubr("tls-get-cipher-info", 1, 0, 0, scm_tls_get_cipher_info);
  scm_c_define_gsubr("tls-send", 2, 0, 0, scm_tls_send);
  scm_c_define_gsubr("tls-recv!", 2, 0, 0, scm_tls_recv_x);
  //from zlib
  scm_c_define_gsubr("gunzip-bytevector", 2, 0, 0, scm_gunzip_buf);
}
