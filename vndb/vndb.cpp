//standard library headers
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <string>
#include <vector>
#include <string_view>
//external library headers
#include <fmt/format.h>
#include <openssl/ssl.h>
#include <sqlite3.h>
//local headers.
#include "util.h"
using format_writer = fmt::BasicMemoryWriter;
namespace json_ns = nlohmann;
//default json type uses a std::map for objects, I'd rather
//use an unordered_map
using json = json_ns::basic_json<std::unordered_map>;
void serialize_json(const json& obj, string_buf& buf){
  using value_t = json::value_t;
  switch(obj.type()){
    case value_t::null:
      return (void)buf.append("null");
    case value_t::boolean:
      return (void)buf.append(obj.get<bool>() ? "true" : "false");
    case value_t::number_integer:
      return (void)buf.append(obj.get<int64_t>());
    case value_t::number_unsigned:
      return (void)buf.append(obj.get<uint64_t>());
    case value_t::number_float:
      return (void)buf.append(obj.get<double>());
    //This just directly copies the string, which could cause
    //issues if there are any weird characters/escape sequences.
    case value_t::string:{
      auto *str = obj.get_ptr<json::string_t*>();
      return (void)buf.append(str.data(), str.size());
    }
    case value_t::object:{
      auto it = obj.begin();
      //could do; char prefix = '{', after loop: prefix = ',';
      buf.append("{\"");
      buf.append(it->first);
      buf.append("\" : ");
      serialize_json(it->second, buf);
      while(++it != obj.end()){
        buf.append(",\"");
        buf.append(it->first);
        buf.append("\" : ");
        serialize_json(it->second, buf);
      }
      buf.append('}');
      return;
    }
    case value_t::array:{
      auto it = obj.begin();
      buf.append('[');
      serialize_json(*it, buf);
      while(++it != obj.end){
        buf.append(',');
        serialize_json(*it, buf);
      }
      buf.append(']');
    }
    default:
      abort();
  }
}
//Constants for the hostname, ports and login command.
static const char* vndb_port_number = "19534";
static const char* vndb_tls_port_number = "19535";
static const char* vndb_hostname = "api.vndb.org";
static const char *login_anon =
  R"(login {"protocol":1, "client":"vndb-cpp", "clientver":0.1})";
static const char *login_fmt_str =
  R"(login {"protocol":1, "client":"vndb-cpp", "clientver":0.1,
            "username":"%s", "password":"%s"})";


static SSL_CTX *vndb_ctx = nullptr;

template<typename ... Ts>
void print_ssl_errors(FILE* out, const char *fmt, Ts&&... Args){
  fprintf(out, fmt, std::forward<Ts>(Args)...);
  ERR_print_errors(out);
}
template<typename ... Ts>
void print_ssl_errors(const char *fmt, Ts&&... Args){
  fprintf(stderr, fmt, std::forward<Ts>(Args)...);
  ERR_print_errors(stderr);
}
SSL_CTX* init_ssl_ctx(){
  SSL_CTX *ctx  = SSL_CTX_new(TLS_client_method());
  if(!ctx){ goto error; }
  if(SSL_CTX_set_min_proto_version(ctx, TLS1_VERSON) <= 0){
    goto error;
  }
  if(SSL_CTX_load_verify_locations(ctx, "/etc/ssl/cert.pem",
                                    "/etc/ssl/certs") <= 0){
    goto error;
  }
  SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
  if(SSL_CTX_set_cipher_list(ctx, "HIGH:!aNULL:!kRSA:!SRP:!PSK") <= 0){
    goto error;
  }
  //if handshake needs to be retried do it transparently.  SSL_CTX_set_mode(ctx, SSL_MODE_AUTO_RETRY);
  return ctx;
 error:
  print_ssl_errors("Error initializing ssl context.\n");
  SSL_CTX_free(ctx);
  return nullptr;
}
bool init_vndb_ssl_ctx(){
  vndb_ctx = init_ssl_ctx();
  return vndb_ctx;
}
struct bio_wrapper {
  BIO *bio = NULL;
  bio_wrapper() = default;
  bio_wrapper(const char* hostname, const char* port, SSL_CTX* ctx){
    bio = BIO_new_ssl_connect(ctx);
    if(!is_encrypted()){
      print_ssl_errors("Can't find SSL pointer\n");
      bio = NULL;
      return;
    }
    BIO_set_conn_hostname(hostname);
    BIO_set_conn_port(port);
  }
  bio_wrapper(const char* hostname, const char* port){
    bio = BIO_new_connect(hostname);
    BIO_set_port(port);
  }
  bool connect(){
    if(BIO_do_connect(bio) <= 0){
      print_ssl_errors("Error connecting to server\n");
      return false;
    }
    if(is_encrypted()){
      if(BIO_do_handshake(bio) <= 0){
        print_ssl_errors("Error establishing SSL connection.\n");
        return false;
      }
    }
    return true;
  }
  void reset(){
    BIO_reset();
  }
  //Simple wrappers around BIO_read and BIO_write, on failure
  //they print an error message.
  ssize_t read(char *buf, size_t len){
    ssize_t nbytes = BIO_read(bio, buf, len);
    if(nbytes <= 0){
      print_ssl_errors("Error reading from bio.\n");
    }
    return nbytes;
  }
  ssize_t write(std::string_view msg){
    ssize_t nbytes = BIO_write(bio, msg.data(), msg.size());
    if(nbytes <= 0){
      print_ssl_errors("Error writing to bio.\n");
    }
    return nbytes;
  }
  //Repeatedly calls read until the delimiter character is read,
  //it is assumed that the delimiter will be at the end of the
  //message, so only the last byte is checked after each read.
  //The data is stored in buf which is resized if needed.
  ssize_t read_delim(util::svector<char> &buf, char delim){
    size_t nbytes = 0;
    size_t bufsz = buf.capacity();
    char *ptr = buf.data();
    do {
      if((bufsz - nbytes) < (bufsz / 2)){
        ptr = (char*)realloc(ptr, bufsz*2);
        bufsz *= 2;
      }
      ssize_t nbytes_read = read(ptr + nbytes, bufsz - nbytes);
      if(nbytes_read <= 0){
        buf.set_contents(ptr, 0, bufsz);
        return nbytes_read;
      }
      nbytes += nbytes_read;
    } while(ptr[nbytes-1] != delim);
    buf.set_contents(ptr, nbytes, bufsz);
    return nbytes;
  }
  bool is_encrypted(){
    SSL *tmp = nullptr;
    BIO_get_ssl(bio, &tmp);
    return (tmp != nullptr);
  }
  operator bool(){ return bio; }
};
/*
  I'm just going to use the hostname/port constants
  rather than passing them as parameters.
*/
struct vndb_connection {
  bio_wrapper bio;
  util::svector<char> buf;
  std::string username;
  std::string passwd;
  vndb_connection(std::string_view username, std::string_view passwd)
    : bio(vndb_hostname, vndb_tls_port_number, vndb_ctx),
      username(username), passwd(passwd) {
    buf.reserve(4096);
  }

};


/*
  connection struct and related functions.

  Could probably use some re-organization.
*/
struct connection {
  SSL *ssl; //can be NULL.
  BIO *bio;
  char *buf;
  size_t bufsz;
  char *username;
  char *passwd;
  //Allocated with scheme_malloc_immobile_box
  Scheme_Object **err_port;
};
int ssl_print_callback(const char *str, size_t len, void *out_port){
  scheme_write_byte_string(str, len, out_port);
  return (int)len;
}
//print to err_port if non-null, stderr otherwise.
void connection_print_error(connection *conn, const char *errstr){
  if(conn->err_port){
    scheme_write_byte_string(errstr, strlen(errstr), *conn->err_port);
  } else {
    fputs(errstr, stderr);
  }
}
//Print to err_port if non-null, don't print to stderr if null.
void connection_print_info(connection *conn, const char *str){
  if(conn->err_port){
    scheme_write_byte_string(str, strlen(str), *conn->err_port);
  }
}
//Print errstr and any ssl errors with same rules as connection_print_error.
void connection_print_ssl_errors(connection *conn, const char *errstr){
  if(conn->err_port){
    scheme_write_byte_string(errstr, strlen(errstr), *conn->err_port);
    ERR_print_errors_cb(ssl_print_callback, *conn->err_port);
  } else {
    fputs(errstr, stderr);
    ERR_print_errors(stderr);
  }
}
void free_connection(connection *conn){
  SSL_free(conn->ssl);
  BIO_free_all(conn->bio);
  free(conn->buf);
  free(conn->username);
  free(conn->passwd);
  //Not sure how this behaves on NULL so just check to be safe
  if(conn->err_port){
    scheme_free_immobile_box(conn->err_port);
  }
  //Don't free conn, it's allocated via gc.

  //memeset to 0, this might get called more that once on the same
  //connection, due to an explicit call + the finalizer.
  memset(conn, '\0', sizeof(conn));
}
int connection_connect(connection *conn){
  //Make sure we're disconnected, in case we're trying to reconnect.
  BIO_reset(conn->bio);
  //establish connection
  if(BIO_do_connect(sbio) <= 0){
    connection_print_ssl_errors("Error connecting to server\n");
    return 0;
  }
  if(conn->ssl){
    if(BIO_do_handshake(sbio) <= 0){
      connection_print_ssl_errors("Error establishing SSL connection\n");
      return 0;
    }
  }
  return 1;
}
//Login using username & passwd from conn.
//1 on success, 0 on connection/write failure, -1 on error from server.
int connection_do_login(connection *conn){
  char *ptr = stpcpy(conn->buf, login_prefix);
  if(conn->username){
    if(!conn->ssl){
      connection_print_error(
        "Warning attempt made to login on unencrypted connection, aborting.");
      return 0;
    }
    ptr += snprintf(ptr, conn->bufsz - sizeof(login_prefix),
p                    ", \"username\":\"%s\", \"password\", \"%s\"}",
                    conn->username, conn->passwd);
  } else {
    *ptr++ = '}';
  }
  if(BIO_write(conn->bio, conn->buf, ptr - conn->buf) <= 0){
    connection_print_ssl_errors(conn, "Failed to login to server\n");
    return 0;
  }
  return (memcmp(conn->buf, "ok\x4", 3) == 0 ? 1 : -1);
}
int connection_set_username_passwd(connection *conn,
                                    const char *username, ssize_t username_len,
                                    const char *passwd, ssize_t passwd_len){
  if(username_len < 0){
    conn->username = strdup(username);
  } else {
    conn->username = memdup(username, username_len);
  }
  if(passwd_len < 0){
    conn->passwd = strdup(passwd);
  } else {
    conn->passwd = memdup(passwd, passwd_len);
  }
  //0 if either of these are null, 1 otherwise.
  return (conn->username && conn->passwd);
}
//Set username/passwd of conn then login using them.
int connection_login(connection *conn,
                     const char *username, ssize_t username_len,
                     const char *passwd, ssize_t passwd_len){
  if(connection_set_username_passwd(conn, username, username_len,
                                    passwd, passwd_len) <= 0){
    return 1;
  }
  return connection_do_login(conn);
}
//Returns true if we successfully login
int connection_relogin(connection *conn){
  //re-connect
  int connected = connection_connect(conn);
  return (connected <= 0 ? connected :
          //relogin
          connection_do_login(conn));
}

connection* make_empty_connection(Scheme_Object *err_port){
  size_t bufsz = 4096;
  char *buf = calloc(bufsz,1);
  connection* ret = NULL;
  if(!buf){
    return NULL;
  }
  ret = scheme_malloc_atomic(sizeof(ssl_connection));
  if(!ret){
    free(buf);
    return NULL;
  }
  //initialize all fields to NULL
  memset(ret, '\0', sizeof(connection));

  ret->out_port = scheme_malloc_immobile_box(err_port);
  if(!ret->out_port){
    free(buf);
    return NULL;
  }
  scheme_register_finalizer(ret, free_connection, NULL, NULL, NULL);
  return ret;
}
connection* make_connection(const char *hostname,
                            const char *port,
                            Scheme_Object *err_port,
                            SSL_CTX *ctx){
  connection *ret = make_empty_connection(err_port);
  if(!ret){
    return NULL;
  }
  ret->bio = BIO_new_connect(hostname);
  BIO_set_conn_port(ret->bio, port);
  if(ctx){
    BIO *sbio = BIO_new_ssl(ctx, 0);
    ret->bio = BIO_push(sbio, ret->bio);
    BIO_get_ssl(sbio, &(ret->ssl));
    if(!ret->ssl){
      connection_print_ssl_errors(ret, "Failed to get ssl pointer\n");
      free_connection(ret);
      return NULL;
    }
  }
  if(connection_connect(ret) <= 0){
    free_connection(ret);
    return NULL;
  }
  return ret;
}
int connection_write(connection *conn, const char *str, size_t len){
  if(BIO_write(conn->bio, str, len) <= 0){
    //On failure to write try to reconnect to the server.
    relogin(conn);
    if(BIO_write(conn->bio, str, len) <= 0){
      connection_print_ssl_errors(conn, "Failed to write to server\n");
      return 0;
    }
  }
  return 1;
}
/*
  Send / Recv functions exposed to racket.
*/
//Takes a connection and a byte string / list of byte strings.
//If given a bytestring just appends '\x04' and writes that to the server,
//otherwise concatenates the bytestrings, seperating them with spaces,
//appends '\x04' and sends the result to the server,
//also prints the command to the connections error_port.
Scheme_Object *scheme_vndb_send(int argc, Scheme_Object **argv){
  connection *conn = SCHEME_CPTR_VAL(argv[0]);
  char *buf = conn->buf;
  size_t bufsz = conn->bufsz;
  size_t offset = 0;
  Scheme_Object* ls = argv[1];
  if(SCHEME_BYTE_STRINGP(ls)){
    //Cheat and use the space for the null terminator to store
    //the end of message character.
    char *str = SCHEME_BYTE_STR_VAL(ls);
    size_t len = SCHEME_BYTE_STRLEN_VAL(ls);
    str[len] = 0x4;
    Scheme_Object *ret = (connection_write(conn, buf, bufsz) > 0
                          ? scheme_true : scheme_false);
    str[len] = '\0';
    return ret;
  }
  //Copy message into buffer, use the buffer in conn if possible
  //allocate memory if necesary.
  while(!SCHEME_NULLP(ls)){
    Scheme_Object *bs = SCHEME_CAR(ls);
    ls = SCHEME_CDR(ls);
    char *str = SCHEME_BYTE_STR_VAL(bs);
    size_t len = SCHEME_BYTE_STRLEN_VAL(bs);
    if(len + offset >= bufsz){
      if(buf == conn->buf){
        buf = malloc(bufsz*2);
        memcpy(buf, conn->buf, buffsz);
      } else {
        buf = realloc(buf, bufsz*2);
      }
      bufsz *= 2;
    }
    memcpy(buf + offset, str, len);
    offset += len;
    buf[offset] = ' ';
  }
  //set end char to newline for printing
  buf[offset] = '\n';
  connection_print_info(conn, buf);
  //set end char to 0x4 to send to server.
  buf[offset] = 0x4;
  Scheme_Object *ret = (connection_write(conn, buf, bufsz) > 0
                        ? scheme_true : scheme_false);
  if(buf != conn->buf){
    free(buf);
  }
  return ret;
}
//Takes a connection, reads from server until \x4 is read
//returns response as a byte string, or false on error.
//The returned string is only guaranteed to be valid until
//the next call to scheme_vndb_send.
Scheme_Object *scheme_vndb_recv(int argc, Scheme_Object **argv){
  connection *conn = SCHEME_CPTR_VAL(argv[0]);
  //No need to leave space for a null terminator, we can convert
  //the trailing \x4 into null.
  int nbytes = BIO_read(conn->bio, conn->buf, conn->bufsz);
  if(nbytes <= 0){
    connection_print_ssl_errors(conn, "Error reading from server\n");
    return scheme_false;
  }
  //We may read < conn->bufsz bytes, but not read the entire message, so
  //we need to check for the delimiter.
  //If we can fit the entire message into the buffer return a byte-string
  //which uses the buffer.
  if(conn->buf[nbytes-1] == 0x4){
    conn->buf[nbytes-1] = '\0';
    return scheme_make_sized_byte_string(conn->buf, nbytes, 0);
  }
  //We could attempt to use the gc to alllocate the buffer, but
  //it's not worth the trouble
  size_t bufsz = 2*conn->bufsz;
  char *buf = malloc(bufsz);
  memcpy(buf, conn->buf, nbytes);
  while(1){
    int nbytes2 = BIO_read(conn->bio, buf + nbytes, bufsz - nbytes);
    if(nbytes2 <= 0){
      goto error;
    }
    nbytes += nbytes2;
    if(buf[nbytes-1] == 0x4){
      break;
    }
    //reallocate when we've used up more than half the buffer.
    if(nbytes > (bufsz/2)){
      bufsz *= 2;
      buf = realloc(buf, bufsz);
    }
  }
  //make a copy of buf.
  Scheme_Object *ret = scheme_make_sized_byte_string(buf, nbytes, 1);
  free(buf);
  return ret;
 error:
  free(buf);
  connection_print_ssl_errors(conn, "Error reading from server\n");
  return scheme_false;
}
//(define (vndb-connect (username-passwd NULL)
//                      (output-port (current-error-port))
//                      (hostname vndb-hostname) (port vndb-tls-port)))
//Typechecking needs to be done in scheme. port needs to be a string.
//login is only done if username-passwd is non-null.
Scheme_Object *scheme_vndb_connect(int argc, Scheme_Object **argv){
  const char *hostname = vndb_hostname;
  //use ssl/tls by default
  const char *port = vndb_tls_port_number;
  const char *username = NULL;
  const char *passwd = NULL;
  Scheme_Object *out = NULL;
  SSL_CTX *ctx = vndb_ctx;
  if(argc >= 1){
    Scheme_Object *username_passwd = argv[0];
    //TODO: use the bytestring length to avoid calling strlen later.
    if(!SCHEME_NULLP(username_passwd)){
      username = SCHEME_BYTE_STR_VAL(SCHEME_CAR(username_passwd));
      passwd = SCHEME_BYTE_STR_VAL(SCHEME_CDR(username_passwd));
    }
  }
  if(argc >= 2){
    out = argv[1];
  } else {
    out = scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_PORT);
  }
  if(argc > 2){
    hostname = SCHEME_BYTE_STR_VAL(argv[2]);
    if(argc > 3){
      port = SCHEME_BYTE_STR_VAL(argv[3]);
      //I'm not sure about this, but it's currently the
      //only way to make an unencrypted connection so I'm
      //leaving it for now.
      if(strcmp(port, vndb_tls_port_number) != 0){
        ctx = NULL;
      }
    }
  }
  connection* conn = make_connection(hostname, port, out, ctx);
  if(conn == NULL){
    return scheme_false;
  }
  if(username && passwd){
    int err = connection_do_login(conn, username, -1, passwd, -1);
    if(err <= 0){
      free_connection(conn);
      return scheme_false;
    }
  }
  Scheme_Object *tag = scheme_intern_symbol("vndb-connection");
  Scheme_Object *ret = scheme_make_cpointer(conn, tag);
  return ret;
}
Scheme_Object* scheme_set_vndb_username(int argc, Scheme_Object **argv){
  connection *conn = SCHEME_CPTR_VAL(argv[0]);

  const char *useranme = SCHEME_BYTE_STR_VAL(argv[1]);
  size_t username_len = SCHEME_BYTE_STRLEN_VAL(argv[1]);

  const char *passwd = SCHEME_BYTE_STR_VAL(argv[2]);
  size_t passwd_len = SCHEME_BYTE_STRLEN_VAL(argv[2]);

  int err = connection_set_username_passwd(conn, username, username_len,
                                           passwd, passwd_len);
  return (err ? scheme_false : scheme_true);
}
