#include "vndb.h"
//static const char *login_anon =
//  R"(login {"protocol":1, "client":"vndb-cpp", "clientver":0.1})";
static const char *login_prefix =
  R"(login {"protocol":1, "client":"vndb-cpp", "clientver":0.1)";

//Not that complicated but makeing this a function makes it much
//clearer what's going on
bool has_prefix(util::string_view sv, util::string_view prefix){
  return !strncmp(sv.data(), prefix.data(), prefix.size());
  //return sv.substr(0, prefix.size()) == prefix;
}

template<typename ... Ts>
void print_ssl_errors(FILE* out, const char *fmt, Ts&&... Args){
  fprintf(out, fmt, std::forward<Ts>(Args)...);
  ERR_print_errors_fp(out);
}
template<typename ... Ts>
void print_ssl_errors(const char *fmt, Ts&&... Args){
  fprintf(stderr, fmt, std::forward<Ts>(Args)...);
  ERR_print_errors_fp(stderr);
}
static SSL_CTX* init_ssl_ctx(){
  SSL_CTX *ctx  = SSL_CTX_new(TLS_client_method());
  if(!ctx){ goto error; }
  if(SSL_CTX_set_min_proto_version(ctx, TLS1_VERSION) <= 0){
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
void free_vndb_ssl_ctx(){
  SSL_CTX_free(vndb_ctx);
}

BIO_wrapper::BIO_wrapper(const char* hostname, const char* port, SSL_CTX* ctx){
  this->bio = BIO_new_ssl_connect(ctx);
  SSL *tmp = nullptr;
  BIO_get_ssl(this->bio, &tmp);
  if(tmp == nullptr){
    print_ssl_errors("Can't find SSL pointer\n");
    this->bio = NULL;
    return;
  }
  BIO_set_conn_hostname(this->bio, hostname);
  BIO_set_conn_port(this->bio, port);
}
BIO_wrapper::~BIO_wrapper(){
  BIO_free_all(this->bio);
}
bool BIO_wrapper::is_encrypted(){
  SSL *tmp = nullptr;
  BIO_get_ssl(this->bio, &tmp);
  return (tmp != NULL);
}
bool BIO_wrapper::connect(){
  if(BIO_do_connect(this->bio) <= 0){
    print_ssl_errors("Error connecting to server\n");
    return false;
  }
  //  if(is_encrypted()){
  if(BIO_do_handshake(this->bio) <= 0){
    print_ssl_errors("Error establishing SSL connection.\n");
    return false;
  }
  //  }
  return true;
}
void BIO_wrapper::reset(){
  BIO_reset(this->bio);
}
//Simple wrappers around BIO_read and BIO_write, on failure
//they print an error message.
ssize_t BIO_wrapper::read(char *buf, size_t len){
  ssize_t nbytes = BIO_read(this->bio, buf, len);
  if(nbytes <= 0){
    print_ssl_errors("Error reading from bio.\n");
  }
  return nbytes;
}
ssize_t BIO_wrapper::write(std::string_view msg){
  ssize_t nbytes = BIO_write(this->bio, msg.data(), msg.size());
  if(nbytes <= 0){
    print_ssl_errors("Error writing to bio.\n");
  }
  return nbytes;
}
//Repeatedly calls read until the delimiter character is read,
//it is assumed that the delimiter will be at the end of the
//message, so only the last byte is checked after each read.
//The data is stored in buf which is resized if needed.
ssize_t BIO_wrapper::read_delim(util::svector<char> &buf, char delim){
  size_t nbytes = 0;
  size_t bufsz = buf.capacity();
  char *ptr = buf.data();
  do {
    if((bufsz - nbytes) < (bufsz / 2)){
      ptr = (char*)realloc(ptr, bufsz*2);
      bufsz *= 2;
    }
    ssize_t nbytes_read = this->read(ptr + nbytes, bufsz - nbytes);
    if(nbytes_read <= 0){
      buf.set_contents(ptr, 0, bufsz);
      return nbytes_read;
    }
    nbytes += nbytes_read;
  } while(ptr[nbytes-1] != delim);
  buf.set_contents(ptr, nbytes, bufsz);
  return nbytes;
}
vndb_connection::vndb_connection(std::string_view username,
                                 std::string_view passwd)
  : bio(vndb_hostname, vndb_tls_port_number, vndb_ctx),
    buf(4096),
    username(username), passwd(passwd) {
  if(this->bio.connect()){
    this->logged_in = this->login();
  }
}
vndb_connection::vndb_connection() : vndb_connection("","") {}
//Login using username & passwd from conn.
//true on success, false on connection/write failure.
//sets error to true iff connection succeeded but login failed.
//sets logged_in to true on success.
bool vndb_connection::login(){
  this->buf.clear();
  this->buf.append(login_prefix, constexpr_strlen(login_prefix));
  if(!this->username.empty()){
    // if(!this->is_encrypted()){
    //   fprintf(stderr,
    //     "Warning attempt made to login on unencrypted thisection, aborting.");
    //   return false;
    // }
    this->buf.append_formatted(", \"username\":\"%s\", \"password\", \"%s\"}\x4",
                           this->username.c_str(), this->passwd.c_str());
  } else {
    this->buf.append("}\x4", constexpr_strlen("}\x4"));
  }
  if(this->write_buf() <= 0){
    //bio method will print the errors
    return false;
  }
  this->read();
  if(memcmp(this->buf.data(), "ok", 2) == 0){
    return true;
  } else {
    this->error = true;
    return false;
  }

}
int vndb_connection::write(const char *str, size_t len){
//  DEBUG_PRINTF("Writing |%.*s|\n", len, str);
//  if(str[len-1] != this->EOT){    
//    DEBUG_PRINTF("Error missing end of message character, got '%d'\n", str[len-1]);
//    return -1;
//  }
  int nbytes = BIO_write(this->bio, str, len);
  if(nbytes <= 0){
    //On failure to write try to reconnect to the server.
    if(this->relogin()){
      nbytes = BIO_write(this->bio, str, len);
      if(nbytes <= 0){
        this->logged_in = false;
      }
    } else {
      return -1;
    }
  }
  return nbytes;
}
int vndb_connection::write_buf(){
  util::string_view sv = this->buf.to_string_view();
  return this->write(sv.data(), sv.size());
}
int vndb_connection::read(){
  this->buf.clear();
  int nbytes = this->bio.read_delim(this->buf.buf, this->EOT);
  if(nbytes > 0){
    this->buf.buf[nbytes-1] = '\0';
    this->buf.buf.pop_back();
  }
//  DEBUG_PRINTF("Read |%s|\n", this->buf.c_str());
  return nbytes;
};
json vndb_connection::get_error(){  
  util::string_view response = this->buf.to_string_view();
//  DEBUG_PRINTF("Recieved error %s\n", response.data());
  assert(has_prefix(response, "error"));
  this->error = true;
  return json::parse(response.substr(constexpr_strlen("error")));
}

json vndb_connection::send_get_command_once(int page_no,
                                            util::string_view sort_by){
  static const std::string_view options =
    R"( {"sort" : "%s", "results" : 25, "page" : %d})"sv;

  this->buf.append_formatted(options.data(), sort_by.data(), page_no);
//  DEBUG_PRINTF("Sending command %s\n", this->buf.c_str());
  this->buf.append(this->EOT);
  int nbytes_written = this->write_buf();
  if(nbytes_written <= 0){//assume this means we couldn't connect to the server
    this->logged_in = false;
    return json_null;
  }
  int nbytes_read = this->read();
  if(nbytes_read <= 0){//Shouldn't happen very often.
    this->logged_in = false;
    return json_null;
  }
  //maybe not the most elegant way to do this but it should work.
  util::string_view response = this->buf.to_string_view();
  if(has_prefix(response, "error")){
    return this->get_error();
  }
  if(!has_prefix(response, "results")){
    fprintf(stderr, "Got unexpected result from server: %s\n",
            this->buf.c_str());
    //not strictly true but we need to indicate an error somehow
    this->error = true;
    return json_null;
  }
  //skip over literal 'results' and parse json.
  return json::parse(response.substr(constexpr_strlen("results")));
}


int vndb_connection::send_get_command(std::vector<json>& results,
                                      util::string_view sort_by){
  std::string cmd = this->buf.to_std_string();//copy command
  results.clear();
  int page_no = 1;
  bool more = true;
  while(more){
    //Should proably reuse this storage.
    json result = this->send_get_command_once(page_no++, sort_by);
    if(this->error){
      results.push_back(result);
      return -1;
    }
    results.reserve(results.size() + result["num"].get<int>());
    //definately not the fastest way to do this
    json::array_t& items = result["items"].get_ref<json::array_t>();
    for(size_t i = 0; i < items.size(); i++){
      results.emplace_back(std::move(items[i]));
    }
    more = result["more"].get<bool>();
    this->buf.clear();
    this->buf.append(cmd);
  }
  return results.size();
}

int vndb_connection::get_vns(int start, int stop, std::vector<json>& vec){
  static const std::string_view command =
    R"||(get vn basic,details,anime,relations,tags,stats,screens,staff
       (id >= %d and id <= %d))||"sv;
  this->buf.clear();
  this->buf.append_formatted(command.data(), start, stop, 0);
  //TODO: optimize for the case where stop-start <= 25
  return this->send_get_command(vec);
}
json vndb_connection::dbstats(){
  this->buf.clear();
  this->buf.append("dbstats\x4", constexpr_strlen("dbstats\x4"));
  this->write_buf();
  this->read();
  util::string_view response = this->buf.to_string_view();
  if(has_prefix(response, "error")){
    return this->get_error();
  }
  if(!has_prefix(response, "dbstats")){
    fprintf(stderr, "Got unexpected result from server: %s\n",
            this->buf.c_str());
    //not strictly true but we need to indicate an error somehow
    this->error = true;
    return json_null;
  }
  return json::parse(response.substr(constexpr_strlen("dbstats")));
}
