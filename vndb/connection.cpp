#include "vndb.h"
#include <chrono>
//Even though we include this we don't need to link with pthreads since
//we don't actually use any thread functions.
#include <thread>
namespace util {
void sleep(double seconds){
  std::chrono::duration<double> dur(seconds);
  std::this_thread::sleep_for(dur);
}
}
SSL_CTX* vndb_ctx = nullptr;
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
//bool SSL_CTX_wrapper::init(){
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
  vndb_log->print(util::log_level::info,"Initializing SSL context.\n");
  vndb_ctx = init_ssl_ctx();
  vndb_log->printf(util::log_level::info,"SSL context = %p.\n", vndb_ctx);
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
//repeaditly call read until exactly n bytes have been read
ssize_t BIO_wrapper::read_n(char *buf, size_t len){
  size_t nbytes = 0;
  while(nbytes < len){
    ssize_t nbytes_read = BIO_read(this->bio, buf + nbytes, len - nbytes);
    if(nbytes_read <= 0){
      print_ssl_errors("Error reading from bio.\n");
      return nbytes_read;
    }
    nbytes += nbytes_read;
  }
  return nbytes;
}
//Internal function to read from a bio into a buffer, if the buffer
//is more than half full it is resized before reading data, the length
//of the buffer is increased by the number of bytes read and the
//result of the read call is returned..
static ssize_t buf_read(BIO_wrapper &bio, char *& buf,
                        size_t& buflen, size_t& bufsz){
  if((bufsz - buflen) < (bufsz / 2)){
    buf = (char*)realloc(buf, bufsz*2);
    bufsz *= 2;
  }
  //leave space in buf for a null terminator, its kinda lame, but I need to.
  ssize_t nbytes =  bio.read(buf + buflen, bufsz - buflen - 1);
  //Assume that if nbytes < 0 that the caller doesn't really care
  //about buflen anymore so it doesn't matter if we make it smaller.
  buflen += nbytes;
  return nbytes;
}
ssize_t BIO_wrapper::read_all(util::svector<char> &buf){
  static constexpr size_t min_bufsz = 1024;
  size_t bufsz = buf.capacity();
  if(bufsz < min_bufsz){
    buf.reserve(min_bufsz);
    bufsz = min_bufsz;
  }
  char *ptr = buf.data();
  ssize_t nbytes = 0, nbytes_total = 0;
  while((nbytes = BIO_read(this->bio, ptr + nbytes, bufsz - nbytes)) > 0){
    nbytes_total += nbytes;
    if((bufsz - nbytes) < (bufsz / 2)){
      ptr = (char*)realloc(ptr, bufsz*2);
      bufsz *= 2;
    }
  }
  if(nbytes < 0){
    print_ssl_errors("Error reading from bio.\n");
    buf.set_contents(ptr, 0, bufsz);
    return nbytes;
  } else {
    buf.set_contents(ptr, nbytes_total, bufsz);
    return nbytes_total;
  }
}
//Repeatedly calls read until the delimiter character is read,
//it is assumed that the delimiter will be at the end of the
//message, so only the last byte is checked after each read.
//The data is stored in buf which is resized if needed.
ssize_t BIO_wrapper::read_delim(util::svector<char> &buf, char delim){
  size_t buflen = 0;
  size_t bufsz = buf.capacity();
  char *ptr = buf.data();
  do {
    ssize_t nbytes = buf_read(*this, ptr, buflen, bufsz);
    if(nbytes <= 0){
      buf.set_contents(ptr, 0, bufsz);
      return nbytes;
    }
  } while(ptr[buflen-1] != delim);
  buf.set_contents(ptr, buflen, bufsz);
  return buflen;
}
//Same as above but using a multicharacter string as the delimiter.
ssize_t BIO_wrapper::read_delim(util::svector<char> &buf, const char* delim){
  size_t buflen = 0;
  size_t bufsz = buf.capacity();
  char *ptr = buf.data();
  size_t delim_len = strlen(delim);
  do {
    ssize_t nbytes = buf_read(*this, ptr, buflen, bufsz);
    if(nbytes <= 0){
      buf.set_contents(ptr, 0, bufsz);
      return nbytes;
    }
  } while(memcmp(ptr + (buflen-delim_len), delim, delim_len) != 0);
  buf.set_contents(ptr, buflen, bufsz);
  return buflen;
}
#if 0
int http_connection::http_get(std::string_view uri, util::svector<char>& buf){
  static constexpr std::string_view success_response = "HTTP/1.1 200 OK\r\n"sv;
  static constexpr std::string_view length_field = "Content-Length: "sv;
  buf.clear();
  buf.reserve(1024);
  //Due to how I've written things I can't read until a delmiter unless
  //its at the end of the message, so I make a head request to get the
  //header length (and content length) so I can skip over it
  int nbytes = snprintf(buf.data(), buf.capacity(),
                        "HEAD %s HTTP/1.1\r\nHost: %s\r\n\r\n",
                        uri.data(), hostname.data());
  DEBUG_PRINTF("Sending http HEAD request:\n%.*s\n", nbytes, buf.data());
  if(bio.write(std::string_view(buf.data(), nbytes)) < 0){
    return -1;
  }
  DEBUG_PRINTF("Reading response.\n");
  int header_length = (int)bio.read_delim(buf, "\r\n\r\n");
  if(header_length <= 0){
    //Return -1 for 0 bytes read.
    return (header_length ? -1 : header_length);
  }
  DEBUG_PRINTF("HEAD response:\n%.*s\n", header_length, buf.data());
  //Check the status line, I'm assuming if that's ok then the rest of the
  //header is fine too.
  if(memcmp(buf.data(), success_response.data(),
            std::min(buf.size(), success_response.size()) != 0)){
    char *status_code_offset = (char*)memchr(buf.data(), ' ', buf.size());
    if(status_code_offset){
      int status_code = strtol(status_code_offset, &status_code_offset, 10);
      if(status_code > 0){
        return status_code;
      }
    }
    fprintf(stderr, "Error malformed http status line %.*s.\n",
            (int)buf.size(), buf.data());
    return -1;
  }
  buf[header_length-1] = '\0';//avoid possible buffer overrun in strstr.
  char *length_offset = strstr(buf.data(), length_field.data());
  if(!length_offset){
    fprintf(stderr, "Error could not find Content-Length in header.\n");
    return -1;
  }
  int content_length = strtol(length_offset + length_field.size(),
                              &length_offset, 0);
  if(content_length == 0){
    fprintf(stderr, "Error parsing content length, or content length was 0.\n");
    return -1;
  }
  buf.clear();
  buf.reserve(content_length + 1);//leave space for null terminator, because why not.
  nbytes = snprintf(buf.data(), buf.capacity(),
                    "GET %s HTTP/1.1\r\nHost: %s\r\n\r\n",
                    uri.data(), hostname.data());
  DEBUG_PRINTF("Sending http GET request:\n%.*s\n", nbytes, buf.data());
  if(bio.write(std::string_view(buf.data(), nbytes)) <= 0){
    return -1;
  }
  nbytes = bio.read_n(buf.data(), header_length);
  if(nbytes <= 0){
    return (nbytes ? -1 : nbytes);
  }
  assert(nbytes == header_length);
  nbytes = bio.read_n(buf.data(), content_length);
  if(nbytes <= 0){
    return (nbytes ? -1 : nbytes);
  }
  assert(nbytes == content_length);
  buf.set_length(nbytes);
  return 0;
}
#endif
#if 1
//Issue an http get request for 'uri'.
//We read the http header into a stack allocated buffer to figure out
//the size of the actual content, then read the content into buf.
int http_connection::http_get(std::string_view uri, util::svector<char>& buf){
  static constexpr std::string_view success_response = "HTTP/1.1 200 OK\r\n"sv;
  static constexpr std::string_view length_field = "Content-Length: "sv;
  static constexpr std::string_view header_delim = "\r\n\r\n"sv;
  //This is the maximum http header size for most servers, if we get a
  //header thats larger than this we can just crash.
  static constexpr size_t tmp_buf_sz = 8096;
  char tmp_buf[tmp_buf_sz];//Stack allocated buffer for the header.
  int tmp_buf_len = 0;
  int nbytes = snprintf(tmp_buf, tmp_buf_sz,
                        "GET %s HTTP/1.1\r\nHost: %s\r\n\r\n",
                        uri.data(), hostname.data());
//  DEBUG_PRINTF("Sending http GET request:\n%s\n", tmp_buf);
  if(bio.write(std::string_view(tmp_buf, nbytes)) < 0){
    return -1;
  }
//  DEBUG_PRINTF("Reading response.\n");
  //This should be enough to read the whole header, without reading
  //too much actual content.
  nbytes = bio.read(tmp_buf, 512);
  if(nbytes <= 0){ return (nbytes ? -1 : nbytes); }
  tmp_buf_len = nbytes;
  tmp_buf[tmp_buf_len] = '\0';
  //Check for a 200 OK response, return response code if we get something else.
  if(strncmp(tmp_buf, success_response.data(), success_response.size()) != 0){
    char *status_code_offset = (char*)strchr(tmp_buf, ' ');
    if(status_code_offset){
      int status_code = strtol(status_code_offset, &status_code_offset, 10);
      if(status_code > 0){
        return status_code;
      }
    }
    fprintf(stderr, "Error malformed http status line %s.\n", tmp_buf);
    return -1;
  }
  //Find the end of the header (two empty lines in a row)
  char *header_end = strstr(tmp_buf, header_delim.data());
  while(header_end == nullptr){
    nbytes = bio.read(tmp_buf + tmp_buf_len, tmp_buf_sz - tmp_buf_len -1);
    if(nbytes <= 0){  return (nbytes ? -1 : nbytes); }
    tmp_buf_len += nbytes;
    tmp_buf[tmp_buf_len] = '\0';
    header_end = strstr(tmp_buf, header_delim.data());
  }
  //Figure out content length and copy any content we already read from
  //tmp_buf into buf.
  int header_length = (header_end - tmp_buf) + header_delim.size();
//  DEBUG_PRINTF("Response header:\n%.*s\n", header_length, tmp_buf);
  char *length_offset = strstr(tmp_buf, length_field.data());
  if(!length_offset){
    fprintf(stderr, "Error could not find Content-Length in header.\n");
    return -1;
  }
  int content_length = strtol(length_offset + length_field.size(),
                              &length_offset, 0);
  if(content_length == 0){
    fprintf(stderr, "Error parsing content length, or content length was 0.\n");
    return -1;
  }
  buf.clear();
  buf.reserve(content_length);
  int content_read = tmp_buf_len - header_length;
  memcpy(buf.data(), tmp_buf + header_length, content_read);

  nbytes = bio.read_n(buf.data() + content_read, content_length - content_read);
  if(nbytes <= 0){  return (nbytes ? -1 : nbytes); }
  assert((nbytes + content_read) == content_length);
  buf.set_length(content_length);
  return 0;
}
#endif
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
  vndb_log->printf(util::log_level::debug, "Sending login command %s.\n",
                   this->buf.c_str());
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
  assert(has_prefix(response, "error"));
  json ret = json::parse(response.substr(constexpr_strlen("error")));
  if(ret["id"].get_ref<std::string>() == "throttled"){
    DEBUG_PRINTF("Recieved throttled error.\n");
    this->throttled = true;
    //If wait on throttle is true than we don't consider it an error,
    //otherwise we do, since it will cause an early return.
    this->error = !this->wait_on_throttle;
  } else {
    DEBUG_PRINTF("Recieved error %s\n", response.data());
    this->error = true;
  }
  return ret;
}
//If the last response to conn was an error due to the connection
//being throttled then wait for the recommented amount of time and return true
static bool wait_if_throttled(vndb_connection* conn){
  //We have several error returns, so set error to true by default, then
  //we only need to do anything for the one successful return. I was
  //using a 'goto error' before and the compilier yelled at me.
  conn->error = true;
  if(!conn->wait_on_throttle ||
     !strstr(conn->buf.c_str(), "throttled")){
    return false;
  }
  const char *wait_offset = strstr(conn->buf.c_str(), "fullwait");
  if(!wait_offset){ return false; }//super unlikely
  const char *duration_offset = strchr(wait_offset, ':');
  if(!duration_offset) { return false; }
  double duration = strtod(duration_offset + 1, nullptr);
  if(duration == 0.0) { return false; }
  vndb_log->printf(util::log_level::debug, "Throttled, waiting for %d seconds.\n",
                   duration);
  util::sleep(duration);
  conn->error = false;
  conn->throttled = false;
  return true;
}
static bool is_error_response(util::string_view response){
  return has_prefix(response, "error");
}
json vndb_connection::send_get_command_once(int page_no,
                                            util::string_view sort_by){
  static const std::string_view options =
    R"( {"sort" : "%s", "results" : 25, "page" : %d})"sv;
  this->buf.append_formatted(options.data(), sort_by.data(), page_no);

  vndb_log->printf(util::log_level::debug, "Sending get command %s\n",
                   this->buf.c_str());

  //  DEBUG_PRINTF("Sending command %s\n", this->buf.c_str());
  this->buf.append(this->EOT);
  int nbytes_written = this->write_buf();
  if(nbytes_written <= 0){//assume this means we couldn't connect to the server
    vndb_log->printf(util::log_level::warn, "Couldn't send get command to server.\n");
    this->logged_in = false;
    return json_null;
  }
  int nbytes_read = this->read();
  if(nbytes_read <= 0){//Shouldn't happen very often.
    vndb_log->printf(util::log_level::warn, "Couldn't get response from server.\n");
    this->logged_in = false;
    return json_null;
  }
  //maybe not the most elegant way to do this but it should work.
  util::string_view response = this->buf.to_string_view();
  if(is_error_response(response)){
    vndb_log->printf(util::log_level::debug, "Got error response %.*s.\n",
                     (int)response.size(), response.data());
    //We can't retry the command if we get a throttled response, We could
    //save the command after we send it, but that would defeat the
    //purpose of this function in the first place.
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
      vndb_log->printf(util::log_level::warn, "Got error response.\n");
      //Store the error in case the calling function wants more details.
      results.push_back(result);
      return -1;
    }
    this->buf.clear();
    this->buf.append(cmd);
    //If wait_on_throttle is false then error gets set to true and
    //we take the above branch, so no need to check that here.
    if(this->throttled){
      DEBUG_PRINTF("Received throttled error for %s, waiting %f seconds.\n",
                   result["type"].get_ref<std::string>().c_str(),
                   result["fullwait"].get<double>());

      util::sleep(result["fullwait"].get<double>());
      //we could avoid incrementing the page number until later and
      //then remove this.
      page_no--;
      this->throttled = false;
      continue;
    }
    //definately not the fastest way to do this
    json::array_t& items = result["items"].get_ref<json::array_t>();
    for(size_t i = 0; i < items.size(); i++){
      results.emplace_back(std::move(items[i]));
    }
    more = result["more"].get<bool>();
  }
  return results.size();
}
int vndb_connection::send_get_command(get_callback& callback,
                                      util::string_view sort_by){
  std::string cmd = this->buf.to_std_string();//copy command
  int page_no = 1;
  int count = 0;
  bool more = true;
  while(more){
    //Should proably reuse this storage.
    json result = this->send_get_command_once(page_no++, sort_by);
    if(this->error){
      vndb_log->printf(util::log_level::warn, "Got error response.\n");
      return -1;
    }
    this->buf.clear();
    this->buf.append(cmd);
    //If wait_on_throttle is false then error gets set to true and
    //we take the above branch, so no need to check that here.
    if(this->throttled){
      DEBUG_PRINTF("Received throttled error for %s, waiting %f seconds.\n",
                   result["type"].get_ref<std::string>().c_str(),
                   result["fullwait"].get<double>());

      util::sleep(result["fullwait"].get<double>());
      //we could avoid incrementing the page number until later and
      //then remove this.
      page_no--;
      this->throttled = false;
      continue;
    }
    std::vector<json>& items = result["items"].get_ref<std::vector<json>>();
    int err = callback(items);
    if(err != 0){
      DEBUG_PRINTF("Callback returned error %d.\n", err);
      vndb_log->printf(util::log_level::warn, "Got error from callback %d.\n", err);
      return err;
    }
    count += items.size();
    more = result["more"].get<bool>();
  }
  return count;
}
bool vndb_connection::send_set_command(){
  this->buf.append(this->EOT);
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
int vndb_connection::get(vndb::object_type what, int start, int stop,
                         std::vector<json>& vec){
  this->buf.clear();
  std::string_view command_base = this->get_get_command_base(what);
  buf.append(command_base);
  buf.append_formatted("(id >= %d and id <= %d)", start, stop);
  vec.reserve(stop - start);
  return this->send_get_command(vec);
}
int vndb_connection::get(vndb::object_type what, int start, int stop,
                         get_callback& callback){
  this->buf.clear();
  std::string_view command_base = this->get_get_command_base(what);
  buf.append(command_base);
  buf.append_formatted("(id >= %d and id <= %d)", start, stop);
  return this->send_get_command(callback);
}
int vndb_connection::get_all(vndb::object_type what,
                             get_callback& callback, int start){
  this->buf.clear();
  std::string_view command_base = this->get_get_command_base(what);
  buf.append(command_base);
  //Not sure if it's documented but theres a limit of 999 for the page field
  //so if we have more than 25000 items we need to use muliple commands
  buf.append_formatted("(id >= %d and id <= %d)", start, start + 24999);
  int cnt = this->send_get_command(callback);
  int total = cnt;
  while(cnt > 20000){
    buf.append_formatted("(id >= %d and id <= %d)", start + total, start + total + 24999);
    cnt = this->send_get_command(callback);
    total += cnt;
  }
  return total;
}
json vndb_connection::dbstats(){
  this->buf.clear();
  this->buf.append("dbstats\x4", constexpr_strlen("dbstats\x4"));
  vndb_log->printf(util::log_level::debug, "Sending dbstats command: %s.\n", this->buf.c_str());
  this->write_buf();
  this->read();
  util::string_view response = this->buf.to_string_view();
  if(is_error_response(response)){
    return this->get_error();
  }
  if(!has_prefix(response, "dbstats")){
    fprintf(stderr, "Got unexpected result from server: %s\n",
            this->buf.c_str());
    //not strictly true but we need to indicate an error somehow
    this->error = true;
    return json_null;
  }
  vndb_log->printf(util::log_level::debug, "Got dbstats result %.*s.\n",
                   (int)response.size(), response.data());
  return json::parse(response.substr(constexpr_strlen("dbstats")));
}
bool vndb_connection::set_vote(int vn_id, int value){
  this->buf.clear();
  this->buf.append_formatted("set votelist %d {\"vote\" : %d}",
                             vn_id, value);
  //TODO: Refactor this into aseperate functino some how.
  if(this->send_set_command()){
    return true;
  } else if(wait_if_throttled(this)){
    //Try again if we got a throttled error
    return this->set_vote(vn_id, value);
  } else {
    return false;
  }
}
bool vndb_connection::remove_vote(int vn_id){
  this->buf.clear();
  this->buf.append_formatted("set votelist %d", vn_id);
  if(this->send_set_command()){
    return true;
  } else if(wait_if_throttled(this)){
    //Try again if we got a throttled error
    return this->remove_vote(vn_id);
  } else {
    return false;
  }
}
//priority is 0:high, 1:medium, 2:low, 3:blacklist
bool vndb_connection::set_wishlist(int vn_id, int priority){
  assert(priority >= 0 && priority <= 3);
  this->buf.clear();
  this->buf.append_formatted("set wishlist %d {\"priority\" : %d}",
                            vn_id, priority);
  if(this->send_set_command()){
    return true;
  } else if(wait_if_throttled(this)){
    //Try again if we got a throttled error
    return this->set_wishlist(vn_id, priority);
  } else {
    return false;
  }
}

bool vndb_connection::remove_from_wishlist(int vn_id){
  this->buf.clear();
  this->buf.append_formatted("set wishlist %d", vn_id);
  if(this->send_set_command()){
    return true;
  } else if(wait_if_throttled(this)){
    //Try again if we got a throttled error
    return this->remove_from_wishlist(vn_id);
  } else {
    return false;
  }
}
//status 0=Unknown, 1=playing, 2=finished, 3=stalled, 4=dropped.
bool vndb_connection::set_vnlist(int vn_id, int status){
  assert(status >= 0 && status <= 4);
  this->buf.clear();
  this->buf.append_formatted("set vnlist %d {\"status\": %d}", vn_id, status);
  if(this->send_set_command()){
    return true;
  } else if(wait_if_throttled(this)){
    //Try again if we got a throttled error
    return this->set_vnlist(vn_id, status);
  } else {
    return false;
  }
}
//I've never set a note on my vnlist, so I doubt I'll use this, currenly
//I just ignore the note paramater.
bool vndb_connection::set_vnlist(int vn_id, std::string_view note, int status){
  return this->set_vnlist(vn_id, status);
}
bool vndb_connection::remove_from_vnlist(int vn_id){
  this->buf.clear();
  this->buf.append_formatted("set vnlist %d", vn_id);
  if(this->send_set_command()){
    return true;
  } else if(wait_if_throttled(this)){
    //Try again if we got a throttled error
    return this->remove_from_vnlist(vn_id);
  } else {
    return false;
  }
}
