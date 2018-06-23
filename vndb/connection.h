#ifndef __VNDB_H__
#error "Don't include connection.h directly use vndb.h"
#endif
//struct dealing with SSL details.
struct BIO_wrapper {
  BIO* bio = NULL;
  BIO_wrapper() = default;
  BIO_wrapper(const char* hostname, const char* port,
              SSL_CTX* ctx = vndb_ctx);
  BIO_wrapper(std::string_view hostname, std::string_view port,
              SSL_CTX* ctx = vndb_ctx)
    : BIO_wrapper(hostname.data(), port.data(), ctx) {}
  ~BIO_wrapper();
  bool is_encrypted();
  void reset();
  bool connect();
  bool reconnect(){
    reset();
    return connect();
  }
  ssize_t write(std::string_view msg);
  //Read upto len bytes into buf, if BIO_read returns 0 an error message
  //is printed (though you don't need to treat it as an error).
  ssize_t read(char *buf, size_t len);
  //Read exactly n bytes into buf.
  ssize_t read_n(char *buf, size_t n);
  //these next two functions repeatidly call BIO_read, if there is an error
  //the length of buf is set to 0 and the error code is returned, otherwise
  //the total number of bytes read is returned.

  //read until BIO_read returns 0.
  ssize_t read_all(util::svector<char> &buf);
  //read until delim is read, delim is assumed to be the last character of
  //the message, so only the last character read is ever checked.
  ssize_t read_delim(util::svector<char> &buf, char delim);
  //read until delim is read, delim is assumed to be a suffix of the message.
  ssize_t read_delim(util::svector<char> &buf, const char* delim);
  //  ssize_t read_delim(const char **buf, size_t *bufsz, char delim);
  operator BIO*() const noexcept {
    return bio;
  }
};
struct http_connection {
  //For vndb we need to exclude the www, since I'm not going to handle
  //redirects, and www.vndb.org will give a redirect response.
  std::string hostname = "vndb.org";
  std::string port = "443";
  BIO_wrapper bio;
  int err = 0;
  http_connection(bool do_connect = true)
    : bio(hostname.c_str(), port.c_str(), vndb_ctx) {
    if(do_connect){
      init();
    }
  }
  http_connection(std::string_view hostname, std::string_view port, 
                  bool do_connect = true)
    : hostname{hostname}, port{port}, bio(hostname, port, vndb_ctx) {
      if(do_connect){
        init();
      }
    }
  bool full_reset(){
    std::string hostname(std::move(this->hostname));
    std::string port(std::move(this->port));
    this->~http_connection();
    new (this) http_connection(hostname, port, false);
    return bio.connect();
  }
  void reset(){
    bio.reset();
  }
  bool connect(){
    return bio.connect();
  }
  bool reconnect(){
    bio.reset();
    return bio.connect();
  }
  void init(){
    if(bio){
      err = !bio.connect();
    } else {
      err = 1;
    }
  }
  //Perform an http get request for 'uri' and copy the response body into buf.
  //return 0 for a 200 response, a negitive number if there was an error in
  //the underlying connection and return the http response code otherwise.
  int http_get(std::string_view uri, util::svector<char>& buf);
  
  //indicates if there was an error in the constructor.
  operator bool(){
    return err == 0;
  }
};
//definition of different vndb objects, since we need this in
//vndb_connection.

//Struct dealing with communication with the vndb server.
struct vndb_connection {
  static constexpr char EOT = 0x4; // message terminator
  BIO_wrapper bio;
  string_buf buf;
  std::string username;
  std::string passwd;
  bool logged_in = false;
  bool error = false;//set to true if server returns an error
  bool wait_on_throttle = true;
  bool throttled = false;
  static constexpr const char *hostname = vndb_hostname;
  static constexpr const char *port = vndb_tls_port_number;
  //Get commands, without the filter or options, to get all possible
  //information for the different types of vndb objects.
  static constexpr std::string_view get_vn_command_base =
    "get vn basic,details,anime,relations,tags,stats,screens,staff "sv;
  static constexpr std::string_view get_release_command_base =
    "get release basic,details,vn,producers "sv;
  static constexpr std::string_view get_producer_command_base =
    "get producer basic,details,relations "sv;
  //Excludes the character measurements, since I really don't need them.
  static constexpr std::string_view get_character_command_base =
    "get character basic,details,traits,vns,voiced "sv;
  static constexpr std::string_view get_staff_command_base =
    "get staff basic,details,aliases,vns,voiced "sv;
  static constexpr std::string_view get_get_command_base(vndb::object_type what){
    //    assert(what != vndb::object_type::tag);
    //    assert(what != vndb::object_type::trait);
    switch(what){
      case(vndb::object_type::VN): return get_vn_command_base;
      case(vndb::object_type::release): return get_release_command_base;
      case(vndb::object_type::producer): return get_producer_command_base;
      case(vndb::object_type::character): return get_character_command_base;
      case(vndb::object_type::staff):  return get_staff_command_base;
      default:
        assert(false);
    }
  }


  vndb_connection(std::string_view username, std::string_view passwd,
                  bool wait = true, bool login = true)
      : bio(vndb_hostname, vndb_tls_port_number, vndb_ctx),
        buf(4096),
        username(username), passwd(passwd), wait_on_throttle{wait} {
    if(login && this->bio.connect()){
      this->logged_in = this->login();
    }
  }
  vndb_connection(bool wait = true, bool login = true)
    : vndb_connection("","",wait,login) {}

  //both write and write_buf will append an EOT if it is missing
  int write_buf();//write the contents of buf to bio.
  int write(const char *str, size_t len);//write str to bio.
  int read();//synchronous read from bio into buf, reads until EOT.
  //Given a 'get' command in buf, append an appropiate options argument
  //using the given string as the argument to sort, send it to the
  //server, and read the results. If more than one page of results
  //is available repeat until there are no more results left.
  //The value returned from the server is checked for errors.
  //returns number of results, or -1 on error, on error any
  //results already retrieved are kept, and the json object containing
  //the error response can be obtained using get_error;
  int send_get_command(std::vector<json>& results,
                       util::string_view sort_by = "id");
  //Run a get command and call 'callback' with each page of results as
  //they are retrieved. The callback is free to modify the vector of items
  //it is given. If 'callback' returns a nonzero value then the function
  //will immediately terminate and return that value.
  using get_callback = std::function<int(std::vector<json>&)>;
  int send_get_command(get_callback &callback,
                       util::string_view sort_by = "id");
  //Internal helper function for send_get_command.
  json send_get_command_once(int page_no,
                             util::string_view sort_by = "id");
  //assumes an error response is in buf, parses it as json.
  json get_error();
  bool login();
  //returns true if already logged in, and if not tries to login and returns
  //the result of that attempt.
  bool ensure_logged_in(){
    if(logged_in){ 
      return true; 
    } else {
      return relogin();
    }
  }
  //sets username and passwd, can be used to switch from an anonymous
  //session to a specific user.
  bool login(std::string_view username, std::string_view passwd){
    this->username = username;
    this->passwd = passwd;
    return relogin();
  }
  //just a different name for the above function
  bool relogin(std::string_view username, std::string_view passwd){
    return login(username, passwd);
  }
  bool relogin(){
    bool connected = bio.reconnect();
    logged_in = (connected ? login() : false);
    return logged_in;
  }
  //TODO: make get_vns just call this internally.
  //send a get command for the given type to get entries
  //where id>= start && id < stop
  int get(vndb::object_type what, int start, int stop,
          std::vector<json>& results);
  int get(vndb::object_type what, int start,
          std::vector<json>& results){
    return get(what, start, start + 25, results);
  }
  int get(vndb::object_type what, int start, int stop,
          get_callback& callback);
  int get_all(vndb::object_type what, get_callback& callback, int start = 1);
  // //Get a number of vns from the server starting at a given id
  // //and ending at either a given id, or id+25 (which is the max
  // //number of vns we can get per response.
  // int get_vns(int start, int stop, std::vector<json>& vec){
  //  return get(vndb::object_type::VN, start, stop, vec);
  // }
  // int get_vns(int start, std::vector<json>& vec){
  //   return get_vns(start, start + 25, vec);
  // }
  // //You need to check this->error after calling this, since on error
  // //the error response is stored in the vector.
  // std::vector<json> get_vns(int start, int count = 25){
  //   std::vector<json> ret;
  //   (void)get_vns(start, start + count, ret);
  //   return ret;
  // }
  json dbstats();
  //TODO: change these so they get 100 results at a time, since
  //thats allowed.
  json get_vnlist(std::vector<json>& vec){
    buf.clear();
    buf.append("get vnlist (uid = 0)");
    return send_get_command(vec);
  }
  json get_votelist(std::vector<json>& vec){
    buf.clear();
    buf.append("get votelist (uid = 0)");
    return send_get_command(vec);
  }
  json get_wishlist(std::vector<json>& vec){
    buf.clear();
    buf.append("get wishlist (uid = 0)");
    return send_get_command(vec);
  }
  int get_current_user_id(){
    buf.clear();
    buf.append("get user (id = 0)");
    return do_get_user();
  }
  int get_user_id(std::string_view name){
    buf.clear();
    buf.append("get user (username = ").append(name).append(')');
    return do_get_user();
  }
  //send the get user command currently in the buffer, return
  //0 if no user was found, the user id if one user was found
  //and -1 if multiple users were found, I may add a function
  //to search for multiple users later.
  int do_get_user(){
    const json tmp = send_get_command_once(1, "id");
    int cnt = tmp["num"].get<int>();
    if(cnt == 0){
      return 0;//0 is an invalid user
    } else if(cnt == 1){
      return tmp["items"][0]["id"].get<int>();
    } else {
      return -1;
    }
  }

    
  //Functions to add/remove/modify votes / entries on user lists.
  //value is a score on [10,100]
  bool send_set_command();
  bool set_vote(int vn_id, int value);
  bool remove_vote(int vn_id);
  //priority is 0:high, 1:medium, 2:low, 3:blacklist
  bool set_wishlist(int vn_id, int priority);
  bool remove_from_wishlist(int vn_id);
  //status 0=Unknown, 1=playing, 2=finished, 3=stalled, 4=dropped.
  bool set_vnlist(int vn_id, int status = 0,  std::string_view notes = ""sv);
  bool remove_from_vnlist(int vn_id);
};

/* Local Variables: */
/* mode: C++ */
/* End: */
