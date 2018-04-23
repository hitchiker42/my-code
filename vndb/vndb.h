#ifndef __VNDB_H__
#define __VNDB_H__
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
static SSL_CTX* vndb_ctx;
using string_buf = fmt::MemoryWriter;
struct BIO_wrappper {
  BIO* bio = NULL;
  BIO_wrapper() = default;
  BIO_wrapper(const char* hostname, const char* port,
              SSL_CTX* ctx = vndb_ctx);
  void reset();
  bool connect();
  ssize_t write(std::string_view msg);
  ssize_t read(const char *buf, ssize_t len);
  ssize_t read_delim(util::svector<char> &buf, char delim);
  ssize_t read_delim(const char **buf, size_t *bufsz, char delim);
};
struct vndb_connection {
  BIO_wrapper *bio;
  string_buf buf;
  std::string username;
  std::string passwd;
  vndb_connection(std::string_view username, std::string_view passwd);
};
#endif /* __VNDB_H__ */
