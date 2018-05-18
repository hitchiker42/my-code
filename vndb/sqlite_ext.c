#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sqlite3.h>
#include <prce.h>
/*
  Simple regex function thats a bit more optimized that the most
  basic verison. It caches the last regexp used so repeated
  calls with the same pattern won't need to recompile it. 
  If a sql statement has more than one regex call in it however
  this will slow it down slightly, I think its worth the trade off.
*/
typedef struct simple_cache simple_cache;
struct simple_cache {
  const char *pat;
  size_t patlen;
  size_t mem;
  prce *regex;
};
void destroy_simple_cache(void *ptr){
  simple_cache *cache = ptr;
  free(cache->pat);
  pcre_free(cache->regex);
}
//See if the pattern used in 'cache' is the same as pat, if not copy
//pat into cache and free the old regexp.
int check_simple_cache(simple_cache *cache, char *pat, size_t patlen){
  if((cache->patlen == patlen) && !strcmp(pat, cache->pat)){
    return 1;
  }
  cache->patlen = patlen;
  if(cache->mem < patlen){
    cache->pat = realloc(cache->pat, patlen+1);
    cache->mem = patlen + 1;
  }
  memcpy(cache->pat, pat, patlen+1);
  pcre_free(cache->regex);
}

void sqlite_regexp_simple(sqlite3_context* context, int argc,
                          sqlite3_value** values) {
  int ret;
  char* pat = (char*)sqlite3_value_text(values[0]);
  char* text = (char*)sqlite3_value_text(values[1]);

  if (argc != 2 || pat == NULL || text == NULL) {
    sqlite3_result_error(context, "SQL function regexp() "
                         "called with invalid arguments.\n", -1);
    return;
  }
  size_t patlen = strlen(pat);
  size_t text_len = strlen(text);
  simple_cache *cache = sqlite3_user_data(context);

  //Compile pattern if necessary.
  if(!check_simple_cache(cache, pat, patlen)){
    char *errptr;
    int erroffset;
    cache->regex = pcre_compile(pat, PCRE_UTF8, &errptr, &erroffset, NULL);
    if(!cache->regex){
      //make sure we don't get a cache hit for a wrong pattern.
      cache->patlen = 0;
      char buf[512];
      //Print error message in such a way that if anything gets truncated
      //it will be the pattern.
      int nbytes = sprintf(buf, 4096,"error compiling regular expression, "
                           "\"%s\" at offset %d of pattern %s.\n", 
                           errptr, erroffset, pat);
      sqlite3_result_error(context, buf, nbytes);
      return;      
    }
  }
  //We have reason to to save the capture groups, but we need to give
  //pcre space to store them in order to allow backreferences to work.
  int ovector[18];
  ret = pcre_exec(cache->regexp, NULL, text, text_len, 0, 0, ovector, 18);
  //Check for error from pcre_exec
  if(ret < -1){
    char buf[64];
    int nbytes = snprintf(buf, 128, 
                          "Error in pcre_exec, error number %d.\n", ret);
    sqlite3_result_error(context, buf, nbytes);
  }
  sqlite3_result_int(context, ret >= 0);
}
int init_sqlite_ext(sqlite3 *db){
  simple_cache *cache = calloc(sizeof(simple_cache));
  int ret = sqlite3_create_function_v2(db, "regexp", 2, 
                                       SQLITE_UTF8 | SQLITE_DETERMINISTIC,
                                       cache, sqlite_regexp_simple,
                                       NULL, NULL,
                                       destroy_simple_cache);
  if(ret != SQLITE_OK){
    return ret;
  }
  //initialize other extensions
  return ret;
}
// Open an in memory database and copy the contents of the datebase located
// in filename into it. Written using a simlar interface as most sqlite3 api
// functions.
int sqlite_open_db_in_memory(const char* filename, sqlite3 **db_ptr){
  sqlite3 *file = nullptr;
  //insure calling sqlite3_close on *dp_ptr after an error will work.
  *db_ptr = nullptr;
  int err = sqlite3_open_v2(filename, &file,
                            SQLITE_OPEN_READWRITE, nullptr);
  //This doesn't work in C++ since this jump skips the initialization
  //of mem & bkup.
  if(err != SQLITE_OK){ goto end; }

  err = sqlite3_open_v2(":memory:", db_ptr, SQLITE_OPEN_READWRITE, nullptr);
  if(err != SQLITE_OK){ goto end; }

  sqlite3 *mem = *db_ptr;
  sqlite3_backup *bkup = sqlite3_backup_init(mem, "main", file, "main");
  if(!bkup){
    err = sqlite3_errcode(mem);
    goto end;
  }
  err = sqlite3_backup_step(bkup, -1);  //Copy all the pages at once.
  //we just opened both db connections, so these should be impossible
  assert(err != SQLITE_BUSY && err != SQLITE_LOCKED);

  //returns SQLITE_OK if there were no errors on bkup
  err = sqlite3_backup_finish(bkup);

 end:
  sqlite3_close(file);
  return err;
}
