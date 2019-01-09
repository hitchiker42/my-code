#ifndef __FILESYSTEM_H__
#define __FILESYSTEM_H__
#include <string>
#include <vector>
#include <optional>
#include <string_view>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <string.h>
#ifdef __unix__
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#else
#define O_ACCMODE	   0003
#define O_RDONLY	     00
#define O_WRONLY	     01
#define O_RDWR		     02
#define O_TRUNC           01000
#define O_APPEND	  02000
#endif

#if (defined NEED_FILESYSTEM_LIB)
//Use either the C++17 filesystem library or the experimental filesytem
//library if that's not available.
#if __has_include(<filesystem>)
#include <filesystem>
#endif
#if __has_include(<experimental/filesystem>)
#include <experimental/filesystem>
#endif
#if (defined __cpp_lib_filesystem) && (__cpp_lib_filesystem >= 201603)
namespace fs = std::filesystem;
#elif (defined __cpp_lib_experimental_filesystem) && \
  (__cpp_lib_experimental_filesystem >= 201406)
namespace fs = std::experimental::filesystem;
#else
//Some functions are still available without a filesystem library, to
//keep function signitures consistant we create a dummy fs namespace
//which uses std::strings as paths.
#define NO_FS_LIB
namespace fs {
  using path = std::string;
}
#endif //Filesystem
#else /* NEEED_FILESYSTEM_LIB */
#define NO_FS_LIB
namespace fs {
  using path = std::string;
}
#endif
//None of these used to be in the util namespace so I'm still going to
//import them into the global namespace at the end of the header
namespace util {
//inline namespace to make it eaiser to import everything into both
//namespace util and the top level namespace.
inline namespace filesystem {
//Abstraction of file read/write modes than can be used
//with both open and fopen.
struct open_mode {
  enum mode_type : int {
    read = O_RDONLY,
    rdonly = read,
    r = read,
    write = O_WRONLY,
    wronly = write,
    w = write,
    read_write = O_RDWR,
    rdrw = read_write,
    rw = read_write,
    append = O_APPEND,
    a = append,
    trunc = O_TRUNC,
    create = O_CREAT
  };
  int mode;
  constexpr open_mode(int m) : mode{m} {
    assert(((mode & read) == read) ||
           ((mode & write) == write) ||
           ((mode & read_write) == read_write));
  };
  constexpr open_mode(const char *str) : open_mode(parse_mode_string(str)) {}
  static constexpr mode_t parse_mode_string(const char *str){
    int ret = (str[1] == '+' ? read_write : 0);
    switch(str[0]){
      case 'r':
        return (ret | read);
      case 'w':
        return (ret | trunc | write);
      case 'a':
        return (ret | append | write);
      default:
        return 0;
    }
  }
  static constexpr const char* to_mode_string(int mode){
    if(mode & read){
      return "r";
    } else if(mode & write){
      return (mode & append ? "a" : "w");
    } else {
      return (mode & append ? "a+" :
              mode & trunc ? "w+" : "r+");
    }
  }
  constexpr const char* to_string() const {
    return to_mode_string(mode);
  }
  constexpr operator int(){ return mode; }
  constexpr operator const char*(){ return to_string(); }
};
//Simple RAII wrapper for a FILE* which closes the file on destruction.
//implicitly convertible to a raw FILE* (except not really)
//Reading from an offset and converting to a string are possible
//but not threadsafe
struct FILE_wrapper {
  FILE *f = nullptr;
  //Constructor doesn't throw if file fails to open (maybe it should?)
  //instead the whole struct is implicitly convertible to bool, which can
  //be used to check if opening the file succeeded.
  FILE_wrapper(const char* path, const char* mode) noexcept
    : f{fopen(path, mode)} {};
  FILE_wrapper(const char* path, open_mode mode) noexcept
    : FILE_wrapper(path, mode.to_string()) {}

  FILE_wrapper(std::string_view path, const char* mode) noexcept {
    char buf[PATH_MAX];
    memcpy(buf, path.data(), path.size());
    buf[path.size()] = '\0';
    f = fopen(buf, mode);
  }
  FILE_wrapper(std::string_view path, open_mode mode) noexcept
    : FILE_wrapper(path, mode.to_string()) {}

  //Takes ownership of the given FILE pointer.
  explicit FILE_wrapper(FILE *f) noexcept
    : f{f} {};
  /* How a copy constructor could be implemented, if you really wanted.
   explicit FILE_wrapper(const FILE_wrapper &other) noexcept
     : mode{mode} {
       if(!other){
         return;
       } else {
         int fd = fileno(f);
         int fd2 = dup(fd);
         f = fdopen(fd2, mode);
       }
     }
  */
  FILE_wrapper(const FILE_wrapper &other) = delete;
  //Only really useful for allowing these to be stored in containers.
  FILE_wrapper(FILE_wrapper &&other)
    : f{other.f} {
      //make sure when other gets destructed it doesn't close the file.
      other.f = nullptr;
  }

  FILE_wrapper& operator=(const FILE_wrapper& other) = delete;
  //Its usually a bad idea to assign to a FILE_wrapper, even via move-assignment,
  //but it's provided anyway.
  FILE_wrapper& operator=(FILE_wrapper&& other){
    if(&(other) == this){
      return (*this);
    } else {
      this->~FILE_wrapper();
      new (this) FILE_wrapper(std::move(other));
      return (*this);
    }
  }
  ~FILE_wrapper(){
    if(f){fclose(f);}
  }
  operator FILE*() const noexcept {
    return f;
  }
  FILE* unwrap(){
    return f;
  }
  //explicitly close the file to avoid waiting for the destructor.
  void close(){
    fclose(f);
    f = nullptr;
  }
  //Use this if you don't want the FILE to close when this goes out of scope
  void set_to_null(){
    f = nullptr;
  }
  //combines unwrap and set_to_null.
  FILE* release(){
    FILE* ret = f;
    f = nullptr;
    return ret;
  }
#if (defined unix) || (defined __unix) || (defined __unix__)
  int native_handle(){
    return fileno(f);
  }
#elif (defined _WIN32)
  HANDLE native_handle(){
    return (HANDLE)_get_osfhandle(_fileno(f));
  }
#endif
  bool eof() const {
    return feof(f);
  }
  bool error() const {
    return ferror(f);
  }
  bool invalid() const {
    return (!f || eof() || error());
  }
  explicit operator bool() const noexcept {
    return !(invalid());
  }
  void flush(){
    fflush(f);
  }
  size_t write(const void *ptr, size_t nmemb, size_t sz = 1){
    return fwrite(ptr, sz, nmemb, f);
  }
  size_t read(void *ptr, size_t nmemb, size_t sz = 1){
    return fread(ptr, sz, nmemb, f);
  }
  //Emulates pread, but isn't threadsafe.
  size_t read(void *ptr, size_t sz, long offset) const {
    long old_offset = getpos();
    fseek(f,offset, SEEK_SET);
    size_t ret = fread(ptr, 1, sz, f);
    fseek(f,old_offset, SEEK_SET);
    return ret;
  }
  int getchar(){
    return fgetc(f);
  }
  //Could store the last read char and default to ungetting that.
  void ungetchar(int c){
    ungetc(c, f);
  }
  FILE_wrapper& print(std::string_view sv){
    write(sv.data(), sv.size());
    return *this;
  }
  FILE_wrapper& print(int ch){
    fputc(ch, f);
    return *this;
  }
  FILE_wrapper& println(std::string_view sv){
    write(sv.data(), sv.size());
    fputc('\n', f);
    return *this;
  }
  //using a varidaic template instead of va_args should allow
  //compiler warnings for mismatched types.
  template<typename ... Ts>
  int printf(Ts&&... Args){
    return fprintf(f, std::forward<Ts>(Args)...);
  }
  //Returns the number of characters read, including the delimiter, or
  //0 at end of file, this means 1 will be returned for an empty line.
  //This allows: while(f.getline(str)){..} to work as expected, whereas
  //posix getline which returns -1 on end of file requires adding ' '> 0'
  //to the conditional, which is easily forgetten (at least in my experience)
  size_t getline(std::string &lineptr, char delim = '\n',
                 bool store_delim = false){
    //We can't use posix getline with a std::string
    lineptr.clear();
    int ch;
    while((ch = getc(f)) != EOF){
      if(ch == delim){
        if(store_delim){
          lineptr.push_back(ch);
        }
        //Make sure we return non-zero even if we only read a delimiter
        return lineptr.size() + !store_delim;
      }
      lineptr.push_back(ch);
    }
    return lineptr.size();
  }
  //wrapper of posix getline
  ssize_t getline(char **lineptr, size_t *n, char delim = '\n'){
#ifdef __unix__
    return getdelim(lineptr, n, delim, f);
#else
    //inefficent but screw windows
    static std::string tmp;
    size_t count = this->getline(tmp, delim, true);
    if(count == 0){
      return -1;
    }
    if(*n < (count.size()+1)){
      *n = count.size()+1;
      free(*lineptr);
      *lineptr = (char*)malloc(*n);
    }
    memcpy(*lineptr, tmp.c_str(), tmp.size()+1);
    return (ssize_t)count;
#endif
  }
   
  //could cache this, since it involves multiple syscalls
  //using fstat would probably be faster but nonportable.
  size_t size() const {
    long pos = ftell(f);
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, pos, SEEK_SET);
    return sz;
  }
  //File positioning
  long getpos() const {
    return ftell(f);
  }
  //Like fseek, but returns the new position on success.
  long setpos(long offset, int whence){
    if(fseek(f, offset, whence) < 0){
      return -1;
    } else {
      return ftell(f);
    }
  }
  //If relative is true then the position is set to getpos() + offset,
  //otherwise the position as set to offset if it is positive or zero and
  //to size() + offset if it is negitive (to be clear this means
  //passing -1 as offset is the same as using SEEK_END whth a 0 offset)
  //This is to avoid using SEEK_{SET,CUR,END}, which are kinda clumsy
  long setpos(long offset, bool relative = true){
    if(relative){
      return setpos(offset, SEEK_CUR);
    } else {
      if(offset >= 0){
        return setpos(offset, SEEK_SET);
      } else {
        return setpos(offset+1, SEEK_END);
      }
    }
  }
  //Get the current position and file size and set the position to 0.
  void to_string_helper(long *original_pos, size_t *sz) const {
    *original_pos = getpos();
    fseek(f, 0, SEEK_END);
    *sz = getpos();
    fseek(f, 0, SEEK_SET);
  }
  char* to_string(char **buf, size_t *n) const {
    if(invalid()){
      return NULL;
    }
    *buf = nullptr; //Make sure *buf is null if we hit an error.
    long orig_pos;
    to_string_helper(&orig_pos, n);
    *buf = (char*)malloc((*n) + 1);
    fread(*buf, 1, *n, f);
    //NOTE: The parentheses around *buf are necessary,
    //*buf[*n] = buf[*n][0] not buf[0][*n]
    (*buf)[*n] = '\0';
    fseek(f, orig_pos, SEEK_SET);
    return *buf;
  }
  std::string to_string() const {
    long orig_pos;
    size_t sz;
    to_string_helper(&orig_pos, &sz);
    std::string ret(sz, '\0');
    fread(ret.data(), 1, sz, f);
    fseek(f,orig_pos, SEEK_SET);
    return ret;
  }
  const char* c_str() const {
    char *ret;
    size_t n;
    return to_string(&ret, &n);
  }

};

//Wrapper around a raw file descriptor, should be much faster
//for converting a file into a string than the FILE_wrapper.
#ifdef __unix__
struct fd_wrapper {
  int fd = -1;
  //Called by other constructors, if fd is < 0 sets it
  //to -(errno + 1) so we can print the reason for the error later.
  fd_wrapper(int fd)
    : fd{(fd < 0 ? -(errno + 1) : fd)} {}
  ~fd_wrapper(){
    close(fd);//if fd is invalid its no big deal
  }
  fd_wrapper(const char *filename, int flags)
    : fd_wrapper(open(filename, flags)) {}
  //due to overloads calling fd_wrapper(fd) has
  //the same effect as calling fd_wrapper(fd, false)
  fd_wrapper(int fd, bool should_dup)
    : fd_wrapper((should_dup ? dup(fd) : fd)) {}
  int access_mode() const {
    int status_flags = fcntl(fd, F_GETFL, 0);
    return (status_flags < 0 ? status_flags : (status_flags & O_ACCMODE));
  }
  size_t size() const {
    struct stat buf;
    int err = fstat(fd, &buf);
    if(err < 0){
      return (size_t)-1;
    }
    return buf.st_size;
  }
  ssize_t read(void *buf, size_t sz){
    return ::read(fd, buf, sz);
  }
  //const size pread doesn't change the file offset.
  ssize_t read(void *buf, size_t sz, off_t offset) const {
    return pread(fd, buf, sz, offset);
  }
  ssize_t write(const void *buf, size_t sz){
    return ::write(fd, buf, sz);
  }
  ssize_t write(const void *buf, size_t sz, off_t offset) const {
    return pwrite(fd, buf, sz, offset);
  }
  //Read file into a null terminated string allocated with malloc,
  //returns NULL on error.
  char* to_string(char **buf, size_t *sz) const {
    if((*sz = size()) == (size_t)-1){
      return NULL;
    }
    *buf = (char*)malloc(*sz + 1);
    if((size_t)pread(fd, *buf, *sz, 0) != *sz){
      free(buf);
      return NULL;
    }
    (*buf)[*sz] = '\0';
    return *buf;
  }
  //Read file into a std::string, no error checking.
  std::string to_string() const {
    size_t sz = size();
    std::string str(sz, '\0');
    pread(fd, str.data(), sz, 0);
    return str;
  }
  char* c_str() const {
    char *ret;
    size_t tmp;
    return to_string(&ret, &tmp);
  }
  explicit operator bool() const noexcept {
    return fd >= 0;
  }
};
#endif //__unix__
#ifdef __unix__
using to_string_wrapper = fd_wrapper;
#else
using to_string_wrapper = FILE_wrapper;
#endif
//Read an entire file into a string/vector of lines, returns result as in a
//std::optional wrapper to indicate error.
inline bool file_to_string(const char *path, char **strptr, size_t *sz_ptr) noexcept {
  return to_string_wrapper(path, open_mode::read).to_string(strptr, sz_ptr);
}
inline std::string file_to_string(const char *path) noexcept {
  return to_string_wrapper(path, open_mode::read).to_string();
}
inline const char* file_to_cstring(const char *path) noexcept {
  return to_string_wrapper(path, open_mode::read).c_str();
}
//Split a string into lines, the lines don't contain the newline character,
//blank lines are represented by the empty string
template<typename T>
std::vector<T> to_lines(const char *str, size_t sz){
  const char *end = str + sz;
  std::vector<T> ret;
  if(sz == 0){ return ret; }

  const char *line_start = str;
  do {
    if(*str == '\n'){
      ret.emplace_back(line_start, str - line_start);
      line_start = str+1;
    }
  } while(++str != end);
  if(line_start < end){
    ret.emplace_back(line_start, end - line_start);
  }
  return ret;
}
inline std::vector<std::string_view> to_lines_sv(const char *str, size_t sz){
  return to_lines<std::string_view>(str, sz);
}
inline std::vector<std::string> to_lines_str(const char *str, size_t sz){
  return to_lines<std::string>(str, sz);
}
inline std::vector<std::string_view> to_lines_sv(std::string_view str){
  return to_lines<std::string_view>(str.data(), str.size());
}
inline std::vector<std::string> to_lines_str(std::string_view str){
  return to_lines<std::string>(str.data(), str.size());
}
inline std::vector<std::string_view> file_to_lines(const char *path, std::string *strptr){
  *strptr = std::move(file_to_string(path));
  return to_lines_sv(*strptr);
}
inline std::vector<std::string_view> file_to_lines(const char *path, std::string& str){
  str = std::move(file_to_string(path));
  return to_lines_sv(str);
}
inline std::vector<std::string> file_to_lines(const char *path){
  //not super efficent since it allocates memory twice, but it's easy
  std::string file_string = file_to_string(path);
  return to_lines_str(file_string);
}
inline std::vector<std::string_view> file_to_lines(const char *path,
                                                   char **strptr,
                                                   size_t *sz_ptr){
  //not super efficent since it allocates memory twice, but it's easy
  file_to_string(path, strptr, sz_ptr);
  return to_lines_sv(*strptr, *sz_ptr);
}
}//inline namespace filesystem
}//namespace util
#ifndef NO_USING_FILESYSTEM
using namespace util::filesystem;
#endif
/*
std::optional<std::string> file_to_string_opt(const fs::path &path) noexcept;
std::optional<std::vector<std::string>>
file_to_lines_opt(const fs::path &path, char delim = '\n') noexcept;
//With no error checking
std::string file_to_string(const fs::path &path) noexcept;
std::vector<std::string>
file_to_lines(const fs::path &path, char delim = '\n') noexcept;

bool write_to_file(const fs::path &path, const std::string_view data);
bool write_to_file(const fs::path &path,
                   std::initializer_list<std::string_view> data);
*/
#endif //__FILESYSTEM_H__
#if 0
//TODO: Need to figure out how to do this efficently but still keep
//it small enough to fit in the header.
//write_to_file for containers of strings, output is optionally seperated
//by a given delimiter (defaults to newline).
template<typename T>
bool write_to_file(const fs::path &path, const T& container,
                   const char* delim = "\n");
#endif
