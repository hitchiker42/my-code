#ifndef __FILESYSTEM_H__
#define __FILESYSTEM_H__
#include <string>
#include <vector>
#include <optional>
#include <string_view>
#include <stdio.h>

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
//Simple RAII wrapper for a FILE* which closes the file on destruction.
//implicitly convertible to a raw FILE*
struct FILE_wrapper {
  FILE *f = nullptr;
  //Constructor doesn't throw if file fails to open (maybe it should?)
  //instead the whole struct is implicitly convertible to bool, which can
  //be used to check if opening the file succeeded.
  FILE_wrapper(const char* path, const char* mode) noexcept
    : f{fopen(path, mode)} {};

  FILE_wrapper(std::string_view path, const char* mode) noexcept
    : f{fopen(path.data(), mode)} {};
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
  operator bool() const noexcept {
    return (f && !feof(f) && !ferror(f));
  }
  operator FILE*() const noexcept {
    return f;
  }
  FILE* unwrap(){
    return f;
  }
  //Use this if you don't want the FILE to close when this goes out of scope
  void set_to_null(){
    f = nullptr;
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
  bool eof(){
    return feof(f);
  }
  bool error(){
    return ferror(f);
  }
  size_t write(const void *ptr, size_t sz){
    return fwrite(ptr, 1, sz, f);
  }
  size_t read(void *ptr, size_t sz){
    return fread(ptr, 1, sz, f);
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
  //0 at end of file.
  //This allows: while(f.getline(str)){..} to work as expected, whereas
  //posix getline which returns -1 on end of file requires adding ' '> 0'
  //to the conditional, which is easily forgetten (at least in my experience)
  size_t getline(std::string &lineptr, char delim = '\n',
                  bool store_delim = false){
    //We can't use posix getline or std::getline due to type conflicts.
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
  //could cache this, since it involves multiple syscalls
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

  void to_string(char **buf, size_t *n) const {
    *buf = nullptr; //Make sure *buf is null if we hit an error.
    *n = 0;
    long orig_pos = getpos();
    //I'm just going to assume fseek succeeds, since if it fails
    //something else has probably already gone wrong.
    fseek(f, 0, SEEK_END);
    //if(fseek(f, 0, SEEK_END) < 0){ return; }
    *n = getpos();
    fseek(f, 0, SEEK_SET);
    //if(fseek(f, 0, SEEK_SET) < 0){ return; }
    *buf = (char*)malloc((*n) + 1);
    fread(*buf, *n, 1, f);
    *buf[*n] = '\0';
    fseek(f, orig_pos, SEEK_SET);
    return;
  }
  std::string to_string() const {
    long orig_pos = getpos();
    //I'm just going to assume fseek succeeds, since if it fails
    //something else has probably already gone wrong.
    fseek(f, 0, SEEK_END);
    //if(fseek(f, 0, SEEK_END) < 0){ return; }
    size_t sz = getpos();
    fseek(f, 0, SEEK_SET);
    std::string ret(sz, '\0');
    fread(ret.data(), sz, 1, f);
    fseek(f, orig_pos, SEEK_SET);
    return ret;
  }
  const char* c_str(){
    char *ret;
    size_t n;
    to_string(&ret, &n);
    return ret;
  }

};

//Read an entire file into a string/vector of lines, returns result as in a
//std::optional wrapper to indicate error.
bool file_to_string(const fs::path &path, char **strptr, size_t *sz_ptr) noexcept;
const char* file_to_string(const fs::path &path) noexcept;
std::optional<std::string> file_to_string_opt(const fs::path &path) noexcept;
std::optional<std::vector<std::string>>
file_to_lines(const fs::path &path, char delim = '\n') noexcept;

bool write_to_file(const fs::path &path, const std::string_view data);
bool write_to_file(const fs::path &path,
                   std::initializer_list<std::string_view> data);
#endif
#if 0
//TODO: Need to figure out how to do this efficently but still keep
//it small enough to fit in the header.
//write_to_file for containers of strings, output is optionally seperated
//by a given delimiter (defaults to newline).
template<typename T>
bool write_to_file(const fs::path &path, const T& container,
                   const char* delim = "\n");
#endif
