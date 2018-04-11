#include <random>
#include <stdio.h>
#include <string>
#include <vector>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdlib.h>
#include <limits.h>
#include <functional>
#include <utility>
struct FILE_wrapper {
  FILE *f = nullptr;  
  size_t getline(std::string &lineptr, char delim = '\n',
                  bool store_delim = false);

  size_t getdelim(std::string &lineptr, const std::function<bool(int)>& test,
                  bool store_delim = false);
  size_t getline2(std::string &lineptr, bool store_delim = false);
};
size_t FILE_wrapper::getline(std::string &lineptr, char delim,
                             bool store_delim){
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
size_t FILE_wrapper::getdelim(std::string &lineptr, 
                              const std::function<bool(int)> &test,
                              bool store_delim){
  lineptr.clear();
  int ch;
  while((ch = getc(f)) != EOF){
    if(test(ch)){
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
size_t FILE_wrapper::getline2(std::string &lineptr, bool store_delim){
  auto test = [](int ch){return ch == '\n';};
  return getdelim(lineptr, test, store_delim);
}
#if 0
#define NANO_SCALE 1000000000
#define NANO_SCALE_FLOAT 1e9
#define MICRO_SCALE 1000000
#define MICRO_SCALE_FLOAT 1e6

double timespec_to_float(struct timespec t){
  return t.tv_sec + (t.tv_nsec / NANO_SCALE_FLOAT);
}
struct timespec timeval_to_timespec(struct timeval tv){
  struct timespec ts;
  ts.tv_sec = tv.tv_sec;
  ts.tv_nsec = (tv.tv_usec * (NANO_SCALE/MICRO_SCALE));
  return ts;
}
struct timespec get_current_time(){
  struct timespec ts;
#if (defined _POSIX_TIMERS) && (_POSIX_TIMERS > 0)
  clock_gettime(CLOCK_REALTIME, &ts);
#else
  struct timeval tv;
  gettimeofday(&tv, NULL);
  ts =  timeval_to_timespec(tv);
#endif
  return ts;
}
struct timespec get_cpu_time(){
  struct timespec ts;
#if (defined _POSIX_TIMERS) && (_POSIX_TIMERS > 0) && (defined _POSIX_CPUTIME)
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
#else
  clock_t clk = clock();
  int64_t nsecs = (clk*NANO_SCALE)/CLOCKS_PER_SEC;
  ts.tv_sec = nsecs/NANO_SCALE;
  ts.tv_nsec = nsecs % NANO_SCALE;
#endif
  return ts;
}
double float_time(){
  return timespec_to_float(get_current_time());
}
double float_cputime(){
  return timespec_to_float(get_cpu_time());
}

struct function_timer {
  function_timer(std::function<int()> fn)
    : fn{fn} {}
  template<typename T, typename... Ts>
  function_timer(T fn, Ts &&... Args)
    : fn(std::bind(fn, std::forward<Ts>(Args)...)) {}
    
  std::function<int()> fn;
  
  double time_fn(int count = 1){
    double start = 0.0, end = 0.0, total = 0.0;
    for(int i = 0; i < count; i++){
      start = float_cputime();
      if(fn() < 0){
        return -1.0;
      }
      end = float_cputime();
      total += (end - start);
    }
    return total / count;
  }
};
/*
  Concatenate lines from infile and write result to outfile.
*/  
int cat_lines_c(const char *infile_path, const char *outfile_path){
  FILE* infile = fopen(infile_path, "r");
  FILE* outfile = fopen(outfile_path, "w");
  if(!infile || !outfile){return -1;}
  char *lineptr = NULL;
  size_t n = 0;
  ssize_t nbytes = 0;
  while((nbytes = getline(&lineptr, &n, infile)) > 0){
    fwrite(lineptr, 1, nbytes-1, outfile);
  }
  fclose(infile);
  fclose(outfile);
  free(lineptr);
  return 0;
}
int cat_lines_cpp(const char *infile_path, const char *outfile_path){
  std::ifstream infile = std::ifstream(infile_path);
  std::ofstream outfile = std::ofstream(outfile_path);
  std::string line;
  while(std::getline(infile, line)){
    outfile << line;
  }
  return 0;
}
/*
  Read an integer from each line of infile and compute the sum.
*/
int sum_file_c(const char *infile_path){
  FILE* infile = fopen(infile_path, "r");
  if(!infile){return -1;}
  char *lineptr = NULL;
  size_t n = 0;
  int sum = 0;
  while(getline(&lineptr, &n, infile) > 0){
    sum += strtol(lineptr, NULL, 0);
  }
  fclose(infile);
  free(lineptr);
  printf("%d\n", sum);
  return sum;
}
int sum_file_cpp(const char *infile_path){
  std::ifstream infile = std::ifstream(infile_path);
  std::string line;
  int sum = 0;
  while(std::getline(infile, line)){
    sum += std::stoi(line);
  }
  std::cout << sum << '\n';
  return sum;
}

int main(int argc, const char *argv[]){
  //don't sync C and C++ io, should speed up C++ io.
//  std::ios::sync_with_stdio(false);
  chdir("/tmp"); 
  char in_template[] = {'t','e','m','p','i','n','X','X','X','X','X','X'};
  char out_template[] = {'t','e','m','p','o','u','t','X','X','X','X','X','X'};
  //Hang on to these file descriptors to prevent the files from being deleted.
  int in_fd = mkstemp(in_template);
  int out_fd = mkstemp(out_template);

  //Create a file of random numbers between 0-1000, which sums to 
  //(INT_MAX-1000) ± 1000
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> dis(0, 10000);
  unsigned int sum = 0;
  fprintf(stderr, "Creating file of random numbers\n");
  while(1){
    int r = dis(gen);
    sum += r;
    if(sum > INT_MAX){ break; }
    dprintf(in_fd, "%d\n", r);
  }
  
  auto cat_c_timer = function_timer(cat_lines_c, in_template, out_template);
  auto cat_cpp_timer = function_timer(cat_lines_cpp, in_template, out_template);
  auto sum_c_timer = function_timer(sum_file_c, in_template);
  auto sum_cpp_timer = function_timer(sum_file_cpp, in_template);

  fprintf(stderr, "Timing CPP I/0 concatenation\n");
  double cat_cpp_time = cat_cpp_timer.time_fn(100);
  fprintf(stderr, "Timing CPP I/0 summation\n");
  double sum_cpp_time = sum_cpp_timer.time_fn(10);

  fprintf(stderr, "Timing C I/0 concatenation\n");
  double cat_c_time = cat_c_timer.time_fn(10);
  fprintf(stderr, "Timing C I/0 summation\n");
  double sum_c_time = sum_c_timer.time_fn(10);



  fprintf(stderr,
          "Time to concatenate lines using C I/O: %f\n"
          "Time to concatenate lines using CPP I/O: %f\n"
          "Time to sum lines using C I/O: %f\n"
          "Time to sum lines using CPP I/O: %f\n",
          cat_c_time, cat_cpp_time, sum_c_time, sum_cpp_time);
  close(in_fd);
  close(out_fd);
}

  
  
  
#endif
