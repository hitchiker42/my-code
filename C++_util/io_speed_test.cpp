#define NEED_TIMERS
#include "util.h"
#include "filesystem.h"
#include <random>
#include <iostream>
#include <fstream>
/*
  Concatenate lines from infile and write result to outfile.
*/  
int cat_lines_c(const char *infile_path, const char *outfile_path){
  FILE_wrapper infile(infile_path, open_mode::read);
  FILE_wrapper outfile(outfile_path, open_mode::write);
  if(!infile || !outfile){return -1;}
  char *lineptr = NULL;
  size_t n = 0;
  ssize_t nbytes = 0;
  while((nbytes = infile.getline(&lineptr, &n)) > 0){
    outfile.write(lineptr, nbytes-1);
  }
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
int cat_lines_util(const char *infile_path, const char *outfile_path,
                   bool buffered){
  std::string tmp;
  std::vector<std::string_view> lines = file_to_lines(infile_path, &tmp);
  fd_wrapper out(outfile_path, open_mode::rw | open_mode::trunc);
  //The cost of allocating and filling a buffer is minuscule compared
  //to the time saved by only calling write once.
  char *buf = (char*)malloc(tmp.size());
  char *ptr = buf;
  for(const auto& line : lines){
    ptr = (char*)mempcpy(buf, line.data(), line.size());
  }
  out.write(buf, ptr-buf);
  return 0;
}
    
/*
  Read an integer from each line of infile and compute the sum.
*/
int sum_file_c(const char *infile_path){
  FILE_wrapper infile(infile_path, "r");
  if(!infile){return -1;}
  char *lineptr = NULL;
  size_t n = 0;
  int sum = 0;
  while(infile.getline(&lineptr, &n) > 0){
    sum += strtol(lineptr, NULL, 0);
  }
  free(lineptr);
  return sum;
}
int sum_file_cpp(const char *infile_path){
  std::ifstream infile = std::ifstream(infile_path);
  std::string line;
  int sum = 0;
  while(std::getline(infile, line)){
    sum += std::stoi(line);
  }
  return sum;
}
int sum_file_util(const char *infile_path){
  std::string str = file_to_string(infile_path);
  int sum = 0;
  char *lineptr = str.data();
  char *endptr = nullptr;
  while(1){
    int tmp = strtol(lineptr, &endptr, 0);
    if(lineptr == endptr){
      return sum;
    }
    sum += tmp;
    lineptr = endptr;
  }
  return sum;
}
int main(int argc, const char *argv[]){
  //don't sync C and C++ io, should speed up C++ io.
  std::ios::sync_with_stdio(false);
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
  
  auto cat_c_timer = util::function_timer(cat_lines_c, in_template, out_template);
  auto cat_cpp_timer = util::function_timer(cat_lines_cpp, in_template, out_template);
  auto cat_util_buffered_timer = util::function_timer(cat_lines_util, in_template, out_template, true);
  auto cat_util_unbuffered_timer = util::function_timer(cat_lines_util, in_template, out_template, false);
  auto sum_c_timer = util::function_timer(sum_file_c, in_template);
  auto sum_cpp_timer = util::function_timer(sum_file_cpp, in_template);
  auto sum_util_timer = util::function_timer(sum_file_util, in_template);

  fprintf(stderr, "Timing CPP I/0 concatenation\n");
  double cat_cpp_time = cat_cpp_timer.time_fn(10);
  fprintf(stderr, "Timing CPP I/0 summation\n");
  double sum_cpp_time = sum_cpp_timer.time_fn(10);

  fprintf(stderr, "Timing C I/0 concatenation\n");
  double cat_c_time = cat_c_timer.time_fn(10);
  fprintf(stderr, "Timing C I/0 summation\n");
  double sum_c_time = sum_c_timer.time_fn(10);

  fprintf(stderr, "Timing C++_util I/0 concatenation\n");
  double cat_util_time = cat_util_buffered_timer.time_fn(10);
  fprintf(stderr, "Timing C++_util I/0 summation\n");
  double sum_util_time = sum_util_timer.time_fn(10);



  fprintf(stderr,
          "Time to concatenate lines using C I/O:   %f\n"
          "Time to concatenate lines using CPP I/O: %f\n"
          "Time to concatenate using C++_util I/0:  %f\n"
          "Time to sum lines using C I/O:   %f\n"
          "Time to sum lines using CPP I/O: %f\n"
          "Time to sum using C++_util I/0:  %f\n",
          cat_c_time, cat_cpp_time, cat_util_time,
          sum_c_time, sum_cpp_time, sum_util_time);
  close(in_fd);
  close(out_fd);
}
