#define NEED_TIMER
#include "util.h"
#include "xorshift.h"
#include "filesystem.h"
#include <random>
void print_histogram(FILE_wrapper& outfile,
                     int *hist, int size, int total_count){
  static const char fill[81] = "########################################"
                              "########################################";
  static const char space[81] = "                                        "
                              "                                        ";
  int div = total_count / (80*(size/2));
  for(int i = 0; i < size; i++){
    if(hist[i]){
      int count = std::min(hist[i] / div, 80);
      outfile.printf("%.3d: %.*s%.*s|\n", i, count, fill,
                     80-count, space);
    }
  }
}

template<typename T>
void test_rng(int size, int count, const char *outfile_path){
  std::uniform_int_distribution<int> dis(0, size);
  std::vector<int> hist(size, 0);
  std::random_device rd;
  T rng(rd);
  for(int i = 0; i < count; i++){
    hist[dis(rng)]++;
  }
  FILE_wrapper outfile(outfile_path, "w");
  if(!outfile){
    fprintf(stderr, "Couldn't open %s\n", outfile_path);
    exit(1);
  }
  print_histogram(outfile, hist.data(), hist.size(), count);
  printf("Max count = %d\n", *std::max_element(hist.begin(), hist.end()));
}
int main(){
  test_rng<util::xoroshiro128_add>(100, 10000000, "xoroshiro128.out");
  test_rng<util::xorshift128_add>(100, 10000000, "xorshift128.out");
  test_rng<util::xorshift1024_star>(100, 10000000, "xorshift1024.out");
}
