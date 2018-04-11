#define NEED_TIMER
#include "util.h"
#include <assert.h>
#include <math.h>
#include <list>
#include <set>
#include <unordered_set>
//The only valid reason to use this for non-random access iterators
//is if element comparision is an order of magnitude slower than 
//incrementing an iterator
template<typename It, typename T, typename Cmp = std::less<>>
It bsearch(It first, It last, const T& val, Cmp cmp = Cmp()){
  It real_last = last;
  size_t size;
  It mid;
  do {
    if constexpr(std::is_same_v<iter_traits_iterator_category<It>,
                                std::random_access_iterator_tag>){
      size = last-first;
      mid = first + size/2;
    } else {
      //slow/fast pointer method of finding the midpoint of a list.
      auto fast = first;
      mid = first;
      size = 1;
      while(fast != last){
        fast++;
        if(fast == last){
          break;
        }
        fast++;        
        mid++;
        size++;
      }
    }
    if(cmp(val, *mid)){
      last = mid;
    } else if(cmp(*mid, val)){
      first = ++mid;
    } else {
      return mid;
    }
  } while (first != last);
  return real_last;
}
template<typename It, typename T, typename Cmp = std::less<>>
It linear_search(It first, It last, const T& val, Cmp cmp = Cmp()){
  while(first != last){
    if(cmp(*first, val)){
      ++first;
    } else {
      if(cmp(val, *first)){
        return last;
      } else {
        return first;
      }
    }
  }
  return last;
}
  
void test_2(){
  std::vector<uint64_t> numbers;
  std::set<uint64_t> numbers_set;
  std::unordered_set<uint64_t> numbers_hash_set;

  for(int i = 0; i < 10000000; i++){
    numbers.push_back(i*i);
    numbers_set.insert(i*i);
    numbers_hash_set.insert(i*i);
  }
  util::cpu_timer timer;
  timer.start();
  for(uint64_t j = 0; j < 10000; j++){
    auto It = bsearch(numbers.begin(), numbers.end(), j*j);
    assert(It != numbers.end());
  }
  timer.stop();
  printf("Time to bsearch a 10000000 element array 10000  times: %f\n", timer.elapsed());
  timer.restart();
  for(uint64_t j = 0; j < 10000; j++){
    auto It = linear_search(numbers.begin(), numbers.end(), j*j);
    assert(It != numbers.end());
  }
  timer.stop();
  printf("Time to linear search a 10000000 element array 10000 times: %f\n", timer.elapsed());
  timer.restart();
  for(uint64_t j = 0; j < 10000; j++){
    auto It = numbers_set.find(j*j);
    assert(It != numbers_set.end());
  }
  timer.stop();
  printf("Time to search a 10000000 element  set 10000 times: %f\n", timer.elapsed());
  timer.reset();
  timer.start();
  for(uint64_t j = 0; j < 10000; j++){
    auto It = numbers_hash_set.find(j*j);
    assert(It != numbers_hash_set.end());
  }
  timer.stop();
  printf("Time to search a 10000000 element hash set 10000 times: %f\n", timer.elapsed());
}
void test_1(){
  std::vector<uint64_t> squares;
//  std::list<uint64_t> squares_list;
//  std::forward_list<uint64_t> squares_forward_list;
  std::set<uint64_t> squares_set;
  std::unordered_set<uint64_t> squares_hash_set;

  for(int i = 0; i < 10000; i++){
    squares.push_back(i*i);
//    squares_list.push_back(i*i);
    squares_set.insert(i*i);
    squares_hash_set.insert(i*i);
  }
  util::cpu_timer timer;
  timer.start();
  for(uint64_t j = 0; j < 10000; j++){
    auto It = bsearch(squares.begin(), squares.end(), j);
    if(It != squares.end()){
      assert(sqrt(j) == trunc(sqrt(j)));
    }
  }
  timer.stop();
  printf("Time to bsearch a 10000 element array 10000000 times: %f\n", timer.elapsed());
  timer.restart();
  for(uint64_t j = 0; j < 10000; j++){
    auto It = linear_search(squares.begin(), squares.end(), j);
    if(It != squares.end()){
      assert(sqrt(j) == trunc(sqrt(j)));
    }
  }
  timer.stop();
  printf("Time to linear search a 10000 element array 10000000 times: %f\n", timer.elapsed());
  timer.restart();
/*
  for(uint64_t j = 0; j < 10000; j++){
    auto It = bsearch(squares_list.begin(), squares_list.end(), j);
    if(It != squares_list.end()){
      assert(sqrt(j) == trunc(sqrt(j)));
    }
  }
  timer.stop();
  printf("Time to bsearch a 10000 element list 100000 times: %f\n", timer.elapsed());
  timer.restart();
  for(uint64_t j = 0; j < 10000; j++){
    auto It = linear_search(squares_list.begin(), squares_list.end(), j);
    if(It != squares_list.end()){
      assert(sqrt(j) == trunc(sqrt(j)));
    }
  }
  timer.stop();
  printf("Time to linear search a 10000 element list 100000 times: %f\n", timer.elapsed());
  timer.restart();
*/
  for(uint64_t j = 0; j < 10000; j++){
    auto It = squares_set.find(j);
    if(It != squares_set.end()){
      assert(sqrt(j) == trunc(sqrt(j)));
    }
  }
  timer.stop();
  printf("Time to search a 10000 element set 10000000 times: %f\n", timer.elapsed());
  timer.reset();
  timer.start();
  for(uint64_t j = 0; j < 10000; j++){
    auto It = squares_hash_set.find(j);
    if(It != squares_hash_set.end()){
      assert(sqrt(j) == trunc(sqrt(j)));
    }
  }
  timer.stop();
  printf("Time to search a 10000 element hash set 10000000 times: %f\n", timer.elapsed());
}
int main(){
  test_1();
  test_2();
}

