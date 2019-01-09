#ifndef __AUTO_VECTOR_H__
#define __AUTO_VECTOR_H__
/*
  Dynamic vector which works like pythons list in terms of indexing,
  negitive indexes start from the end of the vector and if 
  and index is greater or equal to the length than vector is
  automatically resized.
*/
template<typename T>
struct auto_vector : std::vector<T> {
  using index_type = std::make_signed_t<size_t>;
  T& operator[](size_t idx){
    return this->operator[]((index_type)idx);
  }
  T& operator[](ssize_t idx){
    if(idx < 0){
      do {
        idx = (this->size() + idx);
      } while(idx < 0);
    } else if(idx >= this->size()){
      this->resize(idx + 1);
    }
    return std::vector::operator[]((size_t)idx);
  }
};
/*
struct line {
  std::vector<int8_t> negitive;
  std::vector<int8_t> positive;
  int8_t zero;
  line(const std::string_view& init){
    positive.reserve(init.size() + 40);
    negitive.reserve(40);
    zero = (init[0] == '#' ? 1 : 0);
    for(int i = 1; i < init.size(); i++){      
      positive.push_back(init[i] == '#' ? 1 : 0);
    }
  }
  size_t total_size(){ 
   return negitive.size() + positive.size() + 1;
  }
  //returns indexes of lowest and highest points
  std::pair<ssize_t,ssize_t> range(){
    ssize_t low = -negitive.size();
    return {-negitive.size(), positive.size()};
  }
  void resize(ssize_t low, ssize_t high){
    negitive.resize(abs(low));
    positive.resize(high);
  }
  int8_t& do_index(std::vector<int8_t>& v, size_t idx){
    if(idx >= v.size()){
      v.resize(idx+1, 0);
    }
    return v[idx];
  }
  int8_t& operator[](ssize_t idx){
    if(idx < 0){
      return do_index(negitive, abs(idx+1));
    } else if(idx > 0){
      return do_index(positive, idx-1);
    } else {
      return zero;
    }
  }
};
*/
#endif /* __AUTO_VECTOR_H__ */
