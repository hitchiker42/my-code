#ifndef __DYN_GRID_H__
#define __DYN_GRID_H__
#include "svector.h"
#include "grid2D.h" //for Index
namespace util {
template<typename T>
struct dyn_grid2D {
  T *mem = nullptr;
  Index zero_offset;
  int rows = 0;
  int stride = 0;
  int cols = 0;
  int size = 0;
  dyn_grid2D(int starting_size, const T& init = T())
    : size{size} {}
  grid2D() = default;
  ~grid2D() = default;
  friend void swap(grid2D &x, grid2D &y){
    using std::swap;
  }
  constexpr Index min_idx(){
    return -zero_offset;
  }
  constexpr Index max_idx(){
    return {rows-zero_offset.i, cols-zero_offset.j};
  }
  T& operator()(int i, int j){
    return ref(i,j);
  }
  const T& operator()(int i, int j) const {
    return ref(i,j);
  }
  const T& operator()(Index ij) const {
    return ref(ij);
  }
  T& operator()(Index ij) {
    return ref(ij);
  }
  T& ref(int i, int j){
    return vec[i * cols + j];
  }
  T& ref(Index idx){
    return ref(idx.i,idx.j);
  }
  const T& ref(int i, int j) const {
    return vec[i * cols + j];
  }
  const T& ref(Index idx) const {
    return ref(idx.i,idx.j);
  }
  void set(int i, int j, T val){
    vec[i * cols + j] = val;
  }
  void set(Index idx, T val){
    return set(idx.i, idx.j, val);
  }
  bool is_valid_index(int i, int j) const {
    return (i>=0 && i<rows && j>=0 && j<cols);
  }
  bool is_valid_index(Index idx) const {
    return is_valid_index(idx.i,idx.j);
  }
  bool is_edge_index(int i, int j) const {
    return (i==0 || i==(rows-1) || j==0 || j==(cols-1));
  }
  bool is_edge_index(Index idx) const {
    return is_edge_index(idx.i,idx.j);
  }
  T* data(){
    return vec.data();
  }
  T* maybe_ref(int i, int j) const {
    if(is_valid_index(i,j)){
      return &vec[i*cols + j];
    } else {
      return nullptr;
    }
  }
  T* maybe_ref(Index idx) const {
    return maybe_ref(idx.i,idx.j);
  }
  //Makes a copy of the element
  std::optional<T> opt_ref(Index idx) const {
    std::optional<std::reference_wrapper<T>> ret;
    if(is_valid_index(idx)){
      ret.emplace(ref(idx));
    }
    return ret;
  }
  template<typename U>
  int get_neighbors_helper(Index idx, int num_neighbors, U& out) const {
    for(int i = 0; i < num_neighbors; i++){
      Index off = directional_offsets[i];
      if(is_valid_index(idx + off)){
        if constexpr(std::is_same_v<typename U::value_type, Index>){
          out.push_back(idx + off); //could use emplace_back
        } else {
          out.push_back(ref(idx + off)); //could use emplace_back
        }
      }
    }
    return out.size();
  }
  int cardinal_neighbors(Index idx, std::vector<T>& out) const {
    return get_neighbors_helper(idx, 4, out);
  }
  int neighbors(Index idx, std::vector<T>& out) const {
    return get_neighbors_helper(idx, 8, out);
  }
  util::array<T,4> cardinal_neighbors(Index idx) const {
    util::array<T,4> ret;
    get_neighbors_helper(idx, 4, ret);
    return ret;
  }
  util::array<T,8> neighbors(Index idx) const {
    util::array<T,8> ret;
    get_neighbors_helper(idx, 8, ret);
    return ret;
  }
  util::array<Index,4> cardinal_neighbor_indexes(Index idx) const {
    util::array<Index,4> ret;
    get_neighbors_helper(idx, 4, ret);
    return ret;
  }
  util::array<Index,8> neighbors_indexes(Index idx) const {
    util::array<Index,8> ret;
    get_neighbors_helper(idx, 8, ret);
    return ret;
  }
  void fill(T val){
    int i,j;
    for(i=0;i<rows;i++){
      for(j=0;j<cols;j++){
        vec[i*cols + j] = val;
      }
    }
  }
  void map_inplace(std::function<T(T)>& fn){
    for(int i=0;i<rows*cols;i++){
      data[i] = fn(data[i]);
    }
  }
}
}  
#endif /* __DYN_GRID_H__ */
