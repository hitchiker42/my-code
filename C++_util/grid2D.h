#ifndef __GRID2D_H__
#define __GRID2D_H__
#include "svector.h"
#if __cplusplus < 202000L //C++20 for future proofing
template< class T >
struct type_identity {
    using type = T;
};
#else
using std::type_identity;
#endif
namespace util {
//just a pair of ints with some syntaitic sugar
struct Index {
  union {
    int i;
    int row;
    int y;
  };
  union {
    int j;
    int col;
    int x;
  };
  constexpr Index()
    : i{0}, j{0} {};
  constexpr Index(int i, int j)
    : i{i}, j{j} {}
  constexpr Index(std::pair<int,int> p)
    : i{p.first}, j{p.second} {}
  constexpr Index(const Index &other)
    : i{other.i}, j{other.j} {}
  friend Index operator+(Index idx1, Index idx2){
    return {idx1.i+idx2.i,idx1.j+idx2.j};
  }
  friend Index& operator+=(Index &idx1, Index idx2){
    idx1.i += idx2.i; idx1.j += idx2.j; return idx1;
  }
  friend Index operator-(Index idx){
    return {-idx.i,-idx.j};
  }
  friend Index operator-(Index idx1, Index idx2){
    return {idx1.i-idx2.i,idx1.j-idx2.j};
  }
  friend Index& operator-=(Index &idx1, Index idx2){
    idx1.i -= idx2.i; idx1.j -= idx2.j; return idx1;
  }
  friend Index operator*(Index idx1, int scalar){
    return {idx1.i*scalar, idx1.j*scalar};
  }
  friend Index operator*(int scalar, Index idx1){
    return {idx1.i*scalar, idx1.j*scalar};
  }  
  friend Index& operator*=(Index &idx1, int scalar){
    idx1.i *= scalar; idx1.j *= scalar; return idx1;
  }  
  friend bool operator<(Index idx1, Index idx2){
    return (idx1.i == idx2.i ? idx1.j < idx2.j : idx1.i < idx2.i);
  }
  friend bool operator==(Index idx1, Index idx2){
    return((idx1.i == idx2.i) && (idx1.j == idx2.j));
  }
  friend bool operator!=(Index idx1, Index idx2){
    return !(idx1 == idx2);
  }
  friend bool is_is_valid_index(Index idx, int m, int n){
    return (idx.i>=0 && idx.j>=0 && idx.i < m && idx.j < n);
  }
  //Support for structured binding
  template<size_t N>
  constexpr int get() const {
    if constexpr(N == 0){ return i;} else { return j; }
  }
};
}//end namespace util (we need to add some stuff to std)
namespace std {
template<size_t N> struct tuple_element<N,util::Index> {
  using type = int;
};
template<>
struct tuple_size<util::Index> : std::integral_constant<size_t, 2> {};
};
namespace util {
/*
  A structure to represent a two dimensional grid, generally something like a map.
  This is not meant to be used a matrix.
  TODO: Add iterators
*/
inline constexpr std::array<Index,8> directional_offsets = {{
  {-1, 0}, { 1, 0}, {0,-1}, {0, 1},
  {-1,-1}, {-1, 1}, {1,-1}, {1, 1}
}};
inline constexpr std::array<Index,4> cardinal_offsets = {{
  {-1, 0}, { 1, 0}, {0,-1}, {0, 1}
}};
template<typename T>
struct grid2D {
  //first 4 are cardinal direction offsets, next 4 are diagonals.
  enum direction {
    N = 0, UP = N,
    S, DOWN = S,
    W, LEFT = W,
    E, RIGHT = E,
    NW, UP_LEFT = NW,
    NE, UP_RIGHT = NE,
    SW, DOWN_LEFT = SW,
    SE, DOWN_RIGHT = SE
  };
  svector<T> vec;// = svector<T>(0);
  int rows = 0;
  int cols = 0;
  grid2D(int rows, int cols, const T& init = T())
    : vec(rows*cols, init), rows{rows}, cols{cols} {}
  grid2D() = default;
  ~grid2D() = default;
  friend void swap(grid2D &x, grid2D &y){
    using std::swap;
    swap(x.vec, y.vec);
    swap(x.rows, y.rows);
    swap(x.cols, y.cols);
  }
  //returns a pointer to the idx'th row so that X[idx][j] will work
  //as expected.
  T* operator [](int idx){
    return (vec.data() + idx*cols);
  }
  const T* operator [](int idx) const {
    return (vec.data() + idx*cols);
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
#if 0
  //End iterator represented by an iterator with idx = {g.rows, g.cols};
  struct iterator {
    grid2D* g;
    Index idx;
    Index& idx_inc(){
      idx.j++;
      if(idx.j == g->cols){
        idx.i++;
        if(idx.i < g->rows){
          idx.j = 0;
        }
      }
      return idx;
    }
    Index& idx_add(size_t offset){
      while(idx.j + offset >= g->cols){
        offset -= (g->cols - idx.j);
        idx.j = 0;
        idx.i++;
      }
      if(idx.i >= g->rows){
        idx = {g->rows, g->cols};
      } else {
        idx.j = offset;
      }
      return idx;
    }
    iterator& operator++(){
      idx_inc();
      return *this;
    }
    iterator& operator++(int){
      auto ret = *this;
      this->operator++();
      return ret;
    }
    iterator& operator+=(size_t offset){
      idx_add(offset);
      return *this;
    }
    iterator& operator+=(Index idx_offsett){
      idx += idx_offset;
      return *this;
    }
    bool operator==(iterator &other){
      return (g == other.g && idx == other.idx);
    }
    bool operator!=(iterator &other){
      return (g != other.g || idx != other.idx);
    }
    T& operator*(){
      return g->ref(idx);
    }
    T* operator->(){
      return &(g->ref(idx));
    }
    Index index(){
      return idx;
    }
    //Support for structured binding
    template<size_t N,
             std::enable_if_t<N == 0 || N == 1, int> = 0>
    constexpr auto get() const {
      if constexpr(N == 0){
        return g->ref(idx);
      } else {
        return idx;
      }
    }
  }
#endif
};
}
#if 0
namespace std {
template<typename T>
template<size_t N> struct tuple_element<N,typename grid2D<T>::iterator> {
  using type = decltype(std::declval<typename grid2D<T>::iterator>().get<N>());
};
template<typename T>
template<>
struct tuple_size<typename grid2D<T>::iterator> : std::integral_constant<size_t, sz> {};
};
#endif
#endif /* __GRID2D_H__ */
