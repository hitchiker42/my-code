#include <stdlib.h>
#include <stdio.h>
#include <type_traits>
#include <string>
#include <string.h>
#include <stdint.h>
#include <iterator>
#include <bitset>
#include <array>
#include <assert.h>
#include <tuple>
static constexpr size_t fnv_prime_64 = 1099511628211UL;
static constexpr size_t fnv_offset_basis_64 = 14695981039346656037UL;
static constexpr uint64_t fnv_hash(const void *key, size_t keylen){
  const uint8_t *raw_data = static_cast<const uint8_t*>(key);
  uint64_t hash = fnv_offset_basis_64;
  for(size_t i = 0; i < keylen; i++){
    hash = (hash ^ raw_data[i])*fnv_prime_64;
  }
  return hash;
}
static constexpr size_t constexpr_strlen(const char *s, size_t len = 0){
  return *s ? constexpr_strlen(s+1,len+1) : len;
}
/*
  Produces a unique value for each arithmetic type, intended to be used in
  hash functions so that two identical bit sequences of differing types will
  hash to different values.

  There's no real logic to this, I start with the size, then add different
  values for signed/unsigned and float/int types, as well as a special
  value for the char type.
*/
template<typename T, std::enable_if_t<std::is_arithmetic_v<T>, int> = 0>
static constexpr uint64_t unique_type_value(){
  uint64_t ret = sizeof(T);
  ret <<= 8;
  if constexpr(std::is_signed_v<T>){
    ret |= 0x77;
  } else {
    ret |= 0xcc;
  }
  ret <<= 8;
  if constexpr(std::is_integral_v<T>){
    ret |= 0x66;
  } else {
    ret |= 0x99;
  }
  ret <<= 8;
  //char is distinct from both signed and unsigned char.
  if constexpr(std::is_same_v<T,char>){
    ret |= 0xff;
  }
  ret <<= 8;
  //just add some more bits
  ret |= (~ret)<<32;
  return ret;
}
//these could easily be adapted into template functions parametrized
//on a hash function taking a void ptr and a size.
//Arithmetic type
template<typename T, std::enable_if_t<std::is_arithmetic_v<T>, int> = 0>
static constexpr uint64_t fnv_hash(const T x){
  //Mix in unique type value here.
  return fnv_hash(static_cast<const void*>(&x), sizeof(x));
}
//NULL terminated strings
/*
static constexpr uint64_t fnv_hash(const char *str){
  return fnv_hash(static_cast<const void*>(str), constexpr_strlen(str));
}
*/
static constexpr uint64_t fnv_hash(const char *str){
  const size_t keylen = constexpr_strlen(str);
  uint64_t hash = fnv_offset_basis_64;
  for(size_t i = 0; i < keylen; i++){
    hash = (hash ^ uint8_t(str[i]))*fnv_prime_64;
  }
  return hash;
}
//string_views
static constexpr uint64_t fnv_hash(const std::string_view sv){
  return fnv_hash(static_cast<const void*>(sv.data()), sv.size());
}
//fix sized arrays.
template<typename T, size_t N>
static constexpr uint64_t fnv_hash(const T arr[N]){  
  return fnv_hash(static_cast<const void*>(arr), sizeof(T)*N);
}
template<size_t N>
constexpr size_t find_first_unset(const std::bitset<N> &bs, const size_t idx){
  return (bs[idx] ? find_first_unset(bs, idx+1) : idx);
}
/*
  Immutable hash table which can be initalized at compile time.
  -It uses open adressing (with just linear chanining since I'm lazy).
  -It completely forbids collisions, so in the rare case where you get 
   a collision just pick a different hash function. Ideally we'd use a 
   perfect hash function, but creating one of those is a lot of work, so
   this is a compromise, it's a bit more work than just comparing values
   but is faster at runtime.
  
  It's a bit clunky but you need to give the size of the hash table as
  a template parameter in order to statically allocate memory.
*/
//template <typename T>
//using hash_fn = constexpr size_t(*)(const T);
template<class Key, class Value,
//         hash_fn hash,
         //You can't pass a double as a template parameter so we use 
         //Max_Entries instead of a load factor.
         size_t N, size_t Max_Entries = N/4,
         std::enable_if_t<std::is_trivially_copyable_v<Key> &&
                          std::is_trivially_copyable_v<Value>, int> = 0>
struct constexpr_hashtable {
  //union with both key/value and first/second to allow using same 
  //fields as a pair, or fields with more descriptive names.
  struct hentry {
    union {
       Key key;
       Key first;
    };
    union {
       Value value;
       Value second;
    };
    size_t hv;
    constexpr hentry() : key(), value(), hv{0} {};
    constexpr hentry(const Key k, const Value v) 
      : key{k}, value{v}, hv{fnv_hash(k)} {};
    constexpr hentry(const Key k, const Value v, const size_t hv) 
      : key{k}, value{v}, hv{hv} {};
  };
  struct iterator {
    typedef std::ptrdiff_t difference_type;
    typedef hentry value_type;
    typedef value_type* pointer;
    typedef value_type& reference;
    typedef std::bidirectional_iterator_tag iterator_catagory;
    //TODO: See if it would be better to just include a pointer to the hash table,
    //it would require more indirection but also less time to copy.
    const hentry *table;
    const uint8_t *entries_used;
    size_t idx;
    //may not actually point to a valid entry if idx is not explicitly given.
    constexpr explicit iterator(const constexpr_hashtable* ht, size_t idx = 0) 
      : table{ht->table.data()}, entries_used{ht->entries_used.data()}, idx{idx} {};
    constexpr iterator(const hentry *table, 
                       const uint8_t *entries_used, size_t idx)
      : table{table}, entries_used{entries_used}, idx{idx} {};
    //Could potentially optimize increment/decrement operators by using
    //information about the first and last elements.
    iterator operator++(){
      do {
        ++idx;
      } while(!entries_used[idx] && idx < N);
      return (*this);
    }
    iterator operator--(){
      do {
        --idx;
      } while(!entries_used[idx] && idx >= 0);
      return (*this);
    }
    iterator operator++(int){
      auto ret = *this;
      operator++();
      return ret;
    }
    iterator operator--(int){
      auto ret = *this;
      operator--();
      return ret;
    }
    const hentry& operator*(){
      return table[idx];
    }
    const hentry* operator->(){
      return table + idx;
    }
    friend bool operator==(const iterator &lhs, const iterator &rhs){
      return (lhs.idx == rhs.idx) && (lhs.table == rhs.table);
    }
    friend bool operator!=(const iterator &lhs, const iterator &rhs){
      return !operator==(lhs,rhs);
    }
  };
  const std::array<hentry, N> table;
  const std::array<uint8_t, N> entries_used;
  const size_t nentries;// = 0;
  const size_t first_entry;// = 0;
  const size_t last_entry;// = (N-1);
  /*
    We want all the fields to be const (not that it really matters since any
    instance is almost certainly going to be const itself), so we create
    the table in the following function which returns a tuple, we can use this
    to initialize the fields in the member initializiter list allowing the
    fields to be const.
  */
  static constexpr 
  std::tuple<std::array<hentry,N>,std::array<uint8_t,N>, size_t, size_t, size_t>
  build_table(const std::initializer_list<std::pair<Key,Value>> ls) {
    std::array<hentry, N> table = {{}};
    std::array<uint8_t, N> entries_used = {{}};
    size_t nentries = 0;
    size_t first_entry = 0;
    size_t last_entry = (N-1);
    assert(nentries <= Max_Entries);
    for(const auto kv : ls){
      const Key key = kv.first;
      const Value value = kv.second;
      const size_t hv = fnv_hash(key);
      size_t idx = hv % N;
      while(entries_used[idx]){
        assert(hv != table[idx].hv);
        //"Error, duplicate hash code, either dupliate key or hash collision");

        ++idx;
      }
      table[idx] = hentry(key,value,hv);
      entries_used[idx] = 1;
    }
    while(!entries_used[first_entry]){
      first_entry++;
    }
    while(!entries_used[last_entry]){
      last_entry--;
    }
    return std::make_tuple(table, entries_used, nentries, first_entry, last_entry);
  }
  constexpr constexpr_hashtable(std::tuple<std::array<hentry,N>,std::array<uint8_t,N>,
                                           size_t,size_t,size_t> args) 
    : table{std::get<0>(args)}, entries_used{std::get<1>(args)},
      nentries{std::get<2>(args)}, first_entry{std::get<3>(args)}, 
      last_entry{std::get<4>(args)} {}
  constexpr constexpr_hashtable(const std::initializer_list<std::pair<Key,Value>> ls)
    : constexpr_hashtable(build_table(std::forward<decltype(ls)>(ls))) {}

  constexpr size_t find_hv_idx(const size_t hv, const size_t idx) const {
    return (table[idx].hv == hv ? idx : find_hv_idx(hv, idx));
  }
  //Caling with a key that's not in the table invokes undefined behavior,
  //in practice it usually will go into an infinite loop.
  constexpr Value operator[](const Key &k) const {
    const size_t hv = fnv_hash(k);
    const size_t idx = find_hv_idx(hv, hv % N);
    return table[idx].value;
  }
  constexpr iterator find(const Key &k) const {
    const size_t hv = fnv_hash(k);
    size_t idx = hv % N;
    while(table[idx].hv != hv && entries_used[idx]){ ++idx; }
    if(entries_used[idx]){
      return iterator(this, idx);
    } else {
      return end();
    }
  } 
  const iterator begin() const {
    return iterator(this, first_entry);
  }
  const iterator end() const {
    return iterator(this, N);
  }
  const iterator rbegin() const {
    return iterator(this, last_entry);
  }
  const iterator rend() const {
    return iterator(this, -1);
  }
  constexpr size_t size() const {
    return nentries;
  }
};

extern constexpr constexpr_hashtable<const char *, int, 20>
test_hash = {{"a",1},{"b",2},{"c",3}};

int main(){
  for(auto it : test_hash){    
    printf("Value of \"%s\" in hashtable is %d\n", it.first, it.second);
  }
}
