#ifndef __XORSHIFT_H__
#define __XORSHIFT_H__
#include <stdint.h>
#include <numeric>
#include <array>
#include <limits>
#include <random>
/*
  Xorshift is a family of prng with fairly good distribution and lightning speed.
*/
//Actual functions
namespace util {
using splitmix64_state = uint64_t;
static inline uint64_t splitmix64_next(splitmix64_state &x) {
  uint64_t z = (x += 0x9E3779B97F4A7C15ULL);
  z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
  z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
  return z ^ (z >> 31);
}
using xorshift128_add_state = std::array<uint64_t,2>;
static uint64_t xorshift128_add_next(xorshift128_add_state &state){
    uint64_t x = state[0];
    const uint64_t y = state[1];
    const uint64_t result = x + y;
    state[0] = y;
    x ^= x << 23;
    state[1] = (x ^ y) ^ (x >> 17) ^ (y >> 26);
    return result;
}
//same as 2^64 calls to next.
void xorshift128_add_jump(xorshift128_add_state &state) {
  static const uint64_t JUMP[2] = { 0x8a5cd789635d2dffULL, 0x121fd2155c472f96ULL };
  uint64_t s0 = 0;
  uint64_t s1 = 0;
  for(int i = 0; i < 2; i++){
    for(int b = 0; b < 64; b++) {
      if (JUMP[i] & 1ULL << b) {
        s0 ^= state[0];
        s1 ^= state[1];
      }
;      xorshift128_add_next(state);
    }
  }
  state[0] = s0;
  state[1] = s1;
}
//Bitwise rotate left.
static inline uint64_t rotl(const uint64_t x, int n){
  return (x << n) | (x >> (64 - n));
}
using xoroshiro128_add_state = std::array<uint64_t,2>;
static uint64_t xoroshiro128_add_next(xoroshiro128_add_state &state){
  const uint64_t x = state[0];
  uint64_t y = state[1];
  const uint64_t result = x + y;
  y ^= x;
  state[0] = rotl(x, 55) ^ y ^ (x << 14);
  state[1] = rotl(y, 36);
  return result;
}
//equivalent to calling next 2^64 times,  same code as for xorshift128_add,
//but different constants, could merge the two.
static void xoroshiro128_add_jump(xoroshiro128_add_state& state) {
  static constexpr uint64_t JUMP[2] = { 0xbeac0467eba5facbULL, 0xd86b048b86aa9922ULL };
  uint64_t s0 = 0;
  uint64_t s1 = 0;
  for(int i = 0; i < 2; i++) {
    for(int b = 0; b < 64; b++) {
      if (JUMP[i] & 1ULL << b) {
        s0 ^= state[0];
        s1 ^= state[1];
      }
      xoroshiro128_add_next(state);
    }
  }
  state[0] = s0;
  state[1] = s1;
}
struct xorshift1024_star_state {
  std::array<uint64_t,16> state = {0};
  int idx = 0;
  uint64_t& operator[](size_t idx){
    return state[idx];
  }
  constexpr size_t size(){
    return state.size();
  }
  friend bool operator==(const xorshift1024_star_state &lhs,
                         const xorshift1024_star_state &rhs){
    return (lhs.state == rhs.state) && (lhs.idx == rhs.idx);
  }
  friend bool operator!=(const xorshift1024_star_state &lhs,
                         const xorshift1024_star_state &rhs){
    return !(lhs == rhs);
  }
};
static uint64_t xorshift1024_star_next(xorshift1024_star_state &state){
  const uint64_t x = state[state.idx];
  uint64_t y = state[(state.idx = (state.idx + 1) & 15)];
  y ^= y << 31;
  state[state.idx] = y ^ x ^ (y >> 11) ^ (x >> 30);
  return state[state.idx] * 0x9e3779b97f4a7c13;
}
//equivlent to calling next 2^512 times.
static void xorshift1024_star_jump(xorshift1024_star_state &state) {
  static constexpr uint64_t JUMP[16] = {
    0x84242f96eca9c41dULL, 0xa3c65b8776f96855ULL, 0x5b34a39f070b5837ULL,
    0x4489affce4f31a1eULL, 0x2ffeeb0a48316f40ULL, 0xdc2d9891fe68c022ULL,
    0x3659132bb12fea70ULL, 0xaac17d8efa43cab8ULL, 0xc4cb815590989b13ULL,
    0x5ee975283d71c93bULL, 0x691548c86c1bd540ULL, 0x7910c41d10a1e6a5ULL,
    0x0b5fc64563b3e2a8ULL, 0x047f7684e9fc949dULL, 0xb99181f2d8f685caULL,
    0x284600e3f30e38c3ULL
  };
  uint64_t t[16] = { 0 };
  for(int i = 0; i < 16; i++) {
    for(int b = 0; b < 64; b++) {
      if ((JUMP[i] & 1ULL) << b) {
        for(int j = 0; j < 16; j++) {
          t[j] ^= state[(j + state.idx) & 15];
          xorshift1024_star_next(state);
        }
      }
    }
  }
  for(int j = 0; j < 16; j++) {
    state[(j + state.idx) & 15] = t[j];
  }
}
namespace detail {
template<typename T, uint64_t(*NEXT)(T&), void(*JUMP)(T&)>
struct xorshift_rng {
  typedef uint64_t result_type;
  typedef T state_type;
  //Random permutiation of the 15 hex digits, + and extra a.
  static constexpr uint64_t default_seed = 0x7e4b9f0c83162daa;
  state_type state;
  xorshift_rng(uint64_t seed){
    state[0] = seed;
    for(size_t i = 1; i < state.size(); i++){
      state[i] = splitmix64_next(seed);
    }
  }
  xorshift_rng()
    : xorshift_rng(default_seed) {};

  xorshift_rng(const state_type &state)
    : state{state} {};
  xorshift_rng(std::random_device& rd){
    seed_from_rd(rd);
  }
  void seed_from_rd(std::random_device& rd){
    state[0] = (uint64_t(rd()) << 32) | (uint64_t(rd()));
    state[1] = (uint64_t(rd()) << 32) | (uint64_t(rd()));
    //Don't read more that 128 bytes from the random device to avoid
    //running out of entropy
    if constexpr(state_type().size() > 2){
      uint64_t seed = state[1];
      for(size_t i = 2; i < state.size(); i++){
        state[i] = splitmix64_next(seed);
      }
    }
  }

  static uint64_t next(state_type& state){
    return NEXT(state);
  }
  static void jump(state_type& state){
    return JUMP(state);
  }
  result_type min() const {
    return std::numeric_limits<result_type>::min();
  }
  result_type max() const {
    return std::numeric_limits<result_type>::max();
  }
  state_type& get_state(){
    return state;
  }
  result_type operator()(){
    return next(state);
  }
  friend bool operator==(const xorshift_rng &lhs, const xorshift_rng &rhs){
    return lhs.state == rhs.state;
  }
  friend bool operator!=(const xorshift_rng &lhs, const xorshift_rng &rhs){
    return lhs.state != rhs.state;
  }
};
}
using xoroshiro128_add = detail::xorshift_rng<xoroshiro128_add_state,
                                              xoroshiro128_add_next,
                                              xoroshiro128_add_jump>;
using xorshift128_add = detail::xorshift_rng<xorshift128_add_state,
                                             xorshift128_add_next,
                                             xorshift128_add_jump>;
using xorshift1024_star = detail::xorshift_rng<xorshift1024_star_state,
                                               xorshift1024_star_next,
                                               xorshift1024_star_jump>;
}
#if 0
struct xoroshiro128_add {
  typedef uint64_t result_type;
  typedef std::array<uint64_t, 2> state_type;
  static constexpr state_type default_seed =
    {{0x123456789abcdefUL, 0xfedcba987654321UL}};
  state_type state;
  //Default constructor should probably use a default seed, and have
  //a seperate constructor for seeding from a random device
  xoroshiro128_add() : state{default_seed} {};
  xoroshiro128_add(uint64_t x, uint64_t y) : state{{x,y}} {};
  xoroshiro128_add(uint64_t *s) : state{{s[0],s[1]}} {};
  xoroshiro128_add(state_type s) : state{s} {};
  //If given one number use the bitwise compliment as the other, to avoid
  //the possibility of  the state array being all 0s.
  xoroshiro128_add(uint64_t x) : state{{x, ~x}} {};

  xoroshiro128_add(std::random_device rd){
    seed_from_rd(rd);
  }
  //If state is ever set to all 0s the generator won't work.
  void ensure_nonzero(){
    if(state[0] == 0 && state[1] == 0){
      state[0] = ~0;
    }
  }
  void seed_from_rd(std::random_device &rd){
    state[0] = (uint64_t(rd()) << 32) | (uint64_t(rd()));
    state[1] = (uint64_t(rd()) << 32) | (uint64_t(rd()));
  }
  void seed(){
    std::random_device rd;
    seed_from_rd(rd);
  }
  void seed(uint64_t x){
   return seed(x, x ^ (x >> 26));
  }
  void seed(uint64_t x, uint64_t y){
    state[0] = x;
    state[1] = y;
  }
  //actual random number generation
  static inline uint64_t next(std::array<uint64_t,2> &state){
    uint64_t x = state[0];
    const uint64_t y = state[1];
    state[0] = y;
    x ^= x << 23;
    state[1] = (x ^ y) ^ (x >> 17) ^ (y >> 26);
    return state[1] + y;
  }
  result_type min() const {
    return std::numeric_limits<result_type>::min();
  }
  result_type max() const {
    return std::numeric_limits<result_type>::max();
  }
  state_type& get_state(){
    return state;
  }
  result_type operator()(){
    return next(state);
  }
  friend bool operator==(const xoroshiro128_add &lhs, const xoroshiro128_add &rhs){
    return lhs.state == rhs.state;
  }
  friend bool operator!=(const xoroshiro128_add &lhs, const xoroshiro128_add &rhs){
    return lhs.state != rhs.state;
  }
};
struct xorshift128_add {
  typedef uint64_t result_type;
  typedef std::array<uint64_t, 2> state_type;
  static constexpr state_type default_seed =
    {{0x123456789abcdefUL, 0xfedcba987654321UL}};
  state_type state;
  //Default constructor should probably use a default seed, and have
  //a seperate constructor for seeding from a random device
  xorshift128_add() : state{default_seed} {};
  xorshift128_add(uint64_t x, uint64_t y) : state{{x,y}} {};
  xorshift128_add(uint64_t *s) : state{{s[0],s[1]}} {};
  xorshift128_add(state_type s) : state{s} {};
  //If given one number use the bitwise compliment as the other, to avoid
  //the possibility of  the state array being all 0s.
  xorshift128_add(uint64_t x) : state{{x, ~x}} {};

  xorshift128_add(std::random_device rd){
    seed_from_rd(rd);
  }
  //If state is ever set to all 0s the generator won't work.
  void ensure_nonzero(){
    if(state[0] == 0 && state[1] == 0){
      state[0] = ~0;
    }
  }
  void seed_from_rd(std::random_device &rd){
    state[0] = (uint64_t(rd()) << 32) | (uint64_t(rd()));
    state[1] = (uint64_t(rd()) << 32) | (uint64_t(rd()));
  }
  void seed(){
    std::random_device rd;
    seed_from_rd(rd);
  }
  void seed(uint64_t x){
   return seed(x, x ^ (x >> 26));
  }
  void seed(uint64_t x, uint64_t y){
    state[0] = x;
    state[1] = y;
  }
  //actual random number generation
  static inline uint64_t next(std::array<uint64_t,2> &state){
    uint64_t x = state[0];
    const uint64_t y = state[1];
    state[0] = y;
    x ^= x << 23;
    state[1] = (x ^ y) ^ (x >> 17) ^ (y >> 26);
    return state[1] + y;
  }
  result_type min() const {
    return std::numeric_limits<result_type>::min();
  }
  result_type max() const {
    return std::numeric_limits<result_type>::max();
  }
  result_type operator()(){
    return next(state);
  }
  state_type& get_state(){
    return state;
  }
  friend bool operator==(const xorshift128_add &lhs, const xorshift128_add &rhs){
    return lhs.state == rhs.state;
  }
  friend bool operator!=(const xorshift128_add &lhs, const xorshift128_add &rhs){
    return lhs.state != rhs.state;
  }
};
#endif
#endif /* __XORSHIFT_H__ */

/* Local Variables: */
/* mode: c++ */
/* End: */
