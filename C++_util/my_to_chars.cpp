#ifndef __MY_TO_CHARS_H__
#define __MY_TO_CHARS_H__
namespace util {
//Optimized function for finding the length of v in base B.
//could possibly be optimizd further by turning it into a
//binary seach and eliminating division entirely.
template<typename T>
constexpr int to_chars_len(T v, int B = 10){
  //Check 4 possible lengths per division
  constexpr int B_n[4] = {B, B*B, B*B*B, B*B*B*B};
  int n = 1;
  while(true){
    //Assuming compilier will unroll this
    for(int i = 0; i < 4; i++){
      if(v < B_n[i]){
        return n + i;
      }
    }
    v /= B;
    n += 4;
  }
}
//Convert n to a string in base B. ptr is a pointer to the end
//of the buffer used to store the result. A pointer to the first
//character of the result is returned.
//TODO: Could optimize base 10 further by doing 2 digits at once
//using a lookup table.
template<typename T>
char* my_itoa(T n, char *ptr, int B = 10){
  static constexpr char digits[] = 
    "0123456789abcdefghijklmnopqrstuvwxyz";
  while(n >= B){ //Stopping at B instead of 0 saves a division
    auto rem = n % B;
    *ptr-- = digits[rem];
    n /= B;
  }
  *ptr-- = digits[n];
  return ptr;
}
template<typename T,
         std::enable_if<std::is_integral_v<T>, int> = 0>
int to_chars(T v, char *buf, size_t bufsz, int B = 10){
  int len = to_chars_len(v, B);
  if(len > bufsz){ return -1; }
  my_itoa(v, buf + len-1, B);
  return len;
}
template<typename T,
         std::enable_if<std::is_integral_v<T>, int> = 0>
int to_chars(T v, char *buf, int B = 10){
  int len = to_chars_len(v, B);
  my_itoa(v, buf + len-1, B);
  return len;
}
union double_format {
  double d;
  uint64_t i;
  static constexpr uint64_t exponent_mask = (0x7ffULL << 52);
  static constexpr uint64_t mantissa_mask = ~(0xfffUL << 52);
  static constexpr uint64_t sign_mask = (1ULL << 63);
  static constexpr uint64_t qnan_bit = (1ULL << 51);
  static constexpr int exponent_size = 11;
  static constexpr int mantissa_size = 52;
  static constexpr int exponent_bias = 0x3ff;
  constexpr bool sign() const { return i & sign_mask; }
  constexpr int biased_exponent() const { return (i & exponent_mask) >> 52; }
  constexpr int exponent() const { return biased_exponent() - exponent_bias; }
  constexpr uint64_t mantissa() const { return i & mantissa_mask; }
  //Includes the hidden bit, doesn't check for denormals
  constexpr uint64_t significand() const { 
    return mantissa | 1 << (mantissa_size+1);
  }
  constexpr double as_float() const { return d };
  constexpr uint64_t as_int() const { return i };
  //returns -1 for -∞, 1 for ∞ and 0 for anything else
  //for infinity the exponent = 7ff and the mantissa = 0
  constexpr int is_inf() const {
    return (((i & ~sign_mask) == exponent_mask) ? (sign() ? 1 : -1) : 0);
  }
  //return -1 for signaling NaN, 1 for quiet NaN and 0 for anything else
  constexpr int is_nan() const {
    //this comparison checks for a max exponent and non-zero mantissa
    return (((i & ~sign_mask) > exponent_mask) ? ((i & qnan_bit) ? 1 : -1) : 0);
  }
  constexpr bool is_denormal() const {
    return ((i & ~sign_mask) == (i & mantissa_mask) && (i & mantissa_mask));
  }
  //returns -1  for -0, +1 for +0 and 0 for anything else
  constexpr int is_zero() const {
    return (!(i & ~sign_mask) ? (sign() ? 1 : -1) : 0);
  }
  constexpr bool is_special() const {
    return (is_zero() || is_inf() || is_nan());
  }
};

#endif /* __MY_TO_CHARS_H__ */
/*
  Convert an n digit fixed point number f (0 < f < 1) in base b
  into a string representation in base B.
  n + 1 + ceil(log_b(B-1)) digits of precision are required for
  all the intermediate calculations.
*/
std::string FP_3(fp f, int b, int B, int n){
  int k;
  fp R; //remainder, value we still need to print
  fp U; //???, the value of the current digit
  fp M; //
  std::string F;
  for(k = 0, U = floor(f*B), R = f - U, M = pow(b,-n) / 2;
      R >= M && R <= 1 - M;
      k++, U = floor(R*B), R = R*B - U, M = M*B){
    assert(M == (pow(b, -n)*pow(B, k)) / 2);
    assert(R*pow(B, -k) + sum(i,0,k,F[i]*pow(B,-i)) == f);
    F.push_back(to_digit(U));
  }
  F.push_back(U + (R > 0.5));
  return F;
}
/*
  Print in base B an h-digit integer d in radix b with
  h-n siginficant digits
*/
std::string IP_2(bigint d, int b, int B, int h, int n){
  int k = 1;
  bigint R = d;
  bigint B = pow(b,n);
  bigint S = B;
  while((2*R + M) > 2*S){
    S *= B;
    k++;
  }
  int H = k-1;
  std::string D;
  assert(S == pow(B, H+1));
  for(k = k-1, S = S/B, U = floor(R/S), R = R % S;
      2*R >= M && 2*R <= (2*S) - M;
      k--, S /= B, U = floor(R/S), R %= S){
    D.push_back(to_digit(U));
  }
  D.push_back(U + (2*R > S));
  while(k > 0){
    D.push_back(to_digit(0));
  }
  return D;
}
#include <gmp.h>

/*
  Produces a decmial representation of f, V with digits D
  such that V = sum(i, N, K, D[i] * 10^i)
*/
template<typename T, typename fmt_union>
std::string FPP_3(T f, int *H_ptr, int *N_ptr){
  static constexpr size_t nlimbs =
    (std::numeric_limits<T>::max_exponent / sizeof(mp_limb_t)) +
    (sizeof(T) / sizeof(mp_limb_t)) + 1;
  static constexpr int precision = std::numeric_limits<T>::digits;
  static constexpr B = 10;
  fmt_union v = f;
  mp_limb_t R[nlimbs] = {0};
  mp_limb_t S[nlimbs] = {0};
  mp_limb_t M_lo[nlimbs] = {0};
  mp_limb_t M_hi[nlimbs] = {0};
  int exp = v.exponent();
  auto significand = v.significand();
  int power = exp - precision;
  //This may not work on big-endian systems, since the
  //first word of an mp_limb_t array needs to point to the low order word.
  memcpy(R, &significand, sizeof(significand)); //set R to significand
  S[0] = 1;
  M_lo[0] = 1;
  if(power >= 0){
    //the shift count for mpn_lshift needs to be < mp_bits_per_limb-1, so
    //we can't just shift by 'power'.
    int idx = power / mp_bits_per_limb;
    int cnt = power % mp_bits_per_limb;
    memcpy(R + idx, &significand, sizeof(significand));
    S[0] = 1;
    M_lo[idx] = M_hi[idx] = 1;
    //mpn_lshift also requires a nonzero count
    if(cnt > 0){
      mpn_lshift(R + idx, R + idx, nlimbs - idx, cnt);
      mpn_lshift(M_lo + idx, M_lo + idx, nlimbs - idx, cnt);
      mpn_lshift(M_hi + idx, M_hi + idx, nlimbs - idx, cnt);
    }    
  } else if(power < 0){
    int idx = -power / mp_bits_per_limb;
    int cnt = -power % mp_bits_per_limb;
    memcpy(R, &significand, sizeof(significand));
    M_lo[0] = M_hi[0] = 1;
    S[idx] = 1;
    if(cnt > 0){
      mpn_lshift(S + idx, S + idx, nlimbs - idx, cnt);
    }
  }
  //If f has all 0s in the mantissa shift another digit to
  //account for difference in precision between successor
  //and predecessor
  //This should be folded into the code above eventually
  if(f.mantissa() == 0){
    mpn_lshift(M_hi, M_hi, nlimbs, 1);
    mpn_lshift(R, R, nlimbs, 1);
    mpn_lshift(S, S, nlimbs, 1);
  }
  //Determine the the weight H of the first digit in base B
  int k = 0;
  if(f < 0){
    k = 1 + floor(log10(f));
    //This can definately be optimized, by using the algorithm for
    //fast exponentation if nothing else.
    for(int i = 0; i > k; i--){
      mpn_mul_1(R, R, nlimbs, B);
      mpn_mul_1(M_hi, M_hi, nlimbs, B);
      mpn_mul_1(M_lo, M_lo, nlimbs, B);
    }
  } else {
    //If f is the max value this will just return it rather than overflowing
    T next = nextafter(f, std::numeric_limits<T>::max());
    k = 1 + floor(logB(f/2 + f/2));
    for(int i = 0; i < k; i++){
      mpn_mul_1(S, S, nlimbs, B);
    }
  }
  //Again this can definately be optimized.
  bool low, high;
  mp_limb_t U;
  std::string ret;
  //We could figure this out by keeping track of digits in the above
  //computations, but for now this will work
  //most significant limb of S, mp_tdiv_qr requires the most significant
  //limb of the divisor be non-zero so we need to know this
  mp_size_t max_limb = mpn_sizeinbase(S, nlimbs, 2);
  max_limb = ((max_limb / sizeof(mp_limb_t)) +
              (max_limb % sizeof(mp_limb_t) ? 1 : 0));
  do {
    mpn_mul_1(R, R, nlimbs, B);
    //Set R to R mod S and U to floor(R/S), this may not
    //work as is, I'm not sure if R and S will always have the same
    //number of limbs.
    mpn_tdiv_qr(&U, R, 0, R, max_limb, S, max_limb);
    if(S[max_limb] == 0){
      --max_limb;
    }
    mpn_mul_1(M_hi, M_hi, nlimbs, B);
    mpn_mul_1(M_lo, M_lo, nlimbs, B);
    //Temporally change R and S for comparisions then change them back.
    mpn_lshift(R, R, max_limb, 1);
    mpn_lshift(S, S, max_limb, 1);
    mpn_sub_n(S, S, M_hi, nlimbs);
    //low = (2*R < M_lo), high = (2*R) < (2*S) - M_hi
    low = (mpn_cmp(R, M_hi, nlimbs) < 0);
    high = (mpn_cmp(R, S, nlimbs) > 0);
    mpn_add_n(S, S, M_hi, nlimbs);
    mpn_rshift(R, R, max_limb, 1);
    mpn_rshift(S, S, max_limb, 1);
    k--;
    ret.push_back(U);
  } while(!low && !high);
  //Round the last digit
  if(high){
    if(!low){
      ret.back()++;
    } else {
      mpn_lshift(R, R, nlimbs, 1);
      if(mpn_cmp(R, S, nlimbs) >= 0){
        ret.back()++;
      }
    }
  }
  *H_ptr = H;
  *N_ptr = k;
  return ret;
}

//grisu algorithm
struct diy_fp {
  uint64_t f;
  int e;
  diy_fp(uint64_t f, int e) : f{f}, e{e} {}
  diy_fp(double d){
    double_format tmp = d;
    f = tmp.significand();
    e = tmp.exponent();
  }
  void normalize(){
    //We start with an 53 bit mantissa, so we can always shift 10 bits
    f <<= 10;
    e -= 10;
    while(!(f & (0x1ULL << 63))){
      e--;
    }
    return;
  }    
  static diy_fp subtract(diy_fp x, diy_fp y){
    assert(x.e == y.e && x.f >= y.f);    
    return diy_fp(x.f-y.f, x.e);
  }
  static diy_fp multiply(diy_fp x, diy_fp y){
    //Use 128 bit integers if possible, it simplifies the code
    //and should be faster.
#ifdef (__GNUC__)
    unsigned __int128 tmp = x.f * y.f;
    tmp += (1UL << 63); //round
    return diy_fp((uint64_t)(tmp >> 64), x.e + y.e + 64);
#else
    //Compute the 64 most significant digits of the result
    //of x.f*y.f, with correct rounding.
    static constexpr uint64_t mask = 0xffffffff;
    uint64_t a = x.f >> 32, b = x.f & mask;
    uint64_t b = y.f >> 32, c = y.f & mask;
    uint64_t ac = a * c, ad = a * d;
    uint64_t bc = b * c, bd = b * d; 
    uint64_t tmp = (bd >> 32) + (ad & mask) + (bc & mask);
    tmp += (1U << 31); //round
    tmp = ac +  (ad >> 32) + (bc >> 32) + (tmp >> 32);
    return diy_fp(tmp, x.e + y.e + 64);
#endif
  }
};
//Compute k so that k = ceil(log10(pow(2,α-e+q-1))), with q = 64
//equivalently k = (α - e + q - 1) / lg(10)
int compute_k(int e, int alpha, int gamma){
  static constexpr double d_1_log2_10 = 0.30102999566398114; // 1/lg(10)
  return ceil((alpha - e + 63) * d_1_log2_10);
}
diy_fp cached_power(int k){
  //TODO: Actually build this table, will also likely need to
  //scale k, since it can be negitive.
  return cached_powers[k]; 
}
/*
  grisu1 fast, but produces longer than necessary output.
*/
template<int(*digit_gen)(diy_fp, char *), int alpha, int gamma>
int grisu1(double v, char *buf){
  diy_fp w(v);
  int q = 64, alpha = 0, gamma = 3;
  int mk = compute_k(w.e + q, alpha, gamma);
  diy_fp c_mk = cached_power(mk);
  diy_fp D = diy_fp.multiply(w, c_mk);
  return digit_gen(D, buf);
}
int digit_gen_cut(diy_fp D, char *buf){
  uint32_t parts[3];
  parts[2] = (D.f % (1'000'000 >> D.e)) << D.e;
  parts[1] = D.f % (D.f / (1'000'000 >> D.e));
  parts[0] = D.f / (D.f (1'000'000 >> D.e));
  int nchars = to_chars(parts[0], buf);
  nchars += to_chars(parts[1], buf + nchars);
  nchars += to_chars(parts[2], buf + nchars);
  buf[nchars++] = 'e';
  nchars += to_chars(-mk, buf + nchars);
  return nchars;
}
int digit_gen_mix(diy_fp D, char *buf){
  diy_fp one(1ULL<<-D.e, D.e);
  uint32_t part1 = D.f >> -one.e; //floor(D/one);
  uint64_t f = D.f & (one.f - 1);//part2 = D mod 1
  int nchars = to_chars(part1, buf);
  buf[nchars++] = '.';
  while(nchars < 19){
    f *= 10;
    buf[nchars++] = '0' + (f >> -one.e);//floor(10*D / one)
    f &= one.f - 1; //D = 10*D mod one
  }
  return nchars;
}
int grisu1_simple(double v, char *buf){
  return grisu1<digit_gen_cut, 0, 3>(v,buf);
}
int grisu1_optimized(double v, char *buf){
  return grisu1<digit_gen_mix, -59, -32>(v, buf);
}
inline void compute_boundries(double v, diy_fp &w,
                              diy_fp &m_minus, diy_fp &m_plus){
  bool is_pow_of_2 = !(v & double_format.mantissa_mask);
  w = diy_fp(v);
  m_plus.f = w.f << 1 + 1;
  m_plus.e = w.e - 1;

  m_minus.f = (w.f << (1 - (1 + is_pow_of_2))) - 1;
  m_minus.e = w.e - (1 + is_pow_of_2);

  w.normalize();
  m_minus.normalize();
  m_plus.normalize();
  return;
}
//digit generation for grisu2 & 3
int digit_gen(diy_fp Mp, diy_fp delta,
              char *buf, int *K){
  uint32_t div = 1'000'000'000;
  int kappa = 10;
  diy_fp one(1UL << -Mp.e, Mp.e);
  uint64_t p1 = Mp.f >> -one.e;
  uint64_t p2 = Mp.f & (one.f - 1);
  int len = 0;
  //Find the largest value of kappa where
  //Mp % 10^kappa < delta
  while(kappa > 0){
    int d = p1 / div;
    //d || len check is to skip leading 0s
    if(d || len){ buf[len++] = '0' + d; }
    p1 %= div;
    kappa--;
    div /= 10;   
    if((p << -one.e)+p2 <= delta.f){
      *K += kappa;
      return len;
    }
  }
  do {
    p2 *= 10;
    d = (p2 >> -one.e);
    if(d || len){ buf[len++] = '0' + d; }
    p2 &= (one.f -1);
    kappa--;
    delta.f *= 10;
  } while(p2 > delta.f);
  return len;
}
  
int grisu2(double v, char *buf, int *K){
  const int alpha = -59, gamma = -32, q = 64;
  diy_fp w, m_minus, m_plus;
  compute_boundries(v, w, m_minus, m_plus);
  int mk = compute_k(w.e + q, alpha, gamma);  
  diy_fp c_k = cached_pow(mk);
  diy_fp M_minus = diy_fp.multiply(m_minus, c_k);
  diy_fp M_plus = diy_fp.multiply(m_plus, c_k);
  M_minus.f += 1;
  M_plus.f -= 1;
  diy_fp delta = diy_fp.subtract(M_plus, M_minus);
  return digit_gen(M_plus, delta, buf, K);
}
int grisu3(double v, char *buf, int *K){
  const int alpha = -59, gamma = -32, q = 64;
  diy_fp w, m_minus, m_plus;
  compute_boundries(v, w, m_minus, m_plus);
  int mk = compute_k(w.e + q, alpha, gamma);  
  diy_fp c_k = cached_pow(mk);
  diy_fp M_minus = diy_fp.multiply(m_minus, c_k);
  diy_fp M_plus = diy_fp.multiply(m_plus, c_k);
  M_minus.f += 1;
  M_plus.f -= 1;
}
/*
  I'm not 100% sure how to compute these myself so I'm 
  just using values I've found online.
*/
struct cached_power {
  uint64_t f;
  int16_t e; //binary exponent
  int16_t de; //decimal exponent
};

static constexpr cached_power pow_cache[] = {
  { 0xfa8fd5a0081c0288ULL, -1220, -348 },
  { 0xbaaee17fa23ebf76ULL, -1193, -340 },
  { 0x8b16fb203055ac76ULL, -1166, -332 },
  { 0xcf42894a5dce35eaULL, -1140, -324 },
  { 0x9a6bb0aa55653b2dULL, -1113, -316 },
  { 0xe61acf033d1a45dfULL, -1087, -308 },
  { 0xab70fe17c79ac6caULL, -1060, -300 },
  { 0xff77b1fcbebcdc4fULL, -1034, -292 },
  { 0xbe5691ef416bd60cULL, -1007, -284 },
  { 0x8dd01fad907ffc3cULL,  -980, -276 },
  { 0xd3515c2831559a83ULL,  -954, -268 },
  { 0x9d71ac8fada6c9b5ULL,  -927, -260 },
  { 0xea9c227723ee8bcbULL,  -901, -252 },
  { 0xaecc49914078536dULL,  -874, -244 },
  { 0x823c12795db6ce57ULL,  -847, -236 },
  { 0xc21094364dfb5637ULL,  -821, -228 },
  { 0x9096ea6f3848984fULL,  -794, -220 },
  { 0xd77485cb25823ac7ULL,  -768, -212 },
  { 0xa086cfcd97bf97f4ULL,  -741, -204 },
  { 0xef340a98172aace5ULL,  -715, -196 },
  { 0xb23867fb2a35b28eULL,  -688, -188 },
  { 0x84c8d4dfd2c63f3bULL,  -661, -180 },
  { 0xc5dd44271ad3cdbaULL,  -635, -172 },
  { 0x936b9fcebb25c996ULL,  -608, -164 },
  { 0xdbac6c247d62a584ULL,  -582, -156 },
  { 0xa3ab66580d5fdaf6ULL,  -555, -148 },
  { 0xf3e2f893dec3f126ULL,  -529, -140 },
  { 0xb5b5ada8aaff80b8ULL,  -502, -132 },
  { 0x87625f056c7c4a8bULL,  -475, -124 },
  { 0xc9bcff6034c13053ULL,  -449, -116 },
  { 0x964e858c91ba2655ULL,  -422, -108 },
  { 0xdff9772470297ebdULL,  -396, -100 },
  { 0xa6dfbd9fb8e5b88fULL,  -369,  -92 },
  { 0xf8a95fcf88747d94ULL,  -343,  -84 },
  { 0xb94470938fa89bcfULL,  -316,  -76 },
  { 0x8a08f0f8bf0f156bULL,  -289,  -68 },
  { 0xcdb02555653131b6ULL,  -263,  -60 },
  { 0x993fe2c6d07b7facULL,  -236,  -52 },
  { 0xe45c10c42a2b3b06ULL,  -210,  -44 },
  { 0xaa242499697392d3ULL,  -183,  -36 },
  { 0xfd87b5f28300ca0eULL,  -157,  -28 },
  { 0xbce5086492111aebULL,  -130,  -20 },
  { 0x8cbccc096f5088ccULL,  -103,  -12 },
  { 0xd1b71758e219652cULL,   -77,   -4 },
  { 0x9c40000000000000ULL,   -50,    4 },
  { 0xe8d4a51000000000ULL,   -24,   12 },
  { 0xad78ebc5ac620000ULL,     3,   20 },
  { 0x813f3978f8940984ULL,    30,   28 },
  { 0xc097ce7bc90715b3ULL,    56,   36 },
  { 0x8f7e32ce7bea5c70ULL,    83,   44 },
  { 0xd5d238a4abe98068ULL,   109,   52 },
  { 0x9f4f2726179a2245ULL,   136,   60 },
  { 0xed63a231d4c4fb27ULL,   162,   68 },
  { 0xb0de65388cc8ada8ULL,   189,   76 },
  { 0x83c7088e1aab65dbULL,   216,   84 },
  { 0xc45d1df942711d9aULL,   242,   92 },
  { 0x924d692ca61be758ULL,   269,  100 },
  { 0xda01ee641a708deaULL,   295,  108 },
  { 0xa26da3999aef774aULL,   322,  116 },
  { 0xf209787bb47d6b85ULL,   348,  124 },
  { 0xb454e4a179dd1877ULL,   375,  132 },
  { 0x865b86925b9bc5c2ULL,   402,  140 },
  { 0xc83553c5c8965d3dULL,   428,  148 },
  { 0x952ab45cfa97a0b3ULL,   455,  156 },
  { 0xde469fbd99a05fe3ULL,   481,  164 },
  { 0xa59bc234db398c25ULL,   508,  172 },
  { 0xf6c69a72a3989f5cULL,   534,  180 },
  { 0xb7dcbf5354e9beceULL,   561,  188 },
  { 0x88fcf317f22241e2ULL,   588,  196 },
  { 0xcc20ce9bd35c78a5ULL,   614,  204 },
  { 0x98165af37b2153dfULL,   641,  212 },
  { 0xe2a0b5dc971f303aULL,   667,  220 },
  { 0xa8d9d1535ce3b396ULL,   694,  228 },
  { 0xfb9b7cd9a4a7443cULL,   720,  236 },
  { 0xbb764c4ca7a44410ULL,   747,  244 },
  { 0x8bab8eefb6409c1aULL,   774,  252 },
  { 0xd01fef10a657842cULL,   800,  260 },
  { 0x9b10a4e5e9913129ULL,   827,  268 },
  { 0xe7109bfba19c0c9dULL,   853,  276 },
  { 0xac2820d9623bf429ULL,   880,  284 },
  { 0x80444b5e7aa7cf85ULL,   907,  292 },
  { 0xbf21e44003acdd2dULL,   933,  300 },
  { 0x8e679c2f5e44ff8fULL,   960,  308 },
  { 0xd433179d9c8cb841ULL,   986,  316 },
  { 0x9e19db92b4e31ba9ULL,  1013,  324 },
  { 0xeb96bf6ebadf77d9ULL,  1039,  332 },
  { 0xaf87023b9bf0ee6bULL,  1066,  340 }
};
