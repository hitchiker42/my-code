#ifndef __MY_TO_CHARS_H__
#define __MY_TO_CHARS_H__
#define DOUBLE_BIAS	0x3ff /* Added to exponent.  */
union double_format {
  double d;
  uint64_t i;
  static constexpr uint64_t exponent_mask = (0x7ffULL << 52);
  static constexpr uint64_t mantissa_mask = ~(0xfffUL << 52);
  static constexpr uint64_t sign_mask = (1ULL << 63);
  static constexpr uint64_t qnan_bit = (1ULL << 51);
  constexpr bool sign() const { return i & sign_mask; }
  constexpr int biased_exponent() const { return (i & exponent_mask) >> 52; }
  constexpr int exponent() const { return biased_exponent() - DOUBLE_BIAS; }
  constexpr uint64_t mantissa() const { return i & mantissa_mask; }
  //returns -1 for -∞, 1 for ∞ and 0 for anything else
  //for infinity the exponent = 7ff and the mantissa = 0
  int is_inf(){
    return (((i & ~sign_mask) == exponent_mask) ? (sign() ? 1 : -1) : 0);
  }
  //return -1 for signaling NaN, 1 for quiet NaN and 0 for anything else
  int is_nan(){
    //this comparison checks for a max exponent and non-zero mantissa
    return (((i & ~sign_mask) > exponent_mask) ? ((i & qnan_bit) ? 1 : -1) : 0);
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
    F.push_back(U);
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
