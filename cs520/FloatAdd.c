#include <stdint.h>//because I like having definite lengths to types
#define NDEBUG
//borrowed from mpfr.h
typedef enum {
  flt_RNDN=0,  /* round to nearest, with ties to even */
  flt_RNDZ,    /* round toward zero */
  flt_RNDU,    /* round toward +Inf */
  flt_RNDD,    /* round toward -Inf */
  flt_RNDA,    /* round away from zero */
  flt_RNDF,    /* faithful rounding (not implemented yet) */
  flt_RNDNA=-1 /* round to nearest, with ties away from zero (mpfr_round) */
} flt_rnd_t;
enum {
  float_tininess_after_rounding  = 0,
  float_tininess_before_rounding = 1
};
//technically you shouldn't name your types <name>_t because those names
//are reserved for future use, but at the same time, it makes things
//much clearer
//globals (i.e the floating point flags register)
//although I'm assuming we don't need to do this
flt_rnd_t float_rounding_mode = flt_RNDN;
uint8_t float_exception_flags = 0;
uint8_t float_detect_tininess = float_tininess_after_rounding;
typedef uint32_t real32_t;
static const real32_t minus_zero=0x80000000;//just sign bit set
static const real32_t plus_zero=0x00;//nothing
static const real32_t plus_inf=0x7f800000;//exp bits set
static const real32_t minus_inf=0xff800000;//exp bits +sign bit set
#define is_zero(flt) ((flt & 0x7fffffff) == 0)
#define is_inf(flt) ((flt & 0x7f) && !(flt & 0x007fffff))
#define is_nan(flt) ((flt & 0x7f) && (flt & 0x007fffff))
//assumed that you already know flt is a nan
#define is_snan(flt)(!(flt & 0x00400000)) 
//NANs have a bunch of representations, so these are just used as literal
//NANs, not for comparsions
static const real32_t QNaN = 0xffffffff;//all bits set
static const real32_t SNaN = 0xffbfffff;//all but first mantissa bit
/*static const real32_t SNaN = {.ieee_nan={.sign=1,.exponent=0xff,
  .quiet_nan=0,.mantissa=MAX_MANTISSA}};*/

//straight out of SoftFloat
 /*----------------------------------------------------------------------------
| Shifts `a' right by the number of bits given in `count'.  If any nonzero
| bits are shifted off, they are ``jammed'' into the least significant bit of
| the result by setting the least significant bit to 1.  The value of `count'
| can be arbitrarily large; in particular, if `count' is greater than 32, the
| result will be either 0 or 1, depending on whether `a' is zero or nonzero.
| The result is stored in the location pointed to by `zPtr'.
*----------------------------------------------------------------------------*/

static inline void shift32RightJamming(uint32_t a, int16_t count,
                                       uint32_t *zPtr ){
  uint32_t z;
  if ( count == 0 ) {
    z = a;
  } else if ( count < 32 ) {
    z = ( a>>count ) | ( ( a<<( ( - count ) & 31 ) ) != 0 );
  } else {
    z = ( a != 0 );
  }
  *zPtr = z;
}
//debugging code??? (not really for debugging, but because you asked to but it in)
/*
#if (defined DEBUG) && !(defined NDEBUG)
#define PRINT_FLOAT_SIGN(flt)                           \
  fprintf(stderr,"the sign bit of the float %#0x is %d",flt,extractFloat32Sign(flt))
#define PRINT_FLOAT_MANTISSA(flt)                       \
  fprintf(stderr,"the mantissa of the float %#0x is %d",flt,extractFloat32Frac(flt))
#define PRINT_FLOAT_EXPONENT(flt)                       \
  fprintf(stderr,"the exponent of the float %#0x is %d",flt,extractFloat32Exp(flt))
#define PRINT_FLOAT_PARTS(flt)                  \
  (PRINT_FLOAT_SIGN(flt),PRINT_FLT_MANTISSA(flt),PRINT_FLT_EXPONENT(flt));
#else
#define PRINT_FLOAT_SIGN(flt)
#define PRINT_FLOAT_MANTISSA(flt)
#define PRINT_FLOAT_EXPONENT(flt)
#define PRINT_FLOAT_PARTS(flt)
#endif
*/
static inline uint32_t extractFloat32Frac(uint32_t a){
  return a & 0x007FFFFF;
}
static inline int16_t extractFloat32Exp(uint32_t a){
  return ( a>>23 ) & 0xFF;
}
static inline int extractFloat32Sign(uint32_t a){
  return a>>31;
}

//since Sig is just added Exp should be one less than desired
//(because of the hidden bit in Sig, which should be 1)
static inline uint32_t packFloat32(int zSign, int16_t zExp, uint32_t zSig){
    return ( ( (uint32_t) zSign )<<31 ) + ( ( (uint32_t) zExp )<<23 ) + zSig;
}
uint32_t round_nearest_even(int zSign, int16_t zExp, uint32_t zSig );
uint32_t normalize_and_round(int zSign, int16_t zExp, uint32_t zSig );
uint32_t propagateFloat32NaN(uint32_t a,uint32_t b){
  if(is_snan(a) || is_snan(b)){
    return SNaN;
  } else {
    return QNaN;
  }
}
static uint32_t  addFloat32Sigs( real32_t a, real32_t b, int zSign ) {
  int16_t aExp, bExp, zExp;
  uint32_t aSig, bSig, zSig;
  int16_t expDiff;

  aSig = extractFloat32Frac(a);
  aExp = extractFloat32Exp(a);
  bSig = extractFloat32Frac(b);
  bExp = extractFloat32Exp(b);
  expDiff = aExp - bExp;

  aSig <<= 6;
  bSig <<= 6;
  //align radix points, if aExp<bExp shift the mantissa
  //of b right by the difference,  otherwise do the oppisite
  if ( 0 < expDiff ) {//aExp>bExp 
    //if expDiff <0 aExp is strictly greater that bExp
    if ( aExp == 0xFF ) {//a is nan or inf
      if(aSig) {return propagateFloat32NaN(a, b);}
      return a;
    }
    if ( bExp == 0 ) {//denormal
      --expDiff;//hidden digit
    } else {//normalize hidden bit?
      bSig |= 0x20000000;//magic? (set the 3rd most significant digit)
    }
    //shift bSig right by expDiff bits, if any nonzero bits are shifted
    //out set the least significant bit of bSig
    shift32RightJamming( bSig, expDiff, &bSig );//magic?
    zExp = aExp;
  } else if ( expDiff < 0 ) {//same as last block but bExp>aExp
    if ( bExp == 0xFF ) {//b is inf(or nan)
      if(bSig){return propagateFloat32NaN(a, b);}
      return b;
    }
    if ( aExp == 0 ) {
      ++expDiff;//a is denormal, compensate for that
    } else {
      aSig |= 0x20000000;//magic?
    }
    shift32RightJamming( aSig, - expDiff, &aSig );//magic?
    zExp = bExp;
  }
  else {//aExp==bExp
    if ( aExp == 0xFF ) {
      if (aSig | bSig) {return propagateFloat32NaN( a, b );}
      return a;
    }
    if (aExp == 0) {
      return packFloat32( zSign, 0, ( aSig + bSig )>>6 );
    }
    
    zSig = 0x40000000 + aSig + bSig;//magic?
    zExp = aExp;
    goto roundAndPack;
  }
  //more magic (add and normalize significand)
  aSig |= 0x20000000;
  zSig = ( aSig + bSig )<<1;
  --zExp;
  if ( (int32_t) zSig < 0 ) {
    zSig = aSig + bSig;
    ++zExp;
  }
 roundAndPack:
  return round_nearest_even(zSign, zExp, zSig );
}
static uint32_t subFloat32Sigs( real32_t a, real32_t b, int zSign ){
  int16_t aExp, bExp, zExp;
  uint32_t aSig, bSig, zSig;
  int16_t expDiff;

  aSig = extractFloat32Frac(a);
  aExp = extractFloat32Exp(a);
  bSig = extractFloat32Frac(b);
  bExp = extractFloat32Exp(b);

  expDiff = aExp - bExp;
  aSig <<= 7;
  bSig <<= 7;
  if ( 0 < expDiff ) {goto aExpBigger;}
  if ( expDiff < 0 ) {goto bExpBigger;}
  if ( aExp == 0xFF ) {//nan/inf
    //signaling vs quiet nans
    if ( aSig | bSig ) return propagateFloat32NaN( a, b );
    //    float_raise( float_flag_invalid );
    return a;
  }
  if ( aExp == 0 ) {
    aExp = 1;
    bExp = 1;
  }
  if ( bSig < aSig ) {goto aBigger;}
  if ( aSig < bSig ) {goto bBigger;}
  //  return packFloat32( float_rounding_mode == float_round_down, 0, 0 );
  return plus_zero;
 bExpBigger:
  if ( bExp == 0xFF ) {
    //    if (bSig) return propagateFloat32NaN( a, b );
    if(bSig){return b;}
    return packFloat32( zSign ^ 1, 0xFF, 0 );
  }
  if ( aExp == 0 ) {
    ++expDiff;
  } else {
    aSig |= 0x40000000;
  }
  shift32RightJamming( aSig, - expDiff, &aSig );
  bSig |= 0x40000000;
 bBigger:
  zSig = bSig - aSig;
  zExp = bExp;
  zSign ^= 1;
  goto normalizeRoundAndPack;
 aExpBigger:
  if ( aExp == 0xFF ) {
    //    if ( aSig ) return propagateFloat32NaN( a, b );
    return a;
  }
  if ( bExp == 0 ) {
    --expDiff;
  } else {
    bSig |= 0x40000000;
  }
  shift32RightJamming( bSig, expDiff, &bSig );
  aSig |= 0x40000000;
 aBigger:
  zSig = aSig - bSig;
  zExp = aExp;
 normalizeRoundAndPack:
  --zExp;
  return normalize_and_round( zSign, zExp, zSig );
}

uint32_t normalize_and_round(int zSign, int16_t zExp, uint32_t zSig ){
  int8_t shiftCount;
  shiftCount = __builtin_clz(zSig) - 1;//I assume countLeadingZeros32 is the same as ffs

  return round_nearest_even(zSign,zExp-shiftCount,zSig<<shiftCount);
}
uint32_t round_nearest_even(int zSign, int16_t zExp, uint32_t zSig){
  uint8_t roundBits,roundIncrement;
  roundBits = zSig & 0x7F;
  roundIncrement = 0x40;
  if ( 0xFD <= (uint16_t) zExp ) {
    if (    ( 0xFD < zExp )
            || (    ( zExp == 0xFD )
                    && ( (int32_t) ( zSig + roundIncrement ) < 0 ) )
            ) {
      //float_raise( float_flag_overflow | float_flag_inexact );
      return packFloat32( zSign, 0xFF, 0 ) - ( roundIncrement == 0 );
    }
    if ( zExp < 0 ) {
      //      isTiny =
      //        (float_detect_tininess == float_tininess_before_rounding)
      //        || ( zExp < -1 )
      //        || ( zSig + roundIncrement < 0x80000000 );
      shift32RightJamming( zSig, - zExp, &zSig );
      zExp = 0;
      roundBits = zSig & 0x7F;
      //      if ( isTiny && roundBits ) float_raise( float_flag_underflow );
    }
  }
  //    if ( roundBits ) float_exception_flags |= float_flag_inexact;
  zSig = ( zSig + roundIncrement )>>7;
  //  zSig &= ~ ( ( ( roundBits ^ 0x40 ) == 0 ) & roundNearestEven );
  zSig &= ~ ( ( ( roundBits ^ 0x40 ) == 0 ) & 1 );
  if ( zSig == 0 ) zExp = 0;
  return packFloat32( zSign, zExp, zSig );
}
int32_t FloatAdd (int32_t a, int32_t b){
  int aSign, bSign;
  aSign=extractFloat32Sign(a);
  bSign=extractFloat32Sign(b);
  uint32_t result;
  if ( aSign == bSign ) {
    result=addFloat32Sigs( a, b, aSign );
    return result;
  } else {
    result=subFloat32Sigs( a, b, aSign );
    return result;
  }
}
#if 0
//template code from the softfloat package, used a a guide on how
//to implement things
/*============================================================================

  This C source file is part of the SoftFloat IEC/IEEE Floating-point Arithmetic
  Package, Release 2b.

  Written by John R. Hauser.  This work was made possible in part by the
  International Computer Science Institute, located at Suite 600, 1947 Center
  Street, Berkeley, California 94704.  Funding was partially provided by the
  National Science Foundation under grant MIP-9311980.  The original version
  of this code was written as part of a project to build a fixed-point vector
  processor in collaboration with the University of California at Berkeley,
  overseen by Profs. Nelson Morgan and John Wawrzynek.  More information
  is available through the Web page `http://www.cs.berkeley.edu/~jhauser/
  arithmetic/SoftFloat.html'.

  THIS SOFTWARE IS DISTRIBUTED AS IS, FOR FREE.  Although reasonable effort has
  been made to avoid it, THIS SOFTWARE MAY CONTAIN FAULTS THAT WILL AT TIMES
  RESULT IN INCORRECT BEHAVIOR.  USE OF THIS SOFTWARE IS RESTRICTED TO PERSONS
  AND ORGANIZATIONS WHO CAN AND WILL TAKE FULL RESPONSIBILITY FOR ALL LOSSES,
  COSTS, OR OTHER PROBLEMS THEY INCUR DUE TO THE SOFTWARE, AND WHO FURTHERMORE
  EFFECTIVELY INDEMNIFY JOHN HAUSER AND THE INTERNATIONAL COMPUTER SCIENCE
  INSTITUTE (possibly via similar legal warning) AGAINST ALL LOSSES, COSTS, OR
  OTHER PROBLEMS INCURRED BY THEIR CUSTOMERS AND CLIENTS DUE TO THE SOFTWARE.

  Derivative works are acceptable, even for commercial purposes, so long as
  (1) the source code for the derivative work includes prominent notice that
  the work is derivative, and (2) the source code includes prominent notice with
  these four paragraphs for those parts of this code that are retained.

  =============================================================================*/
/*----------------------------------------------------------------------------
  | Returns the result of adding the absolute values of the single-precision
  | floating-point values `a' and `b'.  If `zSign' is 1, the sum is negated
  | before being returned.  `zSign' is ignored if the result is a NaN.
  | The addition is performed according to the IEC/IEEE Standard for Binary
  | Floating-Point Arithmetic.
  *----------------------------------------------------------------------------*/

static float32 addFloat32Sigs( float32 a, float32 b, flag zSign ){
  int16 aExp, bExp, zExp;
  bits32 aSig, bSig, zSig;
  int16 expDiff;

  aSig = extractFloat32Frac( a );
  aExp = extractFloat32Exp( a );
  bSig = extractFloat32Frac( b );
  bExp = extractFloat32Exp( b );
  expDiff = aExp - bExp;
  aSig <<= 6;
  bSig <<= 6;
  if ( 0 < expDiff ) {
    if ( aExp == 0xFF ) {
      if ( aSig ) return propagateFloat32NaN( a, b );
      return a;
    }
    if ( bExp == 0 ) {
      --expDiff;
    }
    else {
      bSig |= 0x20000000;
    }
    shift32RightJamming( bSig, expDiff, &bSig );
    zExp = aExp;
  }
  else if ( expDiff < 0 ) {
    if ( bExp == 0xFF ) {
      if ( bSig ) return propagateFloat32NaN( a, b );
      return packFloat32( zSign, 0xFF, 0 );
    }
    if ( aExp == 0 ) {
      ++expDiff;
    }
    else {
      aSig |= 0x20000000;
    }
    shift32RightJamming( aSig, - expDiff, &aSig );
    zExp = bExp;
  }
  else {
    if ( aExp == 0xFF ) {
      if ( aSig | bSig ) return propagateFloat32NaN( a, b );
      return a;
    }
    if ( aExp == 0 ) return packFloat32( zSign, 0, ( aSig + bSig )>>6 );
    zSig = 0x40000000 + aSig + bSig;
    zExp = aExp;
    goto roundAndPack;
  }
  aSig |= 0x20000000;
  zSig = ( aSig + bSig )<<1;
  --zExp;
  if ( (sbits32) zSig < 0 ) {
    zSig = aSig + bSig;
    ++zExp;
  }
 roundAndPack:
  return roundAndPackFloat32( zSign, zExp, zSig );

}

/*----------------------------------------------------------------------------
  | Returns the result of subtracting the absolute values of the single-
  | precision floating-point values `a' and `b'.  If `zSign' is 1, the
  | difference is negated before being returned.  `zSign' is ignored if the
  | result is a NaN.  The subtraction is performed according to the IEC/IEEE
  | Standard for Binary Floating-Point Arithmetic.
  *----------------------------------------------------------------------------*/

static float32 subFloat32Sigs( float32 a, float32 b, flag zSign ){
  int16 aExp, bExp, zExp;
  bits32 aSig, bSig, zSig;
  int16 expDiff;

  aSig = extractFloat32Frac( a );
  aExp = extractFloat32Exp( a );
  bSig = extractFloat32Frac( b );
  bExp = extractFloat32Exp( b );
  expDiff = aExp - bExp;
  aSig <<= 7;
  bSig <<= 7;
  if ( 0 < expDiff ) goto aExpBigger;
  if ( expDiff < 0 ) goto bExpBigger;
  if ( aExp == 0xFF ) {
    if ( aSig | bSig ) return propagateFloat32NaN( a, b );
    float_raise( float_flag_invalid );
    return float32_default_nan;
  }
  if ( aExp == 0 ) {
    aExp = 1;
    bExp = 1;
  }
  if ( bSig < aSig ) goto aBigger;
  if ( aSig < bSig ) goto bBigger;
  return packFloat32( float_rounding_mode == float_round_down, 0, 0 );
 bExpBigger:
  if ( bExp == 0xFF ) {
    if ( bSig ) return propagateFloat32NaN( a, b );
    return packFloat32( zSign ^ 1, 0xFF, 0 );
  }
  if ( aExp == 0 ) {
    ++expDiff;
  }
  else {
    aSig |= 0x40000000;
  }
  shift32RightJamming( aSig, - expDiff, &aSig );
  bSig |= 0x40000000;
 bBigger:
  zSig = bSig - aSig;
  zExp = bExp;
  zSign ^= 1;
  goto normalizeRoundAndPack;
 aExpBigger:
  if ( aExp == 0xFF ) {
    if ( aSig ) return propagateFloat32NaN( a, b );
    return a;
  }
  if ( bExp == 0 ) {
    --expDiff;
  }
  else {
    bSig |= 0x40000000;
  }
  shift32RightJamming( bSig, expDiff, &bSig );
  aSig |= 0x40000000;
 aBigger:
  zSig = aSig - bSig;
  zExp = aExp;
 normalizeRoundAndPack:
  --zExp;
  return normalizeRoundAndPackFloat32( zSign, zExp, zSig );

}

/*----------------------------------------------------------------------------
  | Returns the result of adding the single-precision floating-point values `a'
  | and `b'.  The operation is performed according to the IEC/IEEE Standard for
  | Binary Floating-Point Arithmetic.
  *----------------------------------------------------------------------------*/

float32 float32_add( float32 a, float32 b ){
  flag aSign, bSign;

  aSign = extractFloat32Sign( a );
  bSign = extractFloat32Sign( b );
  if ( aSign == bSign ) {
    return addFloat32Sigs( a, b, aSign );
  }
  else {
    return subFloat32Sigs( a, b, aSign );
  }

}
/*----------------------------------------------------------------------------
  | Takes an abstract floating-point value having sign `zSign', exponent `zExp',
  | and significand `zSig', and returns the proper single-precision floating-
  | point value corresponding to the abstract input.  Ordinarily, the abstract
  | value is simply rounded and packed into the single-precision format, with
  | the inexact exception raised if the abstract input cannot be represented
  | exactly.  However, if the abstract value is too large, the overflow and
  | inexact exceptions are raised and an infinity or maximal finite value is
  | returned.  If the abstract value is too small, the input value is rounded to
  | a subnormal number, and the underflow and inexact exceptions are raised if
  | the abstract input cannot be represented exactly as a subnormal single-
  | precision floating-point number.
  |     The input significand `zSig' has its binary point between bits 30
  | and 29, which is 7 bits to the left of the usual location.  This shifted
  | significand must be normalized or smaller.  If `zSig' is not normalized,
  | `zExp' must be 0; in that case, the result returned is a subnormal number,
  | and it must not require rounding.  In the usual case that `zSig' is
  | normalized, `zExp' must be 1 less than the ``true'' floating-point exponent.
  | The handling of underflow and overflow follows the IEC/IEEE Standard for
  | Binary Floating-Point Arithmetic.
  *----------------------------------------------------------------------------*/

static float32 roundAndPackFloat32( flag zSign, int16 zExp, bits32 zSig )
{
  int8 roundingMode;
  flag roundNearestEven;
  int8 roundIncrement, roundBits;
  flag isTiny;

  roundingMode = float_rounding_mode;
  roundNearestEven = roundingMode == float_round_nearest_even;
  roundIncrement = 0x40;
  if ( ! roundNearestEven ) {
    if ( roundingMode == float_round_to_zero ) {
      roundIncrement = 0;
    }
    else {
      roundIncrement = 0x7F;
      if ( zSign ) {
        if ( roundingMode == float_round_up ) roundIncrement = 0;
      }
      else {
        if ( roundingMode == float_round_down ) roundIncrement = 0;
      }
    }
  }
  roundBits = zSig & 0x7F;
  if ( 0xFD <= (bits16) zExp ) {
    if (    ( 0xFD < zExp )
            || (    ( zExp == 0xFD )
                    && ( (sbits32) ( zSig + roundIncrement ) < 0 ) )
            ) {
      float_raise( float_flag_overflow | float_flag_inexact );
      return packFloat32( zSign, 0xFF, 0 ) - ( roundIncrement == 0 );
    }
    if ( zExp < 0 ) {
      isTiny =
        ( float_detect_tininess == float_tininess_before_rounding )
        || ( zExp < -1 )
        || ( zSig + roundIncrement < 0x80000000 );
      shift32RightJamming( zSig, - zExp, &zSig );
      zExp = 0;
      roundBits = zSig & 0x7F;
      if ( isTiny && roundBits ) float_raise( float_flag_underflow );
    }
  }
  if ( roundBits ) float_exception_flags |= float_flag_inexact;
  zSig = ( zSig + roundIncrement )>>7;
  zSig &= ~ ( ( ( roundBits ^ 0x40 ) == 0 ) & roundNearestEven );
  if ( zSig == 0 ) zExp = 0;
  return packFloat32( zSign, zExp, zSig );
}
#endif
#if 0
//An attempt to write things using bitfields, before realizing
//that that's kinda dumb since things need to be shifted out
//of the mantissa and into the significant, and using
//bitfields makes that impossible
//below is code straight out of ieee754.h, but modified
//also ieee754.h is glibc specific so actually including
//the code solves portably problems
//#include <ieee754.h>
/* Copyright (C) 1992-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _IEEE754_H

#define _IEEE754_H 1
#include <features.h>
#include <stdint.h>
#include <endian.h>

__BEGIN_DECLS

union ieee754_float
  {
    float f;
    uint32_t bits32;
    /* This is the IEEE 754 single-precision format.  */
    struct
      {
#if	__BYTE_ORDER == __BIG_ENDIAN
	unsigned int negative:1;
	unsigned int exponent:8;
	unsigned int mantissa:23;
#endif				/* Big endian.  */
#if	__BYTE_ORDER == __LITTLE_ENDIAN
	unsigned int mantissa:23;
	unsigned int exponent:8;
	unsigned int negative:1;
#endif				/* Little endian.  */
      } ieee;

    /* This format makes it easier to see if a NaN is a signalling NaN.  */
    struct
      {
#if	__BYTE_ORDER == __BIG_ENDIAN
	unsigned int negative:1;
	unsigned int exponent:8;
	unsigned int quiet_nan:1;
	unsigned int mantissa:22;
#endif				/* Big endian.  */
#if	__BYTE_ORDER == __LITTLE_ENDIAN
	unsigned int mantissa:22;
	unsigned int quiet_nan:1;
	unsigned int exponent:8;
	unsigned int negative:1;
#endif				/* Little endian.  */
      } ieee_nan;
  };

#define IEEE754_FLOAT_BIAS	0x7f /* Added to exponent.  */

#endif
typedef union ieee754_float ieee32_t;
static const ieee32_t pos_inf={.ieee={.negative=0,.exponent=0xff,.mantissa=0x0}};
static const ieee32_t neg_inf={.ieee={.negative=1,.exponent=0xff,.mantissa=0x0}};
static const ieee32_t SNan={.ieee_nan={.negative=1,.exponent=0xff,.quiet_nan=0,.mantissa=0xffffff}};
static const ieee32_t QNan={.bits32=0xffffffff};
static const ieee32_t pos_zero={.bits32=0x0};
static const ieee32_t neg_zero={.bits32=0x80000000};
uint32_t returnNaN(ieee32_t a,ieee32_t b){
  if(a.ieee_nan.quiet_nan && b.ieee_nan.quiet_nan){
    return QNaN.bits32;
  } else {
    return SNaN.bits32;
  }
}
/*
void shift_mantissa(ieee32_t *a,int shift_cnt){
  if(!shift_cnt){
    return;
  } else if 
uint32_t addFloat32Sigs_bitfield(uint32_t a_bits,uint32_t b_bits){
  ieee32_t a={.bits32=a_bits};
  ieee32_t b={.bits32=b_bits};
  int16_t expDiff=a.ieee.exponent - b.ieee.exponent;
  if(expDiff > 0){
    if(a.ieee.exponent==0xFF){
      if(a.ieee.mantissa){return returnNaN(a,b);}//a is nan
      return a;//a is inf
    }
    if(!b.ieee.exponent){
      --expDIff;
    } else {      
    }
  }
         }*/
#endif
