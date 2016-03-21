#include "util_string.h"
#include "C_util.h"
/*
  Boyer-Moore string search
*/
/*
  Generate a table containing, for each character in the alphabet,
  the distance from the end of the pattern to the rightmost occurance of
  that character in the pattern. If the charater does not appear this will
  be the length of the pattern;

  This table is used to implement the "Bad Character Rule".
*/
static int32_t* gen_delta1_table(uint8_t *pat, int32_t patlen,
                                 int32_t *delta){
  int i=0;
  delta = (delta ? delta : xmalloc(0x100*sizeof(int32_t)));
  while(i<0x100){
    delta[i++] = patlen;
    delta[i++] = patlen;
    delta[i++] = patlen;
    delta[i++] = patlen;
  }
  for(i=0;i<patlen;i++){
    delta[pat[i]] = patlen - (i+1);
  }
  return delta;
}
/*
  is word[pos..len] equal to word[0..(len-pos)]
*/
static int is_suffix_a_prefix(uint8_t *word, int32_t wordlen, int pos){
  int len = wordlen-pos;
  return !(memcmp(word, word+pos, len));
}
/*
  Determine the largest possible i such that:
    word[(pos-i)..pos] == word[(wordlen-(i+1))..(wordlen-1)].
  I.e find the length of the longest substring of word than 
  ends at word[pos], than is also a suffix of word.
*/
static int suffix_length(uint8_t *word, int32_t wordlen, int pos){
  int i;
  for(i=0; (word[pos-i] == word[wordlen-(i+1)]) && i < pos; i++);
  return i;
}
/*
  Generate a table used to shift the pattern when there is a mismatch
  at pat[pos], which use information about the matching substring
  pat[pos+1..patlen-1]. There are two cases for this table.

  The first case is for the case where pat[pos+1..patlen-1] doesn't appear
  elsewhere in the string. In this case we store the longest prefix of pat
  which is a suffix of  pat[pos+1..patlen-1]. 
  This might be the empty string, in which case we store 0.
  This first cas is very similar to the table used in the knuth-morris-prat
  string searching algorithm.

  The second case is for the case where pat[pos+1..patlen-1] does occur
  elsewhere in the string.

  This table is used to implement the "Good Suffix Rule".
*/
static int32_t* gen_delta2_table(uint8_t *pat, int32_t patlen,
                                 int32_t *delta2){
  int p, last_prefix;
  delta2 = (delta2 ? delta2 : xmalloc(patlen*sizeof(int32_t)));
  // Case 1
  for(p = patlen; p > 0; p--){
    //does pat[0..(patlen-p)] equal pat[p..patlen]
    if(is_suffix_a_prefix(pat, patlen, p)){
      last_prefix = p;
    }
    delta2[p-1] = last_prefix + (patlen - p);
  }
  //Case 2
  for(p = 0; p < patlen-1; p++){
    int suflen = suffix_length(pat, patlen, p);
    /*
      suflen is the length of the longest substring in pat which ends on p,
      and is also a suffix of pat. Next we need to make sure that the 
      the substring of length suflen+1 is not a suffix of pat, or else
      we would have the same pattern that we already know doesn't match.
     */
    if(pat[p-suflen] != pat[patlen - (suflen+1)]){
      delta2[patlen - (suflen+1)] = patlen - (p + suflen+1);
    }
  }
  return delta2;
}
uint8_t *boyer_moore(uint8_t *str, uint32_t len, 
                     uint8_t *pat, uint32_t patlen){
/*
  A few special cases
*/
  if(patlen == 0){
    return str;//empty pattern
  } else if(patlen > len){
    return NULL;
  } else if(patlen == 1){
    return memchr(str, pat[0], len);
  }

  int i, align = patlen - 1;
  int previous_align = -1;//used for galil's rule
  
  //Allocate delta1 on the stack and delta2 on the heap
  int32_t *delta1 = alloca(0x100*sizeof(int32_t));
  gen_delta1_table(pat, patlen, delta1);

  int32_t *delta2 = gen_delta2_table(pat, patlen, NULL);
  
  
  while(align < len){
    int i = align;
    int j = patlen - 1;
    //basic string search, except searching backwards
    while(j >= 0 && i > previous_align && str[i] == pat[j]){
      i--;
      j--;
    }
    if(j < 0 || i == previous_align){
      free(delta2);
      return str + (align - (patlen-1));
    }
    int shift = MAX(delta1[str[i]], delta2[j]);
    if(shift > i+1){
      previous_align = align;
    }
    align += shift;
  }
  free(delta2);
  return NULL;
}
uint8_t **boyer_moore_all(uint8_t *str, uint32_t len, 
                          uint8_t *pat, uint32_t patlen, int *n_matches){
  svector matches = make_svector(10);
  while(1){
    uint8_t *match = boyer_moore(str, len, pat, patlen);
    if(!match){
      *n_matches = svector_len(matches);
      return (uint8_t**)svector_data(matches);
    } else {
      SVECTOR_PUSH(match, matches);
      str = match;
      len -= (match - str);
    }
  }
}

int tokenize(string input, string delim,
             string **tokens, int *size){  
  if(!tokens){
    //It's rather arbitary, but default to a buffer size of 8
    *tokens = xmalloc(sizeof(string)*8);
    *size = 8;
  }
  int tokens_read = 0;
  uint8_t reject[256] = {0};
  int i = 0, j = 0;
  for(i=0;i<delim.len;i++){
    reject[delim.mem[i]] = 1;
  }
  //skip leading delimiters
  i = memspn_table(input.mem, input.len, reject);
  while(i < input.len){
    if(j > *size){
      *size *= 2;
      *tokens = xrealloc(*tokens, *size);
    }
    int spn = memcspn_table(input.mem + i, input.len - i, reject);
    *tokens[j++] = sub_string(input, i, i+spn);
    i += spn;
    //if there are several delmitimers in a row skip over all of them
    //There should be a way to toggle this. This is what we want for something
    //whitespace delimited, but not for something e.g. comma delimited
    i += memspn_table(input.mem + i, input.len - i, reject);
  }
  return j;
}
