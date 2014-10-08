#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define SWAP(x,y)                               \
  __extension__ ({ __typeof(x) _x = x;          \
      x = y;                                    \
      y = _x;})
typedef struct align_result align_result;
typedef struct grid_entry grid_entry;
struct align_result {
  char *alignment;
  int max_score;
};
struct grid_entry {
  int32_t score;
  uint32_t parent;
};
int dyn_programming_solution(char *string1, unsigned int len1,
			     char *string2, unsigned int len2){
	/* Here is the thing you have to fill in.*/
	/* Find the alignment */
	/* print the max score */
	/* Display the alignment */
	return 1;
}
static inline int max3(int32_t a,int32_t b,int32_t c){
  int32_t max = a;
  if(b>max){
    max = b;
  }
  if(c>max){
    max = c;    
  }
  switch(max){
    case a: return 1;
    case b: return 2;
    case 3: return 3;
  }
}
align_result needleman_wunsch(uint8_t *str1, uint32_t str_len1,
                              uint8_t *str2, uint32_t str_len2){
  //make sure the first string is longer, just to simplify things
  //also so we can put it in a row instead of a column
  if(str_len2 > str_len1){    
    SWAP(str1, str2);
    SWAP(str_len1, str_len2);
  }
  uint32_t len1 = str_len1;
  uint32_t len2 = str_len2;
  grid_entry* grid = malloc(sizeof(grid_entry)*len1*len2);
  /* I don't like using 2-d arrays (i.e a grid_entry**), so I use
     a 1-D array and use pointer arithmetic to calculate indices,
     just a heads up.
   */
  memset(grid, '\0', sizeof(grid_entry)*len1*len2);
  int i,j,k;
  /*
    The idea of this algoritm is to use a len1xlen2 grid whoes elements
    are the best possible score at for that location. matches give a 
    score of +1 and mismatches/insertions/deletions given a score of -1.

    The elements also hold the index of the previous element in the
    sequence used to get to that element, so the actual best alignment
    can be computed. 

    movements from non diagonals represent insertions/deletions and
    movements from diagonals represent matches. up is a gap in str1
    while left is a gap in str2
   */
  grid[0] = {-1,0};
  //the first row/column can only be composed of insertions, so
  //they are easy to fill
  for(i=1;i<len1;i++){
    grid[i] = {grid[i-1]-1, i-1};
  }
  for(j=1;j<len2;j++){
    grid[j*len1] = {grid[(j-1)*len1]-1, (j-1)*len1};
  }
  //now comes the actual meat of the algorithm
  uint32_t len = len1;
  for(i=1;i<len1;i++){
    for(j=1;j<len2;j++){
      int32_t match = (str1[i]==str2[i]?1:-1) + grid[((j-1)*len)+(i-1)];
      int32_t del = grid[(j*len)+(i-1)]-1;
      int32_t ins = grid[(i*len)+(j-1)]-1;
      //in the case of equally optimal solutions
      //prefer insertions, for no particular reason
      int32_t best = ins;
      grid_location *loc = grid+(j*len)+i;
      if(del > best){best = del;}
      if(match > best){best = match;}
      if(best == ins){
        *loc = {best,(i*len)+(j-1)}
      } else if (best == del){
        *loc = {best,(j*len)+(i-1)};
      } else {
        *loc = {best,((j-1)*len)+(i-1)};
      }
    }
  }
  //the longest possible path is len1+len2(I don't know how that's)
  //possible but eh.
  //construct string representing the aligned sequences
  char *top = malloc(sizeof(char)*(len1+len2));
  char *str1_ptr = str1 + str_len1 - 1;
  char *bottom = malloc(sizeof(char)*(len1+len2));
  char *str2_ptr = str2 + str_len2 - 1;
  char *top_ptr= top+len1+len2;
  *top_ptr--='\0';
  char *bottom_ptr = bottom+len1+len2;
  *bottom_ptr--='\0';
  int i=len1-1,j=len2-1;
  while(i>0 && j>0){
    grid_location loc = grid[j*len+i];
    int32_t up = ((j-1)*len+i);
    int32_t left = (j*len+(i-1));
    int32_t diag = ((j-1)*len + (i-1));
    int32_t next = loc.parent;
    if(next == up){
      *top--='_';
      *bottom--=*str2_ptr--;
      j--;
    } else if(next == down)
    
    
