/**
 * \file align.c
 *
 *
 *
 * \author jtd7
 * \date 03-10-2011
 */

#include "align_io.h"
#include "dfs_solver.h"
#include <sys/time.h>
#include <string.h>
#include <assert.h>

typedef int(*compute_alignment)(char*, unsigned int, char*, unsigned int);

/* Gets the time of day in seconds. */
static double get_current_seconds(void){
    double sec, usec;
    struct timeval tv;

    if (gettimeofday(&tv, NULL) < 0) {
        perror("gettimeofday failed");
        exit(EXIT_FAILURE);
    }

    sec = tv.tv_sec;
    usec = tv.tv_usec;

    return sec + (usec / 1000000);
}


/*
 * Loads the tokens from the files and calls the output_diff() routine
 * to actually output the differences.
 */
static int align_files(char *f0, char *f1, compute_alignment c){
    int err = 0;
    char *ary0, *ary1;
    unsigned int n0, n1;
    double start, end;

    printf("Diffing %s and %s.\n", f0, f1);

    err = load_token_arrays(f0, f1, &ary0, &n0, &ary1, &n1);
    if (err) return 1;

    printf("%s contains %u characters.\n", f0, n0);
    printf("%s contains %u characters.\n", f1, n1);

    start = get_current_seconds();

    if (c != NULL) err = c(ary0,n0,ary1,n1);
    else err = 1;

    end = get_current_seconds();
    fprintf(stdout, "time: %f seconds\n", end - start);

    free(ary0);
    free(ary1);

    return err;
}

/*
 * Print the usage string and exit with failure status.
 */
static void usage(void){
    fprintf(stderr, "Usage\n" "align <alg> <file0> <file1>\n");
    exit(EXIT_FAILURE);
}


int dyn_programming_solution(char *str1, unsigned int str_len1,
                             char *str2, unsigned int str_len2){
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
  #ifdef NDEBUG
  #undef DEBUG
  #endif
  /*
I get warnings from -Wpedantic if I use these, which is why I have
debug code all enclosed in preprocessor conditionals
#ifndef NDEBUG
#define DEBUG_PRINT(fmt,args...) fprintf(stderr,fmt,##args)
#else
#define DEBUG_PRINT(...)

#endif
  */
#ifdef DEBUG
#define DEBUG_PRINT(fmt,...) fprintf(stderr,fmt,##__VA_ARGS__)
#endif
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
    //make sure the first string is longer, just to simplify things
  //also so we can put it in a row instead of a column
  int switched=0;
  if(str_len2 > str_len1){
    char *temp = str1;
    int temp2 = str_len1;
    str1 = str2;
    str_len1 = str_len2;
    str2 = temp;
    str_len2 = temp2;
    switched=1;
  }
  #ifdef DEBUG
  DEBUG_PRINT("starting dynamic programming algorithm\n");
  #endif
  
  
  int32_t len1 = str_len1+1;
  int32_t len2 = str_len2+1;
  grid_entry* grid = malloc(sizeof(grid_entry)*len1*len2);
  /* I don't like using 2-d arrays (i.e a grid_entry**), so I use
     a 1-D array and use pointer arithmetic to calculate indices,
     just a heads up.
   */
  memset(grid, '\0', sizeof(grid_entry)*len1*len2);
  int i,j;
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

    Update for fitting with the program spec:
    match = 1, anything else = 0
   */
  /*  if(str1[0] == str2[0]){
    grid[0] = (grid_entry){1,0};
  } else {
    grid[0] = (grid_entry){-1,0};
    }*/
  grid[0] = (grid_entry){0,0};
  //the first row/column can only be composed of insertions, so
  //they are easy to fill
  for(i=1;i<len1;i++){
    grid[i] = (grid_entry){grid[i-1].score-1, i-1};
  }
  for(j=1;j<len2;j++){
    grid[j*len1] = (grid_entry){grid[(j-1)*len1].score-1, (j-1)*len1};
  }
#ifdef DEBUG
  DEBUG_PRINT("starting body of dynamic programming algorithm\n"
              "Length of 1st string = %d, length of second string = %d\n",
              len1,len2);
#endif
  //now comes the actual meat of the algorithm
  uint32_t len = len1;
  int32_t final_score = 0;
  for(j=1;j<len2;j++){
    for(i=1;i<len1;i++){
      /* Apperently we don't do mismatches at all
      int32_t match = (str1[i]==str2[j]?1:-1) +
        grid[((j-1)*len)+(i-1)].score;
      */
      int32_t match;
      if(str1[i-1]==str2[j-1]){
        //        fprintf(stderr, "Match i==%d; j==%d\n",i,j);
        match = grid[(j-1)*len + (i-1)].score+1;
      } else {
        match = INT_MIN;
      }
      int32_t del = grid[(j*len)+(i-1)].score-1;
      int32_t ins = grid[((j-1)*len)+i].score-1;
      int32_t best = match;
      grid_entry *loc = grid+((j*len)+i);
      if(del > best){best = del;}
      if(ins > best){best = ins;}
      if(best == match){
        assert(best != INT_MIN);
        *loc = (grid_entry){best,((j-1)*len)+(i-1)};
      } else if (best == ins){
        *loc = (grid_entry){best,((j-1)*len+i)};
      } else {//if (best == del){
        *loc = (grid_entry){best,(j*len)+(i-1)};
      }
    }
  }
#if 0
  for(j=0;j<len2;j++){
    fprintf(stderr,"\n");
    for(i=0;i<len1;i++){
      fprintf(stderr,"%d\t",grid[(j*len)+i].score);
    }
  }
  fprintf(stderr,"\n");
#endif

  //the longest possible path is len1+len2, so allocate that much
  //construct string representing the aligned sequences
  char *top = malloc(sizeof(char)*(len1+len2));
  char *top_end = top+len1+len2-1;
  char *str1_ptr = str1 + str_len1 - 1;
  char *bottom = malloc(sizeof(char)*(len1+len2));
  char *bottom_end = bottom+len1+len2-1;
  char *str2_ptr = str2 + str_len2 - 1;
  char *top_ptr= top_end;
  *top_ptr--='\0';
  char *bottom_ptr = bottom_end;
  *bottom_ptr--='\0';
  i=len1-1,j=len2-1;
  while(i>0 || j>0){
    /*    if(i>0 && j>0 && grid[j*len+i].score==grid[(j-1)*len+(i-1)].score+1){
      *top_ptr--=*str1_ptr--;
      *bottom_ptr--=*str2_ptr--;
      i--;j--;
    } else if (i>0 && grid[j*len+i].score==grid[j*len+(i-1)].score-1){
      *top_ptr--=*str1_ptr--;
      *bottom_ptr--='-';
      i--;
    } else if (j>0 && grid[j*len+i].score==grid[(j-1)*len+i].score-1){
      *bottom_ptr--=*str2_ptr--;
      *top_ptr--='-';
      j--;
    } else {
      fprintf(stderr,"Unknown error, aborting\n");
      abort();
      }*/
      
    grid_entry loc = grid[j*len+i];
    int32_t up = ((j-1)*len+i);
    int32_t left = (j*len+(i-1));
    int32_t diag = ((j-1)*len + (i-1));
    int32_t next = loc.parent;
    if(next == up){
      *top_ptr--='-';
      *bottom_ptr--=*str2_ptr--;
      j--;
    } else if(next == left){
      *top_ptr--=*str1_ptr--;
      *bottom_ptr--='-';
      i--;
    } else if(next == diag){
      *top_ptr--=*str1_ptr--;
      *bottom_ptr--=*str2_ptr--;
      i--;
      j--;
      final_score++;
    } else {
      fprintf(stderr,"Unknown Error, Aborting\n");
      abort();
      }
  }
  //  assert(i==0);assert(j==0);
  *top_ptr = *str1_ptr;
  *bottom_ptr = *str2_ptr;
#ifdef DEBUG
  printf("Final string length = %ld\n",top_end-top_ptr);
  printf("Final string length = %ld\n",bottom_end-bottom_ptr);
#endif
    top_ptr++;
    bottom_ptr++;
  printf("Alignment Score: %d\n",final_score);
  //If we switched the strings for processing switch them back for output
  if(switched){
    char *temp1=top_ptr;
    char *temp2=top_end;
    top_ptr = bottom_ptr;
    top_end = bottom_end;
    bottom_ptr = temp1;
    bottom_end = temp2;
  }
  //print lines with a maximum length of 75 + 3(for...) per line
  int max_len = 75;
  while(1){
    if((top_ptr+max_len) > top_end || (bottom_ptr+max_len) > bottom_end){
      printf("%.*s\n%.*s\n\n",max_len,top_ptr,max_len,bottom_ptr);
      break;
    } else {
      printf("%.*s...\n%.*s...\n\n",max_len,top_ptr,max_len,bottom_ptr);
    }
    top_ptr+=max_len;
    bottom_ptr+=max_len;
  }
  //  printf("Sequence 1: %s\n", top_ptr+1);
  //  printf("Sequence 2: %s\n", bottom_ptr+1);

  /* Here is the thing you have to fill in.*/
  /* Find the alignment */
  /* print the max score */
  /* Display the alignment */
  free(top);free(bottom);free(grid);
  return 0;
}


int main(int argc, char *const argv[]){
        char *alg, *f0, *f1;
        compute_alignment c = NULL;
        int err;

        if(argc < 3) usage();
        alg = argv[1];
        f0 = argv[2];
        f1 = argv[3];
        if(strcmp(alg,"dfs") == 0){
                c = &call_dfs_solver; /* from dfs_solver.c */
        }else if(strcmp(alg,"dyn") == 0){
                c = &dyn_programming_solution;
        }else{
                printf("Algorithm %s not recognized, expected dfs or dyn\n",
                       alg);
                return EXIT_FAILURE;
        }
        err = align_files(f0, f1,c);
        if (err) return EXIT_FAILURE;
        return EXIT_SUCCESS;
}
