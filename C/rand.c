
#include <stdlib.h>
#include <stdio.h>
int main(void){
  int x=0,guess=0;
  srand ( time(NULL) );
  printf("\nGuess an integer from 1-15\n");
  scanf("%d" ,&guess);
  x=rand()%15;x+=1;
while(x!=0){
  while(guess<16&&guess>0){ 
    if (x==guess){
      printf("\nyou won\n");
      return(0);
    }
    else if (x==guess-1||x==guess+1){
      printf("\nVery Close\n");
    }
    else if (x==guess+2||x==guess+3||x==guess-2||x==guess-3){
      printf("\nGetting Close\n");
    }
    else {printf("\nNot Close\n");
    }
    printf("\nGuess an integer from 1-15\n");
    scanf("%d",&guess);
  }
  printf("\nPick is not in Range\n");
  printf("\nGuess an integer from 1-15\n");
  scanf("%d",&guess);
}
return(0); 
}

