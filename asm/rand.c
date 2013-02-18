#include <stdlib.h>
#include <stdio.h>
int main(void){
	int ans=rand()%25+1;
	int guess=0;
	int try=0;
	printf("Guess a random number between one and 25\n");
	scanf("%d",&guess);
	while(1){
		if (guess==ans){
			printf("Congradulations you guessed right\n");
			return(0);
		}
		else {
			printf("Sorry wrong answer, enter a non zero number to  guess again?\n");
			try++;
			if (try>10){
				printf("Too many tries");
				return(0);
			}
		}
		scanf("%d",&guess);
		if (guess<=0||guess>25){
			return(0);
		}
	}
}
