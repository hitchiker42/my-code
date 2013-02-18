#include <stdio.h>
int main(void){
	double i=1;
	double e=0;
	int n=12;
	for(n=1;n<12;n++){
		e+=1/i;
		i=i*n;
	}
	printf("%d\n",e);
	return(0);
}
