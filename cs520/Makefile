prog5: prog5.c prog5.h
	gcc -o prog5 prog5.c -std=c99 -Wall -O1 -g -lpthread
prog5_ndebug: prog5.c prog5.h
	gcc -o prog5 prog5.c -std=c99 -Wall -O1 -g -lpthread -DNDEBUG
prog5_prof:	prog5.c prog5.h
	gcc -o prog5_prof prog5.c -std=c99 -Wall -DDEBUG -O1 -g --coverage -pg
prog5_branch:	prog5.c prog5.h
	gcc -o prog5_prof prog5.c -std=c99 -Wall -DDEBUG -O1 -g --coverage -pg \
	 -fbranch-probabilities
prog5.s: prog5.c prog5.h
	gcc -o prog5.s prog5.c -std=c99 -Wall -O1 -S -DNDEBUG
prog5_debug.s: prog5.c prog5.h
	gcc -o prog5_debug.s prog5.c -std=c99 -Wall -O1 -S -DDEBUG -g
