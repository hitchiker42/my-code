prog5: prog5.c prog5.h prog5_macros.h prog5_consts.h prog5_heap.c
	gcc -o prog5 prog5.c -std=c99 -Wall -O1 -g -lpthread
prog5_ndebug: prog5.c prog5.h prog5_macros.h prog5_consts.h prog5_asm.c
	gcc -o prog5 prog5.c -std=c99 -Wall -O1 -g -lpthread -DNDEBUG
prog5_prof:	prog5.c prog5.h prog5_macros.h prog5_consts.h prog5_heap.c
	gcc -o prog5_prof prog5.c -std=c99 -Wall -DDEBUG -O1 -g --coverage -pg
prog5_branch:	prog5.c prog5.h prog5_macros.h prog5_consts.h prog5_heap.c
	gcc -o prog5_prof prog5.c -std=c99 -Wall -DDEBUG -O1 -g --coverage -pg \
	 -fbranch-probabilities
prog5.s: prog5.c prog5.h prog5_macros.h prog5_consts.h prog5_heap.c
	gcc -o prog5.s prog5.c -std=c99 -Wall -O1 -S -DNDEBUG
prog5_debug.s: prog5.c prog5.h prog5_macros.h prog5_consts.h prog5_asm.c
	gcc -o prog5_debug.s prog5.c -std=c99 -Wall -O1 -S -DDEBUG -g
prog5_my_threads: prog5_my_threadss.c prog5_my_threadss.h prog5_macros.h prog5_consts.h
	gcc -o prog5_my_threads prog5_my_threadss.c -std=c99 -Wall -O1 -g -lmy_threads
prog5_my_threads.s: prog5_my_threadss.c prog5_my_threadss.h prog5_macros.h prog5_consts.h
	gcc -o prog5_my_threads.s prog5_my_threadss.c -std=c99 -Wall -O1 -DNDEBUG -S
prog5_my_threads_ndebug: prog5_my_threadss.c prog5_my_threadss.h prog5_macros.h prog5_consts.h
	gcc -o prog5_my_threads.s prog5_my_threadss.c -std=c99 -Wall -O1 -DNDEBUG -g
