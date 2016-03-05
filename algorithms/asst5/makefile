CC=gcc
CFLAGS=-std=c11 -Wall -O2 -g -Wno-unused

all: spell-check spell-check_debug

SPELL_CHECK_SOURCES=spell_check.c trie.c trie_spell_check.c C_util.c rbtree.c

spell-check: $(SPELL_CHECK_SOURCES)
	$(CC) $(CFLAGS) -DNDEBUG -o $@ $^

spell-check_debug: $(SPELL_CHECK_SOURCES)
	$(CC) $(CFLAGS) -fsanitize=address -Og -g3 -o $@ $^


clean:
	rm -f spell-check spell-check_debug
	rm -f *.o

