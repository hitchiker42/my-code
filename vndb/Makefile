DEBUGFLAGS= -DDEBUG -g -Og # -fsanitize=address # Asan is great, but it breaks under gdb
CPPFLAGS:= $(CPPFLAGS) -I$(PWD)/../C++/util -Wall -Wextra -Wno-unused-function -Wmissing-braces
CXXFLAGS:= $(CXXFLAGS) -std=gnu++17 $(DEBUGFLAGS)  -fno-rtti -fno-exceptions -Wno-class-memaccess
CXXFLAGS_NDEBUG=-std=gnu++17 -O2 -DNDEBUG -fno-rtti -fno-exceptions -Wno-class-memaccess
CFLAGS=-std=gnu11 -lm $(DEBUGFLAGS)

LDFLAGS=-lssl -lcrypto -lpcre2-8 -lz -ljpeg -ldl -lSDL2 -lreadline -pthread

SOURCES=$(wildcard *.cpp) $(wildcard *.h) $(wildcard *.c) json.hpp

PROGRAM_FILES=vndb_cpp

.PHONY: all clean tags

all: vndb_cpp

# C++ files
main.o: main.cpp vndb.h json.hpp sql.h sqlite_wrappers.h connection.h image.h
vndb.o : vndb.cpp vndb.h json.hpp sql.h sqlite_wrappers.h connection.h
connection.o : connection.cpp vndb.h json.hpp connection.h sqlite_wrappers.h
sqlite_wrappers.o : sqlite_wrappers.cpp vndb.h json.hpp sql.h sqlite_wrappers.h
tag_trait_parser.o: tag_trait_parser.cpp vndb.h json.hpp sql.h sqlite_wrappers.h
interaction.o: interaction.cpp vndb.h json.hpp gui.h
misc.o: misc.cpp vndb.h
# C files
sqlite_ext.o: sqlite_ext.c
image.o: image.c image.h
sdl_gui.o: sdl_gui.c gui.h
# the .o file doesn't actually depend on the .h file, but it
# makes more sense to put the dependency here rather than vndb_cpp.
progress_bar.o: progress_bar.c progress_bar.h

# temporary (hopefull)
sdl_test.o: sdl_test.cpp vndb.h
# compile c++ to asm
%.s:%.cpp
	$(CXX) $(CXXFLAGS_NDEBUG) $(CPPFLAGS) -S $< -o $@
# compile c++ to asm, then demangle names.
# Just a quick note, -o- prints to stdout, at least when outputing asm
%.S:%.cpp
	$(CXX) $(CXXFLAGS_NDEBUG) $(CPPFLAGS) -S $< -o- | c++filt > $@

# Compile the sqlite library, We complile it outselves since we use a fairly recent
# sql extension, but this also allows us to improve performance quite a bit since
# we can disable things we don't need.
sqlite3.o: sqlite3.c
	$(CC) -c -Os -o $@ $^ -Wall -DSQLITE_ENABLE_JSON1 \
	-DSQLITE_DEFAULT_MEMSTATUS=0 \
	-DSQLITE_LIKE_DOESNT_MATCH_BLOBS -DSQLITE_OMIT_SHARED_CACHE=1 \
	-DSQLITE_OMIT_DEPRECATED=1 -DSQLITE_OMIT_PROGRESS_CALLBACK=1 \
	-DSQLITE_DEFAULT_MEMSTATUS=0 -DSQLITE_USE_ALLOCA=1 \
	-DSQLITE_ENABLE_COLUMN_METADATA=1 -DSQLITE_ENABLE_STMTVTAB=1 -DHAVE_USLEEP=1

vndb_cpp: vndb.o connection.o sqlite_wrappers.o interaction.o progress_bar.o main.o image.o tag_trait_parser.o sqlite3.o sdl_gui.o sqlite_ext.o misc.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^

sdl_test_cpp: vndb.o connection.o sqlite_wrappers.o progress_bar.o image.o tag_trait_parser.o sqlite3.o sdl_gui.o sqlite_ext.o misc.o sdl_test.o
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^
clean:
	rm -f *.o $(PROGRAM_FILES) a.out
tags: TAGS
TAGS: $(SOURCES)
	ctags -eR *
