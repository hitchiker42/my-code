CC := gcc
CXX := g++
LLVM_MODULES := all #I give up trying to find out where everything is 
XCFLAGS := $(shell llvm-config --cflags) -Wno-return-type $(CFLAGS) -g
XCXXFLAGS := $(shell llvm-config --cxxflags)
XLDFLAGS := $(shell llvm-config --ldflags) $(shell llvm-config --libs $(LLVM_MODULES))
C_FILES := codegen.c parser.c
OBJFILES = $(wildcard *.o)
.PHONY: all object clean
all: object
	$(CXX) $(OBJFILES) $(XLDFLAGS) -o Kaleidoscope
object:
	$(CC) $(C_FILES) $(XCFLAGS) -c
clean:
	-rm -f $(OBJFILES)
