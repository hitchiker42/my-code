CC := clang
CXX := clang++
LLVM_MODULES := core jit
XCFLAGS := $(shell llvm-config --cflags) -Wno-return-type $(CFLAGS) -g
XCXXFLAGS := $(shell llvm-config --cxxflags)
XLDFLAGS := $(shell llvm-config --ldflags) $(shell llvm-config --libs $(LLVM_MODULES))
C_FILES := codegen.c parser.c
.PHONY: all object
all: object
	$(CXX) $(wildcard *.o) $(XLDFLAGS) -o codegen
object:
	$(CC) $(C_FILES) $(XCFLAGS) -c
