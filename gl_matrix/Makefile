CFLAGS=-Wall -O3 -ffast-math -std=c11 -lm -D_GNU_SOURCE

all: gl_matrix.a

clean: 
	rm -f gl_matrix.a mat?.o vec?.o quat.o
gl_matrix.a: mat4.o mat3.o mat2.o vec4.o vec3.o vec2.o quat.o
	ar rcs $@ $^
