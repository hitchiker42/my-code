//////////////////////////////////////////////////////////////////////////////
//
//  --- glsl770.h ---
//
//  reduced version of Angel.h: just enough to use 
//     makeShaders.cpp (modification of Angel's InitShader.cpp
//     mat.h
//     vec.h
//     checkGL macros  (modification of Angel's CheckError macro)
//////////////////////////////////////////////////////////////////////////////


#ifndef __GLSL770_H__
#define __GLSL770_H__

#include <iostream>
#include <cmath>

//  Define M_PI in the case it's not defined in the math header file
#ifndef M_PI
#  define M_PI  3.14159265358979323846
#endif

// Define a helpful macro for handling offsets into buffer objects
#define BUFFER_OFFSET( offset )   ((GLvoid*) (offset))

//----------------------------------------------------------------------------
//
//  --- Include our class libraries and constants ---
//

//  Helper function to load vertex and fragment shader files
//    and make a shader program with them.
GLuint makeShaders( const char* vertexShaderFile,
		            const char* fragmentShaderFile );

#include "checkGL.h"

#define Print(x)  do { std::cerr << #x " = " << (x) << std::endl; } while(0)


#endif // __GLSL770_H__

