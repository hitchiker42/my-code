/**
 *   gl770.h 
 *
 *   This file tries to encapsulate in one file all the variations
 *   needed for writing portable OpenGL programs for CS770/870
 *
 *   It includes extra stuff for the early assignments and perhaps
 *   will need to be extended for later assignments.
 *   
 *   Edit it as needed, but submit edited version only after testing
 *   that it works on agate.
 * 
 *   rdb 10/22/13
 *       09/07/13 Changed name to gl770.h and other changes for MAC
 *       10/28/14 Added gl3.h and gl3ext.h to APPLE includes
 *       11/07/14 Removed explicit includes for freeglut on linux
 *       09/06/15 Added contents of glsl770.h for Angel's checkGL
 *                and makeShaders
 *       09/09/15 Moved glew.h before glut.h in WIN section
 *       09/10/15 In linux: include <GL/freeglut_ext.h>
 */

#ifndef __GL770_H__
#define __GL770_H__
#define GL_GLEXT_PROTOTYPES

#if defined( __APPLE__ ) // include Mac OS X verions of headers
#include "GL/glew.h"    //rdb most angel demos segfault without glew
#include <OpenGL/OpenGL.h>
#include <OpenGL/gl3.h>
#include <OpenGL/gl3ext.h>
#include <GLUT/glut.h>
#include "GL/glui.h"

#elif defined( __WIN32 ) || defined( __WIN64 )
#include <windows.h>
#include <GL/glew.h>   
#include <GL/glut.h>   // glut.h includes gl.h
#include <GL/freeglut_ext.h>
#include <GL/glext.h>
#include <GL/glui.h>

#else     // assume linix (__LINUX__) or cygwin (__CYGWIN__)
#include <GL/glew.h>
#include <GL/glut.h>   // glut.h includes gl.h
#include <GL/freeglut_ext.h>
#include <GL/glui.h>
#include <GL/glext.h>
#endif  // __APPLE__

                                                  // Include some Angel includes
#include "mat.h"
#include "vec.h"

#include "glsl770.h"

#endif  // __GL770_H__
