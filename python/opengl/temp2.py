#! /usr/bin/env python
# -*- coding: utf-8 -*-
# -----------------------------------------------------------------------------
# Copyright (c) 2014, Nicolas P. Rougier. All rights reserved.
# Distributed under the terms of the new BSD License.
# -----------------------------------------------------------------------------
import sys
import ctypes
import numpy as np
from OpenGL.GL import *
from OpenGL.GLUT import *
vertex_code = """
    uniform float scale;
    attribute vec4 color;
    attribute vec2 position;
    varying vec4 v_color;
    void main(){
      gl_Position = vec4(scale*position, 0.0, 1.0);
      v_color = color;
    } """

fragment_code = """
    varying vec4 v_color;
    void main(){
        gl_FragColor = v_color;
    } """

def display():
    glClear(GL_COLOR_BUFFER_BIT)
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4)
    glutSwapBuffers()

def reshape(width,height):
    glViewport(0, 0, width, height)

def keyboard( key, x, y ):
    if key == '\033':
        sys.exit( )


# GLUT init
# --------------------------------------
glutInit()
glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE)
glutCreateWindow('test')
glutReshapeWindow(400,400)
glutReshapeFunc(reshape)
glutDisplayFunc(display)
glutKeyboardFunc(keyboard)

# Build data
# --------------------------------------
data = np.zeros(4, [("position", np.float32, 2),
                    ("color",    np.float32, 4)])
data['color']    = [ (1,0,0,1), (0,1,0,1), (0,0,1,1), (1,1,0,1) ]
data['position'] = [ (-1,-1),   (-1,+1),   (+1,-1),   (+1,+1)   ]

# Build & activate program
# --------------------------------------

# Request a program and shader slots from GPU
program  = glCreateProgram()
vertex   = glCreateShader(GL_VERTEX_SHADER)
fragment = glCreateShader(GL_FRAGMENT_SHADER)

# Set shaders source
glShaderSource(vertex, vertex_code)
glShaderSource(fragment, fragment_code)

# Compile shaders
glCompileShader(vertex)
glCompileShader(fragment)

# Attach shader objects to the program
glAttachShader(program, vertex)
glAttachShader(program, fragment)

# Build program
glLinkProgram(program)

# Get rid of shaders (no more needed)
glDetachShader(program, vertex)
glDetachShader(program, fragment)

# Make program the default program
glUseProgram(program)


# Build buffer
# --------------------------------------

# Request a buffer slot from GPU
buffer = glGenBuffers(1)

# Make this buffer the default one
glBindBuffer(GL_ARRAY_BUFFER, buffer)

# Upload data
glBufferData(GL_ARRAY_BUFFER, data.nbytes, data, GL_DYNAMIC_DRAW)


# Bind attributes
# --------------------------------------
stride = data.strides[0]
offset = ctypes.c_void_p(0)
loc = glGetAttribLocation(program, "position")
glEnableVertexAttribArray(loc)
glBindBuffer(GL_ARRAY_BUFFER, buffer)
glVertexAttribPointer(loc, 3, GL_FLOAT, False, stride, offset)

offset = ctypes.c_void_p(data.dtype["position"].itemsize)
loc = glGetAttribLocation(program, "color")
glEnableVertexAttribArray(loc)
glBindBuffer(GL_ARRAY_BUFFER, buffer)
glVertexAttribPointer(loc, 4, GL_FLOAT, False, stride, offset)

# Bind uniforms
# --------------------------------------
loc = glGetUniformLocation(program, "scale")
glUniform1f(loc, 1.0)

# Enter mainloop
# --------------------------------------
glutMainLoop()
