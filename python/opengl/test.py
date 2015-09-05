from OpenGL.GL import *
from OpenGL.GLUT import *
import numpy as np
import sys
import ctypes
vertex_source = """
    uniform float scale;
    attribute vec4 color;
    attribute vec2 position;  
    varying vec4 v_color;
    void main(){
      gl_Position = vec4(scale*position, 0.0, 1.0);
      v_color = color;
    } """
fragment_source = """
    varying vec4 v_color;
    void main(){
      gl_FragColor = v_color;
    }"""
def display():
    glClear(GL_COLOR_BUFFER_BIT)
    glDrawArrays(GL_TRIANGLE_STRIP,0,4)
    glutSwapBuffers()
#enddef
def reshape(width,height):
    glViewport(0, 0, width, height)
#enddef
def keyboard(key, x, y):
    if key == '\033':
        sys.exit()
    #endif
#enddef
glutInit()
glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE)
glutCreateWindow("test")
glutReshapeWindow(512,512)

glutReshapeFunc(reshape)
glutDisplayFunc(display)
glutKeyboardFunc(keyboard)


data = np.zeros(4, [("position", np.float32, 2),
                    ("color",    np.float32, 4)])
data['color']    = [ (1,0,0,1), (0,1,0,1), (0,0,1,1), (1,1,0,1) ]
data['position'] = [ (-1,-1),   (-1,+1),   (+1,-1),   (+1,+1)   ]

## There are eaiser ways to do this, but this is the code
## that runs behind the scenes, so it's important to
## know how it works
program = glCreateProgram()
vertex = glCreateShader(GL_VERTEX_SHADER)
fragment = glCreateShader(GL_FRAGMENT_SHADER)

glShaderSource(vertex, vertex_source)
glShaderSource(fragment, fragment_source)

glCompileShader(vertex)
glCompileShader(fragment)

glAttachShader(program, vertex)
glAttachShader(program, fragment)

glLinkProgram(program)
glValidateProgram(program)

glDetachShader(program, vertex)
glDetachShader(program, fragment)

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

glutMainLoop()
#endif
