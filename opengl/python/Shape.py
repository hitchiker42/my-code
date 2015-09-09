from OpenGL.GL import *
from OpenGL.GLUT import *
from itertools import *
import numpy as np
import ctpypes
# Data used by opengl is stored in numpy arrays
point_type = np.dtype({'names': ['x','y','z'],
                              'formats': list(repeat(np.float32,3))})
color_type = np.dtype({'names': ['r','g','b','a'],
                              'formats': list(repeat(np.float32,4))})
vertex_type = np.dtype([("position", point_type), ("color", color_type)])
class Shape:
    def __init__(self, vertices):
        self.vertices = vertices
        self.buffer = glGenBuffers(1);
        glBufferData(GL_ARRAY_BUFFER, self.vertices.nbytes,
                     self.vertices, self.buffer)
        
        xb
        
        
