from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
import numpy as np
import itertools, functools, math

class Shape:
    def __init__(self, vertices, indices):
        self.vertices = vertices
        self.size = vertices.nbytes
        self.indices = indices                
    #enddef    
#endclass

#A regular polygon is a polygon with all sides and angles
#equal (i.e an equalateral triangle, a square, etc...)
class Regular_Polygon(Shape):
    def __init__(self, positon, sides, radius): 
        """Construct a regular polygon given the center 
        and the (circum)radius"""
        self.interior_angle = ((sides - 2)*math.pi)/sides
        
        
    
