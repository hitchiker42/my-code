## Since openGL is written in C we can import all the
## symbols without worrying about namespace clashing since
## they're all prefixed appropiately.
from OpenGL.GL import *;
from OpenGL.GLU import *;
from OpenGL.GLUT import *;
from OpenGL.arrays import vbo;
from OpenGL.GL import shaders;
import numpy as np;
    
symbol_table = globals();

def glut_init(mode=(GLUT_RGBA | GLUT_ALPHA | GLUT_DOUBLE | GLUT_DEPTH),
              size=(400,400), position=(0,0), title=""):
    """Convience function to initialize glut and create a window"""
    glutInit();
    glutInitDisplayMode(mode);
    glutInitWindowSize(*size);
    glutInitWindowPosition(*position);
    return glutCreateWindow(title);
#enddef
