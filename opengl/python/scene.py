from OpenGL.GL import *
from OpenGL.GLU import *
#from OpenGL.GLUT import *
import cyglfw as glfw
import numpy as np
import itertools, functools, math, os.path
# Numpy datatypes for vertex attributes
point_type =  np.dtype({'names' : ['xzy','x','y','z'],
                        'formats' : [('<f4', (3,)),'<f4','<f4','<f4'],
                        'offsets' : [0,0,4,8], 'itemsize' : 12})
color_type = np.dtype({'names' : ['rgba','r','g','b','a'],
                       'formats' : [('<f4', (4,)), '<f4', '<f4', '<f4', '<f4'],
                       'offsets' : [0,0,4,8,12], 'itemsize' : 16})
def compile_shader(shader_type, src):
    """Compile a shader of type 'shader_type' from 'src'.
    An exception is raised if the shader fails to compile"""
    shader = glCreateShader(shader_type)
    glShaderSource(shader, src)
    if(glGetShaderiv(shader, GL_COMPILE_STATUS) != GL_TRUE):
        raise RuntimeError(glGetShaderInfoLog(shader))
    #endif
    return shader
#enddef
def create_shader_program(*sources):
    """Create a shader program from the provided shader source code.
    Arguments are of the form (source, type), where source is either a string
    containing the literal source code for the shader, or a filename to read
    the shader from and type specifies the type of shader
    (e.g. GL_FRAGMENT_SHADER or GL_VERTEX_SHADER)"""
    shaders = list()
    for source in sources:
        if(os.path.exists(source[0])):
            with open(source[0], 'r') as shader_file:
                src = shader_file.readall();
            #endwith
        else:
            src = source[0]
        #endif
        shaders.append(compile_shader(source[1], src))
    #endfor
    program = glCreateProgram()
    for shader in shaders:
        glAttatchShader(program, shader)
    #endfor
    glLinkProgram(program)
    if(glGetProgramiv(program, GL_LINK_STATUS)):
        raise RuntimeError(glGetProgramInfoLog(program))
    #endif
    glValidateProgram(program)
    if(glGetProgramiv(program, GL_VALIDATE_STATUS)):
        raise RuntimeError(glGetProgramInfoLog(program))
    #endif
    for shader in shaders:
        glDetachShader(program, shader)
        glDeleteShader(shader)
    #endfor
    glUseProgram(program)
    return program
#enddef
def init_gl_context(width, height, name):
    if(!glfw.Init()):
            raise RuntimeError("Glfw initialization failed")
        #endif
        for hint in ((glfw.SAMPLES, 4),(glfw.CONTEXT_VERSION_MAJOR, 3),
                     (glfw.CONTEXT_VERSION_MINOR,3),(glfw.OPENGL_FORWARD_COMPAT,1)
                     (glfw.OPENGL_PROFILE, glfw.OPENGL_CORE_PROFILE))
            glfw.WindowHint(*hint)
        #endfor
        
class Scene:
    def __init__(self, vertex_shader_src = "", fragment_shader_src = ""):
        
        self.program = create_shader_program((vertex_shader_src, GL_VERTEX_SHADER),
                                             (fragment_shader_src, GL_FRAGMENT_SHADER))
        self.VAO = glGenVertexArrays(1);
        
