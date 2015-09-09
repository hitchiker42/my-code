cdef class Vertex:
    cdef float pos[3]
    cdef float color[4];
    def __cinit__(self, pos, color):
        self.pos = pos
        self.color = color
    #enddef
#endclass
