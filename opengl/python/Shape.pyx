cdef class Vertex:
    cdef float pos[3], color[4];
    def __cinit__(float[3] pos={0.0f,0.0f,0.0f}, color = (0.0,0.0,0.0,1.0)
