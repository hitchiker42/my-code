cdef class Cons:
    cdef object car
    cdef object cdr
    def __cinit__(self, car, cdr):
        self.car = car
        self.cdr = cdr
    #enddef
    def __len__(self):
        len_acc(self, 0)
    #enddef
#endcdef class Cons

#car and cdr are python functions, just use direct access in C
def car(Cons ls):
    if(ls is not None):
        return ls.car
    else
        return None
    #endif
#enddef
def cdr(Cons ls):
    if(ls is not None)
        return ls.cdr
    else
        return None
    #endif
#enddef
#Define recursive functions in C, make python wrappers around them
cdef len_acc(Cons ls, long acc):
    if(ls.cdr is None):
        return acc
    else:
        return len_acc(ls.cdr,acc+1)
    #endif
#endcdef
cdef nth_acc(Cons l, long n):
    if(l is None):   #need to check this first
        return None
    elif(n == 0)
        return l.car
    else
        return nth(l.cdr, n-1)
    #endif
#endcdef
        
