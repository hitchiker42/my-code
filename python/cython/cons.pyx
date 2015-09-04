cdef class Cons:
    cdef object car
    cdef object cdr
    def __cinit__(Cons self, object car, object cdr):
        self.car = car
        self.cdr = cdr
    #enddef
    def __len__(Cons self):
        len_acc(self, 0)
    #enddef    
    def __richcmp__(Cons self, object x, int op):
        if(op == 2 || op == 3):
            int result = cmp_acc(self, <Cons?>x);
            if(op == 2):
                return result;
            else:
                return !result;
            #endif
        else:
            raise(TypeError("Cons does not support ordered comparisons"));
        #endif
    #enddef
#endcdef class Cons

#car and cdr are python functions, just use direct access in C
def object car(Cons ls):
    if(ls is not None):
        return ls.car
    else
        return None
    #endif
#enddef
def object cdr(Cons ls):
    if(ls is not None)
        return ls.cdr
    else
        return None
    #endif
#enddef
cpdef object pop(Cons ls):
    if(ls is not None):
        object ret = ls.car;
        ls = ls.cdr;
        return ret;
    else:
        return None;
    #endif
#endcpdef
cpdef object push(object elt, Cons ls):
    Cons ret = Cons(elt, ls);
    ls = ret;
    return ret;
#endcpdef
#Define recursive functions in C, make python wrappers around them
cdef long len_acc(Cons ls, long acc):
    if(ls.cdr is None):
        return acc
    else:
        return len_acc(ls.cdr,acc+1)
    #endif
#endcdef
cdef int cmp_acc(Cons x, Cons y):
    if(x is not None && y is not None):
        if(x.car == y.car):
            return cmp_acc(x.cdr, y.cdr);
        else
            return 0;
        #endif
    elif(x is None && y is None):
        return 1;
    else:
        return 0;
    #endif
#endcdef    
        
cdef object nth_acc(Cons l, long n):
    if(l is None):   #need to check this first
        return None
    elif(n == 0)
        return l.car
    else
        return nth(l.cdr, n-1)
    #endif
#endcdef
cdef consp(object obj):
    return isinstance(obj, Cons);
#endcdef

#define in C to make looping efficent
#assume non None value for l
cdef Cons mapcar(object fxn, Cons l):
    Cons ret = Cons(fxn(pop(l)), None);
    Cons *tail = &ret;
    while(consp(l)):
        tail.cdr = fxn(pop(l));
        tail = &tail.cdr
    #endwhile
#endcdef    

#Assumes a non-empty tuple
cdef Cons tuple_to_cons(object tpl):
    Cons ret = Cons(tpl[0], None);
    Cons* tail = &ret; #I'm not totally sure how cython treats pointers
    long n = len(tpl);
    for i from 1 <= i < n:
        tail.cdr = Cons(tpl[i], None);
        tail = &tail.cdr;
    #endfor
    return ret
#endcdef

#make small lists
cdef Cons list1(object a):
    return Cons(a,None);
#endcdef
cdef Cons list2(object a, object b):
    return Cons(a,Cons(b, None));
#endcdef


#since calling it list would shadow the python function
def Cons make_list(*rest):
    if(rest):
        return tuple_to_cons(rest);
    else:
        return None;
    #endif
#enddef
