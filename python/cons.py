class Cons:
    Cons.__slots__ = ("_car","_cdr");
    def Cons.__init__(self, car, cdr):
        self._car = car
        self._cdr = cdr        
    #enddef
    #just for the sake of consistancy
    def Cons.cdr(self):
        return self.cdr;
    #enddef
    def Cons.car(self):
        return self.car;
    #enddef
    
        
     pass
# cons.__slots__=("car","cdr");
Cons = type('Cons',(object,),dict(__slots__=("car","cdr")));
