module List
export cons,car,cdr,list
abstract _list
type nil <: _list
end
_nil = nil()
type cons <: _list
    car
    cdr::_list
end
car(x::_list)=x.car
cdr(x::_list)=x.cdr
function list(a,b,c...) 
    function acc(ls,l,rest...)
        rest == () ? cons(l,ls) : acc(cons(l,ls),rest...)
    end
    acc(cons(a,_nil),b,c...)
end    
end