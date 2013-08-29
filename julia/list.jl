module List
export cons,car,cdr,cadr,caddr,list,nil,fold
abstract _list
type _nil <: _list
end
nil = _nil()
type cons <: _list
    car
    cdr::_list
end
car=x -> x.car
cdr=x -> x.cdr
cadr=x::_list -> car(cdr(x))
caddr=x::_list -> car(cdr(cdr(x)))
#function rev
function list(a,b,c...) 
    function acc(ls,l,rest...)
        rest == () ? cons(l,ls) : acc(cons(l,ls),rest...)
    end
    acc(cons(a,nil),b,c...)
end    
function fold (l::_list,b,f)
    function loop(ls::_list,acc)
        (list == nil()) ? acc : loop(cdr((ls),f(car(ls),acc)))
    end
    loop(l,b)
end
end
