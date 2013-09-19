module Uncertanity
import Base.log,Base.exp
export Error_Var
immutable Var{T}
    Value::T
    Error::T
end
typealias Error_Var Var
mkArray{T}(x::Array{T},y::Array{T})=[Var(x[i],y[i]) for i=1::length(x)]
sqrtSoS(a...)=sqrt(mapreduce(x->x^2,+,a))
#(defun Error_Var_op (op error)
#(insert (format "\n%s(x :: Var, y :: Var) =\nVar(%s(x.Value,y.Value),%s)\n"
#         op op error)))
#(defun Error_Var_Num_op (op type error)
#(insert (format "\n%s(x :: Var, y :: %s) =\nVar(%s(x.Value,y),%s)\n"
#         op type op error)))
+{T}(x :: Var{T}, y :: Var{T}) = 
Var(+(x.Value,y.Value),sqrtSoS(x.Error,y.Error))
+{T}(x :: Var{T}, y :: Integer) =
Var(+(x.Value,y),x.Error)
+{T}(x :: Var{T}, y :: Number) =
Var(+(x.Value,y),x.Error)
+{T}(y :: Integer,x :: Var{T}) =
Var(+(x.Value,y),x.Error)
+{T}(y :: Number,x :: Var{T}) =
Var(+(x.Value,y),x.Error)

*{T}(x :: Var{T}, y :: Var{T}) =
Var(*(x.Value,y.Value),
    *(sqrt(^(*(x.Value,y.Value),2)),
      sqrtSoS(/(x.Error,x.Value),/(y.Error,y.Value))))
*{T}(x :: Var{T}, y :: Integer) =
Var(*(x.Value,y),sqrt(*(x.Error^2,y^2)))
*{T}(y :: Integer,x :: Var{T}) =
Var(*(x.Value,y),sqrt(*(x.Error^2,y^2)))
*{T}(x :: Var{T}, y :: Number) =
Var(*(x.Value,y),sqrt(*(x.Error^2,y^2)))
*{T}(y :: Number,x :: Var{T}) =
Var(*(x.Value,y),sqrt(*(x.Error^2,y^2)))

-{T}(x :: Var{T}, y :: Var{T}) =
Var(-(x.Value,y.Value),sqrtSoS(x.Error,y.Error))
-{T}(x :: Var{T})=Var(-x.Value,-x.Error)
-{T}(x :: Var{T}, y :: Integer) =
Var(-(x.Value,y),x.Error)
-{T}(x :: Var{T}, y :: Number) =
Var(-(x.Value,y),x.Error)

/{T}(x :: Var{T}, y :: Var{T}) =
Var(/(x.Value,y.Value),
    *(sqrt(^(/(x.Value,y.Value),2)),
      sqrtSoS(/(x.Error,x.Value),/(y.Error,y.Value))))
/{T}(x :: Var{T}, y :: Integer) =
Var(/(x.Value,y),sqrt(*(x.Error^2,y^2)))
/{T}(y :: Integer,x :: Var{T}) =
Var(/(x.Value,y),sqrt(*(x.Error^2,y^2)))
/{T}(x :: Var{T}, y :: Number) =
Var(/(x.Value,y),sqrt(*(x.Error^2,y^2)))
/{T}(y :: Number,x :: Var{T}) =
Var(/(x.Value,y),sqrt(*(x.Error^2,y^2)))

^{T}(x :: Var{T}, y :: Integer) =
Var(^(x.Value,y),*(y,*(^(x.Value,y),/(x.Error,x.Value))))
^{T}(x :: Var{T}, y :: Number) =
Var(^(x.Value,y),*(*(y,(^(x.Value,y-1)),x.Error)))

log{T<:Number}(x::Var{T})=Var(log(x.Value),x.Error/log(x.Value))
exp{T<:Number}(x::Var{T})=Var(exp(x.Value),exp(x.Value)*x.Error)

end