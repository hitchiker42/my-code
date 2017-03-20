module Uncertanity
export Error_Var
immutable Var{T}
    Value::T
    Error::T
end
typealias Error_Var Var
function sqSum(a,b,c...)
    if c == ()
        a
    else
        sqSum(a+(b^2),c...)
    end
end
function sqrtSoS(a,rest...)
    sqrt(sqSum((a^2),rest...))
end
#(defun Error_Var_op (op error)
#(insert (format "\n%s(x :: Var, y :: Var) =\nVar(%s(x.Value,y.Value),%s)\n"
#         op op error)))
#(defun Error_Var_Num_op (op type error)
#(insert (format "\n%s(x :: Var, y :: %s) =\nVar(%s(x.Value,y),%s)\n"
#         op type op error)))
#(Error_Var_op "+" "sqrtSoS(x.Error,y.Error")
+(x :: Var, y :: Var) =
Var(+(x.Value,y.Value),sqrtSoS(x.Error,y.Error))
#(Error_Var_Num_op "+""Integer" "sqrt(x.Error^2,y^2)")
*(x :: Var, y :: Integer) =
Var(*(x.Value,y),sqrt(*(x.Error^2,y^2)))
*(y :: Integer,x :: Var) =
Var(*(x.Value,y),sqrt(*(x.Error^2,y^2)))
#(Error_Var_Num_op "+""Number" "sqrt(x.Error^2,y^2)")
*(x <: Var, y :: Number) =
Var(*(x.Value,y),sqrt(*(x.Error^2,y^2)))
*(y <: Number,x <: Var) =
Var(*(x.Value,y),sqrt(*(x.Error^2,y^2)))
+(x <: Var, y <: Var) = 
Var(+(x.Value,y.Value),sqrtSoS(x.Error,y.Error))
+(x <: Var, y <: Integer) =
Var(+(x.Value,y),x.Error)
+(x <: Var, y <: Number) =
Var(+(x.Value,y),x.Error)
-(x <: Var, y <: Var) =
Var(-(x.Value,y.Value),sqrtSoS(x.Error,y.Error))
-(x <: Var, y <: Integer) =
Var(-(x.Value,y),x.Error)
-(x <: Var, y <: Number) =
Var(-(x.Value,y),x.Error)
*(x <: Var, y <: Var) =
Var(*(x.Value,y.Value),
    sqrtSoS(/(x.Error,x.Value),/(y.Error,y.Value),
            *(x.Value,y.Value)))
^(x <: Var, y <: Integer) =
Var(^(x.Value,y),*(y,*(^(x.Value,y),/(x.Error,x.Value))))
^(x <: Var, y <: Number) =
Var(^(x.Value,y),*(*(y,(^(x.Value,y-1)),x.Error)))
log(x<:Var)=Var(log(x.Value)x.Error/log(x.Value))
exp(x<:Var)=Var(exp(x.Value),exp(x.Value)*x.Error)
end