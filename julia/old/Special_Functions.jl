module Special_Functions
#mathmatical special functions written in julia
#many algorithms taken from the special functions portion of the gsl
export Lagurre,Legendre,Hermite,Chebyshev
module Lagurre
function L(n::Integer,x::Float64)
    if (n == 0) return 1
    elseif (n == 1) return -1+x
    elseif (n == 2) return 0.5*(x^2-4x+2)
    elseif (n == 3) return (1/6)*(-x^3+9x^2-18x+6)
    elseif (n == 4) return (1/24)*(x^4'-16x^3+72x^2-96x+24)
    elseif (n == 5) return (1/120)*(-x^5+25x^4-200x^3+600x^2-600x+120)
    else 
        let L_n2=1,L_n1=1-x
            for i=2:n
                L_n=(1/i)*((2*(i-1)+1-x)*L_n1-(i-1)*L_n2)
                L_n2=L_n1
                L_n1=L_n
            end#for
            return L_n
        end#let
    end#if
end#L
end#module Lagurre
module Legendre
export P_lm,P_l,P_mm
gsl_Plm(l,m,x)=
    ccall((:gsl_sf_legendre_Plm,"libgsl"),
          Float64,(Int32, Int32 ,Float64),l,m,x)
gsl_Pl(l,x)=
    ccall((:gsl_sf_legendre_Pl,"libgsl"),
          Float64,(Int32, Float64),l,x)
function Pl(l::Integer,x::Float64)
    if(l<0 || x <-1.0 || x>1.0)
          #error
    elseif (l == 0) return 1.0
    elseif (l == 1) return x
    elseif (l == 2) return 0.5 * (3.0*x*x - 1.0)
                           # eps()*(abs(3.0*x^2)+1))
    elseif (x == 1.0) return 1.0
    elseif (x == -1.0) return (l&1 == 1?-1.0:1.0)
    elseif (l < 100000)
        #following the gsl code we use recursion for relatively small l
        let P_lm2=1.0,P_lm1=x,P_l=x#,e_lm2=eps(),e_lm1=abs(x)*eps(),e_l=e_lm1
            for i=2:l
                P_l=(x*(2*i-1)*P_lm1 - (i-1)*P_lm2)/i
                P_lm2=P_lm1
                P_lm1=P_l
                # e_l=0.5*(abs(x)*(2*i-1.) - e_lm1+(i-1.)*e_lm2)/i
                # e_lm2=e_lm1
                # e_lm1=e_l
            end#for
            return P_l#,e_l+l*abs(P_l)*eps())
        end#let
    else #asymptotic expansion, not really sure why this works
        let u=l+0.5,th=acos(x),J0=besselj0(u*th),Jm1=besselj(-1,u*th)
            if(th < eps^0.25)
                B00=(1. + th*th/15.0)/24.0
                pre=1. + th*th/12.0
            else
                let sin_th=sqrt(1.-x*x),cot_th=x/sin_th
                    B00 = 1./8. * (1. -th *cot_th)/(th*th)
                    pre = sqrt(th/sin_th)
                end#let
            end#if
                c1=th/u*B00
                return pre * (J0 + c1 * Jm1)
        end#let
    end#if
end#P_l
P_l=Pl
function P_mm(m::Integer,x::Float64)#P_lm for l == m
    if(m == 0) return 1.0
    else
        let p_mm=1.0,root_factor=sqrt(1.-x)*sqrt(1.+x),fact_coeff=1.0
            for i=1:m
                p_mm *= -fact_coeff * root_factor;
                fact_coeff += 2.0
            end#for
            return p_mm
        end#let
    end#if
end#P_mm
function P_lm(l::Integer,m::Integer,x::Float64)
    #gsl checks for overflow, I think julia can handle that
    let p_mm=P_mm(m,x),p_mmp1=x*(2m+1)*p_mm
        if(l == m) return p_mm
        elseif (l == m+1) return p_mmp1
        else #recursion 
            #its a bit weird, so I'm going to write it our
            #(l-m)*P_lm = (2l-1)*x*P_(l-1)m - (l+m-1)*P_(l-2)m
            let p_llm2=p_mm,p_llm1=p_mmp1,p_ll=0.0
                for i=m+2:l
                    p_ll=(x*(2*i-1)*p_llm1 - (i+m-1)*p_llm2)/(i-m)
                    p_llm2=p_llm1
                    p_llm1=p_ll
                end#for
                return p_ll
            end#let
        end#if
    end#let
end#P_lm
P_lm(l::Integer,m::Integer,x::Array{Float64,1})=[P_lm(l,m,x[i])for i=1:length(x)]
P_l(l::Integer,x::Array{Float64,1})=[P_lm(l,x[i])for i=1:length(x)]
P_lm(l::Integer,m::Integer,x::itr{Number})=[P_lm(l,m,i) for i=x]
P_l(l::Integer,x::itr{Number})=[P_lm(l,i) for i=x]
end#module ledengre
module Hermite
    function H_phys(x::Number,n::Integer)
        if(n==0) return 1
        elseif(n==1) return 2x
        else
            let H_nm2=1,H_nm1=2x,H_n=4x^2-2
                for i=2:n
                    H_n=x*H_nm1-(2*i)*H_nm2
                    H_nm1=H_n
                    H_nm2=H_nm1
                end#for
                return H_n
            end#let
        end#else
    end#H_phys
    function H_prob(x::Number,n::Integer)
        if(n==0) return 1
        elseif(n==1) return x
        else
            let H_nm2=1,H_nm1=x,H_n=x^2-1
                for i=2:n
                    H_n=2x*H_nm1-2i*H_nm2
                    H_nm1=H_n
                    H_nm2=H_nm1
                end
                return H_n
            end
        end
    end
end#module Hermite
module Chebyshev
   function Tn(x::Number,n::Integer)
       if(n==0) return 1
       elseif(n==1) return x
       else
           let T_nm2=1,T_nm1=x,T_n=2x^2-1
                   for i=2:n
                       T_n=2x*T_nm1-T_nm2
                       T_nm1=T_n
                       T_nm2=T_nm1
                   end#for
               return T_n
           end#let
       end#if
   end#Tn               
   function Un(x::Number,n::Integer)
       if(n==0) return 1
       elseif(n==1) return 2x
       else
           let U_nm2=1,U_nm1=2x,U_n=4x^2-1
               for i=2:n
                   U_n=2x*U_nm1-U_nm2
                   U_nm1=U_n
                   U_nm2=U_nm1
               end#for
           end#let
           return U_n
       end#if
   end#Un
   function T_roots(n::Integer,a::Integer,b::Integer)
       result=Array(Float64,(n,))
       if(a==-1 && b==1)
           for i=1:n
               result[i]=cos((2i-1/2n)*pi)
           end
       else
           bma=(b-a)*0.5
           bpa=(b+a)*0.5
           for i=1:n
               result[i]=bpa+(bma*cos((2i-1/2n)*pi))
           end
       end
       return result
   end
       
end#module Chebyshev
end