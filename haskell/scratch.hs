--Some Random Functions and algorithms to practice haskell
module Scratch(rk4,f'_5)
 where
  import System.Random
  import Data.List(genericLength)

  fib 0=1
  fib 1=1
  fib x= fib (x-1) + fib (x-2)

  quickSort [x]=[x]
  quickSort []=[]
  quickSort (x : xs) = (quickSort less) ++ (x : equal) ++ (quickSort more)
            where less = filter (< x) xs
                  equal = filter (== x) xs
                  more = filter (>x) xs
--Integrals
  left_diff y i=sum(init y)*(i/genericLength (y-2))
  right_diff y i=sum(tail y)*(i/genericLength (y-2))
  trap y i=left_diff y i + right_diff y i / 2
  midpt ym i=sum(ym) * (i/genericLength(ym-2))
  simpson y i=2*midpt(y i) + trap(y i)
--quad f x=sum f(x) * w(x)
--     where w=
--ODEs
  rk4 t dt y f=y+ dt/6 *(s1 + 2s2 + 2s3 + s4)--return next iteration
       where s1=f(t f(t y))
             s2=f((t + dt/2) (y+ dt/2 * s1))
             s3=f((t + dt/2) (y+ dt/2 * s2))
             s4=f((t + dt) (y + dt * s3))
  --rk4 0 t dt y f=y --return nth iteration
  --rk4 n t dt y f=rk4 n-1 t+dt dt (y+dt/6*(s1 +2s2 +2s3 +s4)) f
  --  where s1=f(t,f(t,y))
  --        s2=f(t + dt/2,y+ dt/2 *s1)
  --        s3=f(t + dt/2,y+ dt/2 *s2)
  --        s4=f(t + dt,y + dt *s3)
  --rk4_list 0 t dt y f=rk4 t dt y f --returns a list of n values hopefully
  --rk4_list n t dt y f=y ++ rk4_list((n-1) (t+dt) dt (rk4 t dt y f) f)

--Deritives

  f'_5  f x h=(-(f(x+2*h))+(8*f(x+h))-(8*f(x-h))+(f(x-2*h)))/12*h
  f''_5 f x h=(-f(x+2*h)+16*f(x+h)-30*f(x) + 16*f(x-h)-f(x-2*h))/12*h**2
  f'''_5 f x h=(f(x+2*h)-2*f(x+h)+2f(x-h)-f(x-2h))/2h**3
  f''''_5 f x h=(f(x+2*h)-4*f(x+h)+6*f(x)-4*f(x-h)+f(x-2*h))/h**4
