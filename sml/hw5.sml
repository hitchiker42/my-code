structure hw5 = struct
fun curry f x y = f(x,y)
fun expand f (x,y) = f x y
(*= is binding eq is testing*)
fun eq x y = x=y
fun sub x y = x-y
(*last is already a fxn in the basis library....
to be honest this is the exact same one as in the basis library
there really aren't many other ways to write it*)

(* heres a length function
fun length l = let
      fun len(n, []) = n
        | len (n, [x]) = n + 1
        | len (n,x::xs)=len (n+1,xs)
      in
        len (0, l)
      end
Now, I'm going to use the standard one since its more efficent
and makes use of some lower level stuff to keep 
things from over flowing*)
fun last [] = raise exception
| last [x] = x
| last (_::x) = last x;
fun most l = if length l > 1 then take(l,List.length l - 1) else l
fun most [] = raise empty
  | most x::[] = []
  | most (x::y::[]) = x
  | most (x::y::z) = x::y::most z
fun natList 0 = []
  | natList 1 = 1::[]
  | natList n = let
(*tail recursion I think*)
      fun acc 0 r = r
        | acc n r = acc (n-1) (n::r)
  in rev (acc n [])
(*this one is kinda silly, there are a million ways to do this*)
fun sum [] = 0
  | sum (x::[]) = x
  | sum (x::xs) = x + sum xs
fun prod [] = 1
  | prod (x::[]) = x
  | prod (x::xs) = x * prod xs
fun sum2 [] = 0
  | sum2 x::xs = sum x + sum2 xs
(*Another one thats also a basis function
, the basis function compares the char code
to a set of ascii codes for membership,
thouch in a different way than this*)
fun isLetter c =
    if (ord c >=0x41 && ord c <=0x5A) || (ord c >=0x61 && ord c <=0x7A)
    then true else false
(* also a basis function*)
fun toLower c =
    if isLetter c then
        if (ord c >=0x41 && ord c <=0x5A)
        then chr (ord c + 0x20)
        else c
    else 
fun palindrome s = let
    val ca=explode s
    fun compress [] = []
      | compress (c::ca)  = if isLetter c than c::compress ca else compress ca
    val c = compress ca
    fun take l = if eq hd l last l then most(tl l) else []
    fun test l = if eq length l 1 then true else
                 if eq length 2 l then eq hd l tl l else test take l
in test c
(*move n-1 disks to B;
  move disk n to C;
  disk A is now B and disk B is a
  subtract 1 from n and repeat*)
fun hanoi 1 A B C = (A,C)::[] 
  | hanoi n A B C = hanoi (n-1) A C B 
                          (A,C)::hanoi (n-1) B C A
(*I think this works...*)
                               
(*fun muitiply int*int list -> int
for i in int*int list:
  prod*=int**int*)
fun intpow n e =
    let
	fun acc 0 r e = r
          | acc n r e = acc (n-1) (r*e) e
    in acc (e-1) n n
fun powint (n,e) = expand intpow (n,e)
fun multiply fact =
    let fun acc [] r  = r
          | acc (l::ls) r  = acc ls r*powint(l)
    in acc fact 1
end
factor
(*really simple trial division
[n|n<-ns,rem n p > 0]*
in ml something like *)
fun factor n = let
    val test=natlist(introot n)
    val remtest=fn (x)=>n mod y = 0

(*(defun factors (n &aux (lows '()) (highs '()))
  (do ((limit (isqrt n)) (factor 1 (1+ factor)))
      ((= factor limit)
       (when (= n (* limit limit))
         (push limit highs))
       (nreconc lows highs))
    (multiple-value-bind (quotient remainder) (floor n factor)
      (when (zerop remainder)
        (push factor lows)
        (push quotient highs)))))*)*)
(*Input: an integer n > 1
 
Let A be an array of Boolean values, indexed by integers 2 to n,
initially all set to true.
 
for i = 2, 3, 4, ..., √n :
  if A[i] is true:
    for j = i2, i2+i, i2+2i, ..., n:
      A[j] := false
 
Now all i such that A[i] is true are prime.*)
hanoi
(*(defun move (n from to via)
  (cond ((= n 1)
         (format t "Move from ~A to ~A.~%" from to))
        (t
         (move (- n 1) from via to)
         (format t "Move from ~A to ~A.~%" from to)
         (move (- n 1) via to from))))*)
fun introot n=Real.floor (Math.sqrt (Real.fromInt n))
fun primes n = let
    val filt=fn (x,n) => Int.mod x n = 0
    val test = natlist(n)
    val max = introot n
