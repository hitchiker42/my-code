structure Misc = struct
structure Fold =
   struct
      fun fold (a, f) g = g (a, f)
      fun step0 h (a, f) = fold (h a, f)
      fun step1 h (a, f) b = fold (h (b, a), f)
   end
structure Foldr =
   struct
      fun foldr (a, f) = Fold.fold (f, fn g => g a)
      fun step0 h = Fold.step0 (fn g => g o h)
      fun step1 h = Fold.step1 (fn (b, g) => g o (fn a => h (b, a)))
   end
val load = use
fun switch (x::xs) l = switch xs ((!x)::l)
fun inc (y:(int*int) ref) = y:=((#1(!y))+1,#2(!y))
fun curry f x y = f(x,y)
fun car x = hd x
fun cdr x = tl x
fun cons x y = op :: (x,y)
fun des_car x = (x:=(cdr (!x));car(!x))
fun seq n = let
    fun build 0 l = l
      | build n l = build (n-1) (n::l)
in build n [] end
fun dec n = let
    val i = ref 0
    val l = ref []
    fun build i l n = if (!i)>n then (!l)
                      else (l:=((!i)::(!l)); i:=((!i)+1);build i l n)in build i l n end
fun arr_swap arr i j = let
    val tempi = Array.sub (arr,i)
    val tempj = Array.sub (arr,j)
in (Array.update (arr,i,tempj);Array.update (arr,j,tempi)) end
val list = fn z => Foldr.foldr ([], fn l => l) z
val ` = fn z => Foldr.step1 (op ::) z
fun add x y = op + (x,y)
fun sub x y = op - (x,y)
fun eql x y = op = (x,y)
fun mult x y = op * (x,y)
fun dv x y = op div (x,y)
fun last (n::[]) = n
  | last [] = raise Empty
  | last (x::xs) = last (cdr xs)
fun foldl f b l = let
    fun f2 ([], b) = b
      | f2 (a :: r, b) = f2 (r, f (a, b))
in
    f2 (l, b)
end
fun foldr f b l = foldl f b (rev l)
fun loop n x f g h = if f n then h x else loop (g n) (h x) f g h
       (*int n, list x, test fxn y, increment fxn g, and do fxn h*)
fun for n f x = (*do f x n times*) if eql 0 n then x::[] else for (n-1) f (f(x))
fun odd n = let
    val f = fn (x::xs) => ((x+2)::x::xs)
in for n f (3::2::[])
end
fun $ (a, f) = f a
fun id x = x
structure Printf =
   struct
      fun fprintf out =
         Fold.fold ((out, id), fn (_, f) => f (fn p => p ()) ignore)

      val printf = fn z => fprintf TextIO.stdOut z

      fun one ((out, f), make) =
         (out, fn r =>
          f (fn p =>
             make (fn s =>
                   r (fn () => (p (); TextIO.output (out, s))))))

      val ` =
         fn z => Fold.step1 (fn (s, x) => one (x, fn f => f s)) z

      fun spec to = Fold.step0 (fn x => one (x, fn f => f o to))

      val B = fn z => spec Bool.toString z
      val I = fn z => spec Int.toString z
      val R = fn z => spec Real.toString z
   end
end
