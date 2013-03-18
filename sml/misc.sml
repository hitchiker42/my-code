structure Misc = struct
fun curry f x y = f(x,y)
fun car x = hd x
fun cdr x = tl x
fun cons x y = x::y
val list = fn z => Foldr.foldr ([], fn l => l) z
val ` = fn z => Foldr.step1 (op ::) z
fun add x y = x + y
fun sub x y = x - y
fun eql x y = x = y
fun mult x y = x * y
fun div x y = x / y
fun last n::[] = n
  | last x::xs = last (cdr xs)
fun foldl f b l = let
      fun f2 ([], b) = b
        | f2 (a :: r, b) = f2 (r, f (a, b))
      in
        f2 (l, b)
      end
fun foldr f b l = foldl f b (rev l)
end
fun $ (a, f) = f a
fun id x = x
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