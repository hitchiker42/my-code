structure MiscList = struct
(*these 2 structures are from mlton*)
val _ = use "misc.sml"
val _ = use "Misc.sml"
open Misc
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
(*lisp names*)
fun car x = hd x
fun cdr x = tl x
fun cons x y = op :: (x,y)
(*pop head off of a mutable list & return the removed element*)
fun des_car x = (x:=(cdr (!x));car(!x))
val des_hd = des_car
val arr_to_list = fn y=>Array.foldr (op ::) [] y
val sl_to_list = fn y=>ArraySlice.foldr (op ::) [] y

(*shuffle order of arr, arr is modified in place & fxn returns ()*)
fun shuffle arr =
    let 
        val i = ref (Array.length arr - 1)
        val k = ref (Random.randRange (0,(!i)) rand)
        val temp = ref (Array.sub(arr,(!i)))
in (while ((!i)>1) do (Array.update (arr,(!i),Array.sub (arr,(!k)));
                    Array.update (arr,(!k),(!temp));
                    i-=1;k:=Random.randRange(0,(!i)) rand;
                    temp:=Array.sub(arr,(!i)))) end
(*shuffles list via intermediate array*)
fun listShuffle l = let
    val arr = Array.fromList l
in (shuffle arr;arr_to_list arr) end
(*return last element of list*)
fun last (n::[]) = n
  | last [] = raise Empty
  | last (x::xs) = last (cdr xs)
(*fun most l = if length l > 1 then List.take(l,List.length l - 1) else l*)
(*return all but the last element of list*)
fun most [] = raise List.Empty
  | most (x::[]) = []
  | most (x::y::[]) = x::[]
  | most (x::z) = x::(most z)
fun foldl f b l = let 
    fun f2 ([], b) = b
      | f2 (a :: r, b) = f2 (r, f (a, b))
in
    f2 (l, b)
end
fun foldr f b l = foldl f b (rev l)
(*return a list n::n-1::n-2...0::[]*)
fun dec n = let
    val i = ref 0
    val l = ref []
    fun build i l n = if (!i)>n then (!l)
                      else (l:=((!i)::(!l)); i:=((!i)+1);build i l n)in build i l n end
val arr_to_list = fn y=>Array.foldr (op ::) [] y
val sl_to_list = fn y=>ArraySlice.foldr (op ::) [] y
(*swap element i with j in array arr*)
fun arr_swap arr i j = let
    val tempi = Array.sub (arr,i)
    val tempj = Array.sub (arr,j)
in (Array.update(arr,i,tempj);Array.update(arr,j,tempi)) end
(*swap element k in array slice i with element l from array slice j*)
fun sl_swap i j k l= let
    val tempi = ArraySlice.sub(i,k)
    val tempj= ArraySlice.sub(j,l)
in (ArraySlice.update(i,k,tempj);ArraySlice.update(j,l,tempi)) end
(*given an array an and index i return
arr[i]::arr[i+1]...arr[len]::arr[0]...arr[i]*)
fun rot arr i = let 
    val mid = Array.sub (arr,i)
    val left = ArraySlice.slice (arr,0,SOME(i))
    val right = ArraySlice.slice (arr,(i+1),NONE)
in mid::(sl_to_list right)@(sl_to_list left) end
(*need to check mlton site to see what these do*)
val list = fn z => Foldr.foldr ([], fn l => l) z
val ` = fn z => Foldr.step1 (op ::) z
(*return list 1::2::3...::n::[]*)
fun seq m n = let
    val stop = (m-1)
    fun build n l = if n=stop then l else build (n-1) (n::l)
in build n [] end
fun sum l = foldl (op +) 0 l
end
