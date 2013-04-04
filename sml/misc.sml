(*sig
    structure Fold : <sig>
    structure Foldr : <sig>
    val load : string -> unit
    val inc : (int * int) ref -> unit
    val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
    val car : 'a list -> 'a
    val cdr : 'a list -> 'a list
    val cons : 'a -> 'a list -> 'a list
    val des_car : 'a list ref -> 'a
    val des_hd : 'a list ref -> 'a
    val seq : int -> int -> int list
    val min : int * int -> int
    val max : int * int -> int
    val dec : int -> int list
    val arr_to_list : 'a array -> 'a list
    val sl_to_list : 'a ArraySlice.slice -> 'a list
    val arr_swap : 'a array -> int -> int -> unit
    val sl_swap : 'a ArraySlice.slice
                  -> 'a ArraySlice.slice -> int -> int -> unit
    val list : (('a -> 'a) * (('b list -> 'c) -> 'c) -> 'd) -> 'd
    val ` : ('a list -> 'b) * 'c -> 'a -> (('a list -> 'b) * 'c -> 'd) -> 'd
    val add : int -> int -> int
    val sub : int -> int -> int
    val eql : ''a -> ''a -> bool
    val mult : int -> int -> int
    val dv : int -> int -> int
    val is_ws : char -> bool
    val strip : string -> char list
    val last : 'a list -> 'a
    val most : 'a list -> 'a list
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val loop : 'a -> 'b -> ('a -> bool) -> ('a -> 'a) -> ('b -> 'b) -> 'b
    val for : int -> ('a -> 'a) -> 'a -> 'a list
    val odd : int -> int list list
    val run_tests : (unit -> bool) list -> string list
    val print_list : string list -> unit
    val $ : 'a * ('a -> 'b) -> 'b
    val id : 'a -> 'a
    structure Printf : <sig>*)
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
fun inc (y:(int*int) ref) = y:=((#1(!y))+1,#2(!y))
fun curry f x y = f(x,y)
(*lisp names*)
fun car x = hd x
fun cdr x = tl x
fun cons x y = op :: (x,y)
(*pop head off of a mutable list & return the removed element*)
fun des_car x = (x:=(cdr (!x));car(!x))
val des_hd = des_car
(*return list 1::2::3...::n::[]*)
fun seq m n = let
    val stop = m
    fun build n l = if n=m then l else build (n-1) (n::l)
in build n [] end
(*return a list n::n-1::n-2...0::[]*)
val min = fn (i,j)=>if (i<j) then i else j
val max = fn (i,j)=>if (i>j) then i else j
fun dec n = let
    val i = ref 0
    val l = ref []
    fun build i l n = if (!i)>n then (!l)
                      else (l:=((!i)::(!l)); i:=((!i)+1);build i l n)in build i l n end
val arr_to_list = fn y=>Array.foldr (op ::) [] y
val sl_to_list = fn y=>ArraySlice.foldr (op ::) [] y

fun arr_swap arr i j = let
    val tempi = Array.sub (arr,i)
    val tempj = Array.sub (arr,j)
in (Array.update(arr,i,tempj);Array.update(arr,j,tempi)) end
(*swap element k in array slice i with element l from array slice j*)
fun sl_swap i j k l= let
    val tempi = ArraySlice.sub(i,k)
    val tempj= ArraySlice.sub(j,l)
in (ArraySlice.update(i,k,tempj);ArraySlice.update(j,l,tempi)) end
val list = fn z => Foldr.foldr ([], fn l => l) z
val ` = fn z => Foldr.step1 (op ::) z
(*prefix operators from builtin infixes*)
fun add x y = op + (x,y)
fun sub x y = op - (x,y)
fun eql x y = op = (x,y)
fun mult x y = op * (x,y)
fun dv x y = op div (x,y)
fun is_ws chr = if ord chr = 0x20 orelse 0x09<=(ord chr) 
                                         andalso (ord chr)<=0x0D then true
                else false
fun strip str =let
    val chars = explode str
    val f = (not o is_ws)
in List.filter f chars end
(*last element of a list*)
fun last (n::[]) = n
  | last [] = raise Empty
  | last (x::xs) = last (cdr xs)
(*fun most l = if length l > 1 then List.take(l,List.length l - 1) else l*)
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
fun loop n x f g h = if f n then h x else loop (g n) (h x) f g h
       (*int n, list x, test fxn y, increment fxn g, and do fxn h*)
fun for n f x = (*do f x n times*) if eql 0 n then x::[] else for (n-1) f (f(x))
fun odd n = let
    val f = fn (x::xs) => ((x+2)::x::xs)
in for n f (3::2::[])
end

fun run_tests (t:(unit->bool) list) = let
    val tests = ListPair.zip (t,(List.map Int.toString (seq 1 (List.length t))))
    fun test (t,i) = if t() then concat ("Test"::i::"Passed"::[]) else
                     concat ("Test"::i::"Failed"::[])
in List.map test tests end
fun print_list (l:string list)=let
    val strings=ref l
in while not ((!strings)=[]) do
         print (concat ((des_car(strings))::"\n"::[])) end
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
