(* Assignment #6: SML 2 *)

(* Sorting functions *)
val _ = use "misc.sml"
fun quickSort filt (x::xs) = let
    val f = List.filter
    fun qs [] = []
      | qs (x::[]) = (x::[])
      | qs (x::xs) = let
          val pred = fn y => filt (y,x)
      in qs (f pred xs)@x::(qs (List.filter (not o pred) xs)) end
in qs (x::xs) end

(*    fun partition pred l = let
          fun loop ([],trueList,falseList) = (rev trueList, rev falseList)
            | loop (h::t,trueList,falseList) = 
                if pred h then loop(t, h::trueList, falseList)
                else loop(t, trueList, h::falseList)
          in loop (l,[],[]) end
*)
fun mergeSort filt lst = let
    val filt = filt
    val len = List.length
    val lst=lst
    fun merge (l,r) = let
        val res = ref []
        fun merge2 [] [] x = rev (!x)
          | merge2 l [] x = rev (l@(!x))
          | merge2 [] r x = rev (r@(!x))
          | merge2 (l::ls) (r::rs) x = if (filt (l,r)) then
                                           (x:=(l::(!x));merge2 ls (r::rs) x)
                                       else (x:=(r::(!x));merge2 (l::ls) rs x)
in merge2 l r res end
    fun mergesort l = if len l = 1 then l else
                      let val mid = len l div 2
                          val left = ref (List.take (l,mid))
                          val right = ref (List.drop (l,mid))
                      in (left:=mergesort (!left);
                          right:=mergesort (!right);
                          if filt(List.last (!left),hd (!right))
                          then (!left)@(!right) else
                          merge ((!left),(!right))) end
in mergesort lst end
fun split [] = ([],[])
  | split [x] = ([x],[])
  | split l = let
      fun loop ([],l,r,_)= (rev l,rev r)
        | loop ((x::xs),l,r,true)= loop(xs,(x::l),r,false)
        | loop ((x::xs),l,r,false) = loop(xs,l,(x::r),true)
  in loop (l,[],[],true) end

exception CannotChange
(*I must admit I found most of this code 
from http://www.cs.cmu.edu/~rwh/smlbook/examples/excs.sml,
I could have ignored it and tried to make the mess of code
seen in the comment below something workable, but seening the 
simplicity of this implementation I couldn't not just use it.
I did modify it so that it uses a finite set of coins, whereas the 
original used a limitless number of each of the given coins*)
fun change _ 0 = nil
  | change nil _ = raise CannotChange
  | change (coin::coins) amt =
    if coin > amt then
       change coins amt
    else
       (coin :: change (coins) (amt-coin))
       handle CannotChange => change coins amt
(*fun change _ 0 = []
  | change [] x = raise CannotChange
  | change l x = let
      val total=x
      (*should turn l into a list of decending integers less than x*)
      val ls=quicksort (op >) (List.filter l (fn y=>(y<=total)))
      val vec=VectorSlice.full (Vector.fromList ls)
      val sum=Misc.sum
      val Some=SOME val None=None
      fun test v 0 = Some v (*returns wrong thing*)
      fun test v x = let 
          val len = VectorSlice.length v
          val i = 0
          val y = x - VectorSlice.sub 0
      in while true do (if i > len then None else
                         r:= test VectorSlice.slice (v,i,None);(*Note Not finished like this*)
                         if (!r) = None then val i = i+1(*is this legal?*)
                         else (!r)) (*need to figure out how to return*) end
      fun result None = raise Cannot Change
        | result Some(x) = x
  in result (test vec x) end*)
(*what we want to do is test x with all permutations of values in xs to see if
some combination adds up to z, we store intrum combinations in y and decrement
z by the values in y as we go along.
Should be something like if some y in xs is <z then z-=y and if some q in
(xs - y & values too big) <z etc etc...*)
(*        | test (x::xs) y z = if (sum y) = z then z else
                       let
                           val ls2=List.filter xs (fn q =>((q+x)<=z))*)
                           (* for x in ls2 test x::rest*)

(*Right now this gets stuck*)
fun changeBest coins amt = let
      val results = ref []
      val i = ref 0
      val len = List.length coins
      val choices = Array.fromList coins
      fun max [] m = m
        | max (l::ls) m = if (List.length l) > (List.length m) then
                              max ls l else max ls m
      fun change2 _ 0 = nil
        | change2 nil _ = raise CannotChange
        | change2 (coin::coins) amt =
          if coin > amt then
              change2 coins amt
          else
              (coin :: change2 (coins) (amt-coin))
              handle CannotChange => if coins = [] then [] 
                                     else change2 coins amt
in ((while ((!i)<len) do results:=((change2 (Misc.rot choices (!i))amt)::(!results));i+=1);
    results:= List.filter (fn x => x<>[]) (!results);
    if (!results) = [] then raise CannotChange else max (!results)) end
(* Sudoku *)

(*val _ = use "sudoku-sig.sml";
val _ = use "listSudoku.sml";*)
