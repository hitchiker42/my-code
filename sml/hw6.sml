(*
signature HW6 = sig
val mergeSort: ('a * 'a -> bool) -> 'a list -> 'a list
val quickSort: ('a * 'a -> bool) -> 'a list -> 'a list
val change: int list -> int -> int list
val changeBest: int list -> int -> int list
end
*)
structure hw6 : HW6 =
struct
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
fun mergeSort filt l = let
        val len = ArraySlice.length
        val get = ArraySlice.sub
        val base = ArraySlice.base
        fun slice (sl,i,sz)=ArraySlice.subslice(sl,i,SOME(sz))
        val return = Array.fromList l
        val arr=ArraySlice.full(return)
        fun merge l r sl = let
            val i = ref ((Misc.min(len l,len r))-1)
        in
            (while (!i)>=0 do
                         if filt(get(l,(!i)),get(r,(!i)))
                          then (Misc.sl_swap l r (!i) (!i);i:=((!i)-1))
                          else i:=((!i)-1);slice(sl,#2(base l),op+(#2(base l),#2(base r))))
        end
        fun mergesort sl = if (len sl) = 1 then sl else if (len sl) = 0 then (return;raise List.Empty) else
                           let
                               val m = op div((len sl),2)
                               val n = (len sl)
                               val l = slice(sl,0,(m-1))
                               val _ = print (Int.toString n)
                               val _ = print "\n"
                               val r = ArraySlice.subslice(sl,m,(NONE:int option))
                               val l = mergesort l
                               val r = mergesort r
                               val last_l=get(l,(len l)-1)
                               val hd_r = get(r,0)
                           in if (filt(hd_r,last_l)) then sl
                                  else  merge l r sl end
in (mergesort arr;Misc.arr_to_list return)end
fun split [] = ([],[])
  | split [x] = ([x],[])
  | split l = let
      fun loop ([],l,r,_)= (rev l,rev r)
        | loop (x::xs,l,r,true)= loop(xs,(x::l),r,false)
        | loop (x::xs,l,r,false) = loop(xs,l,(x::r),true)
  in loop (l,[],[],true) end

fun change _ = raise Fail "not implemented"
fun changeBest _ = raise Fail "not implemented"
(* Sudoku *)

val _ = use "sudoku-sig.sml";
val _ = use "listSudoku.sml";

end
