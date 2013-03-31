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
    let
        val len = ArraySlice.length
        val arr=Array.fromList l
        fun mergesort sl = if 
in end
fun split [] = []
  | split [x] = [x]
  | split l = let
      fun loop ([],l,r,_)= (rev l,rev r)
        | loop (x::xs,l,r,true)= loop(xs,(x::l),r,false)
        | loop (x::xs,l,r,false) = loop(xs,l,(x::r),true)
  in loop (l,[],[],true) end


(* Sudoku *)

val _ = use "sudoku-sig.sml";
val _ = use "listSudoku.sml";

end
