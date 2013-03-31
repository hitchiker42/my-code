(* Assignment #6: SML 2 *)

(* Sorting functions *)

fun mergeSort _ = raise Fail "not implemented"
fun quickSort _ = raise Fail "not implemented"

(* change *)

exception CannotChange

fun change _ = raise Fail "not implemented"
fun changeBest _ = raise Fail "not implemented"

(* Sudoku *)

val _ = use "sudoku-sig.sml";
val _ = use "listSudoku.sml";
