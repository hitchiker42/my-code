(* $Id: sudoku-sig.sml 176 2013-03-26 15:24:13Z cs671a $ *)

signature SUDOKU = sig

eqtype grid

exception Parse
exception Unsolvable

val parseString: string -> grid
val parseFile: string -> grid
val get: grid * int -> int
val set: grid * int * int -> grid (* may modify the grid, or not *)
val unsolved: grid -> int option
val possibles: grid * int -> int list
val valid: grid -> bool
val print: grid -> unit
val sudoku: grid -> grid
end
