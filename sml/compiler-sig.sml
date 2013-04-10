(* $Id: compiler-sig.sml 182 2013-04-08 14:08:59Z cs671a $ *)

signature COMPILER = sig

datatype 'a instr = Load of string  (* loads a variable from memory *)
                  | Apply of string (* loads an op. from mem. and applies it *)
                  | Const of 'a     (* pushes a literal value on the stack *)

type 'a env (* an environment, which associates variable names to values
             * and operator names to functions *)

type 'a expr (* expression to be compiled into programs *)

exception Runtime of string (* runtime error during runs *)

(* Compiles an expression into a list of instructions *)
val compile : 'a expr -> 'a instr list

(* Runs a list of instructions in a given env and returns the resulting value *)
val run : 'a env -> 'a instr list -> 'a
end
