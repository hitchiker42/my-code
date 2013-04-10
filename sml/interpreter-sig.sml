(* $Id: interpreter-sig.sml 182 2013-04-08 14:08:59Z cs671a $ *)

signature INTERPRETER = sig

type 'a expr (* expressions *)

type 'a env (* an environment, which associates variable names to values
             * and operator names to functions *)

exception Eval of string         (* error during evaluation *)

val eval : 'a env -> 'a expr -> 'a (* evaluation *)
end
