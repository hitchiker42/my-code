(* $Id: expression-sig.sml 182 2013-04-08 14:08:59Z cs671a $ *)

signature EXPRESSION = sig

datatype 'a expr = Cex of 'a                         (* literals *)
                 | Vex of string                     (* variables *)
                 | Bex of string * 'a expr * 'a expr (* binary operations *)

exception Parse of string        (* syntax error during parsing *)

(* parse(f) parses an expression and returns the resulting expression tree.
 * f is used to decide which strings are literals: a string s is a literal
 * iff f(s) is SOME value. *)
val parse : (string -> 'a option) -> string -> 'a expr

(* Pretty-prints an expression.  Every operator is surrounded by whitespaces
 * and parentheses unnecessary because of associativity are not included *)
val prettyPrint : ('a -> string) -> 'a expr -> string

(* True iff the string is a valid identifier name.  The (string -> 'a option)
 * function is used to characterize literals, which are not valid names. *)
val isIdentifier : (string -> 'a option) -> string -> bool
end
