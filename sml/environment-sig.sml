(* $Id: environment-sig.sml 182 2013-04-08 14:08:59Z cs671a $ *)

signature ENVIRONMENT = sig (* immutable environments *)

type 'a env (* an environment, which associates variable names to values
             * and operator names to functions *)

exception SymNotFound of string (* raised when a name is not found *)

(* Empty environment (nothing defined *)
val emptyEnv : 'a env

(* addOp((name,op),e) adds operation 'op' in env 'e' under the name 'name' *)
val addOp : (string * ('a * 'a -> 'a)) * 'a env -> 'a env

(* addVar ((name,x),e) add value 'x' in env 'e' under the name 'name' *)
val addVar : (string * 'a) * 'a env -> 'a env

(* getOp e name returns the op named 'name' in env 'e' or raise SymNotFound *)
val getOp : 'a env -> string -> 'a * 'a -> 'a

(* getVar e name returns the var named 'name' in env 'e' or raise SymNotFound *)
val getVar : 'a env -> string -> 'a

(* removes an operator *)
val remOp : (string * 'a env) -> 'a env

(* removes a variable *)
val remVar : (string * 'a env) -> 'a env
end

signature SUPER_ENVIRONMENT = sig (* env with infinite collections *)

include ENVIRONMENT

(* addOps(f,e) adds all the operators defined by function 'f' to env 'e'.
 * 'f' defines an operator under the name 'name' iff f(name) is SOME operator *)
val addOps : (string -> ('a * 'a -> 'a) option) * 'a env -> 'a env

(* addVars(f,e) adds all the values defined by function 'f' to env 'e'
 * 'f' defines a value under the name 'name' iff f(name) is SOME value *)
val addVars : (string -> 'a option) * 'a env -> 'a env
end
