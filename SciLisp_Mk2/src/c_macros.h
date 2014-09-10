/* 
   This code borrows heavily (read: is copied from) the implementation
   of functions/subroutines in emacs lisp
*/
//number of args, followed by pointer to array of args
#define DEFUN_ARGS_MANY		(ptrdiff_t, sl_obj *)
//cons cell containing a list of the arguments, unevaulated
#define DEFUN_ARGS_UNEVALLED	(sl_obj)
#define DEFUN_ARGS_0	(void)
#define DEFUN_ARGS_1	(sl_obj)
#define DEFUN_ARGS_2	(sl_obj, sl_obj)
#define DEFUN_ARGS_3	(sl_obj, sl_obj, sl_obj)
#define DEFUN_ARGS_4	(sl_obj, sl_obj, sl_obj, sl_obj)
#define DEFUN_ARGS_5	(sl_obj, sl_obj, sl_obj, sl_obj, sl_obj)
#define DEFUN_ARGS_6	(sl_obj, sl_obj, sl_obj, sl_obj, sl_obj, sl_obj)
#define DEFUN_ARGS_7	(sl_obj, sl_obj, sl_obj, sl_obj, \
			 sl_obj, sl_obj, sl_obj)
#define DEFUN_ARGS_8	(sl_obj, sl_obj, sl_obj, sl_obj, \
			 sl_obj, sl_obj, sl_obj, sl_obj)
//example implementaion of make_subr
#define make_subr(cname, lname, min_args, max_args, docstr)       \
  {.f##maxargs = cname, .symbol_name = lname,                     \
   .minargs = min_args, .maxargs = max_args, .doc = docstr}
//The internals of the subr structure need to be finalized
//but they shouldn't matter for the most part

//for now doc is just a string, I might do what emacs does and make
//doc a comment and extract it later
/* Explaination:
   define a function that can be called from both C and lisp.
   'lname' is the name of the lisp function as a string(though it doesn't 
   need to be, because I can make it a string using #)
   'cname' is the basis for the name of the function in C and the
   name of the variable contining the subr object.
   the function name is formed by prepennding F to cname and the
   variable name is formed by prepending S.
   'minargs' a c integer containing the minimum number of arguments
   DEFUN_ARGS_N needs to be updated if this is more than 8.
   'maxargs' a c integer specifing the maximum number of arguments, or
   else MANY for functions taking and &rest parameter or UNEVALLED for
   macros/special forms.

   example usage:
   DEFUN("car", car, 1, 1, "Return the car of cell, if cell is nil return nil\n"
   "if cell is not a cons cell raise an error") 
   (sl_obj cell) {
   if(CONSP(cell)){
   return XCAR(cell);
   } else if (NILP(cell)) {
   return NIL;
   } else {
   raise_error();
   }
   }
*/

#define DEFUN(lname, cname, minargs, maxargs, doc)          \
  sl_obj F##cname DEFUN_ARGS_ ## maxargs;                   \
  static sl_subr S##cname =                                 \
    make_subr(F##cname, lname, minargs, maxargs, doc);      \
  sl_obj F##cname
