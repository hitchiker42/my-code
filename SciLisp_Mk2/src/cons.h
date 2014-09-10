#ifndef _SL_CONS_H
#define _SL_CONS_H
#include "common.h"
/* Nothing outside of this file/cons.h should need to manipulate
   literal cons cells, that is to say only pointers to conses
   need to be dealt with.
*/
#define XCAR(obj) (XCONS(obj)->car)
#define XCDR(obj) (XCONS(obj)->cdr)
#define XSETCAR(obj, val) (XCAR(obj) = val)
#define XSETCDR(obj, val) (XCDR(obj) = val)

//this should be used in C code in favor of Fcons for
//creating cons cells
SL_inline sl_obj make_cons(sl_obj car, sl_obj cdr){
  sl_cons *new = xmalloc(sizeof(sl_cons));
  new->car = car;
  new->cdr = cdr;
  return make_sl_obj(new, SL_cons);
}
sl_obj Fcons(sl_obj car,sl_obj cdr);
sl_obj make_list(sl_obj car,...);
sl_obj make_int_list(sl_obj car,...);
sl_obj make_list1(sl_obj car);
sl_obj make_list2(sl_obj car, sl_obj caar); 
/*
  Below here is emacs lisp code to generate the prototypes,
  defination and macros for the c[ad{2,4]r family of functions
  followed by the prototypes and macros themselves, the acutal
  functions are in cons.c
*/
/*
(defun gen-cadrs ()
  (let ((strings nil)
        (funs nil)
        (macros nil))
    (dolist (i '(?a ?d))
      (dolist (j '(?a ?d))
        (push (string i j) strings)
        (dolist (k '(?a ?d))
          (push (string i j k) strings)
          (dolist (l '(?a ?d))
            (push (string i j k l) strings)))))
    (setq strings
          (sort strings (lambda (x y) (if (= (length x) (length y))
                                          (string-lessp x y)
                                        (< (length x) (length y))))))
    (dolist (str strings)
      (let ((name (concat "c" str "r")))
        (push 
         (concat "DEFUN(" name "," name "1,1," "\"return the " name
                 " of the given cons cell\")\n(sl_obj obj){\n\treturn "
                 (mapconcat (lambda (x) (format "c%cr(" x)) str "")
                 "obj" (make-string (length str) ?\))
                 ";\n}\n") funs)
        (push
         (concat "#define X" (upcase name) "(obj) ("
                 (mapconcat (lambda (x) (format "XC%cR(" x)) (upcase str) "")
                 "obj" (make-string (1+ (length str)) ?\)) "\n")
         macros)))
         (mapc #'insert (reverse funs))
         (mapc #'insert (reverse macros))))
; this generates function prototypes
;(insert (concat "sl_obj F" name " (sl_obj obj);\n"))
*/
sl_obj Fcaar   (sl_obj obj);
sl_obj Fcadr   (sl_obj obj);
sl_obj Fcdar   (sl_obj obj);
sl_obj Fcddr   (sl_obj obj);
sl_obj Fcaaar  (sl_obj obj);
sl_obj Fcaadr  (sl_obj obj);
sl_obj Fcadar  (sl_obj obj);
sl_obj Fcaddr  (sl_obj obj);
sl_obj Fcdaar  (sl_obj obj);
sl_obj Fcdadr  (sl_obj obj);
sl_obj Fcddar  (sl_obj obj);
sl_obj Fcdddr  (sl_obj obj);
sl_obj Fcaaaar (sl_obj obj);
sl_obj Fcaaadr (sl_obj obj);
sl_obj Fcaadar (sl_obj obj);
sl_obj Fcaaddr (sl_obj obj);
sl_obj Fcadaar (sl_obj obj);
sl_obj Fcadadr (sl_obj obj);
sl_obj Fcaddar (sl_obj obj);
sl_obj Fcadddr (sl_obj obj);
sl_obj Fcdaaar (sl_obj obj);
sl_obj Fcdaadr (sl_obj obj);
sl_obj Fcdadar (sl_obj obj);
sl_obj Fcdaddr (sl_obj obj);
sl_obj Fcddaar (sl_obj obj);
sl_obj Fcddadr (sl_obj obj);
sl_obj Fcdddar (sl_obj obj);
sl_obj Fcddddr (sl_obj obj);
#define XCAAR(obj)   (XCAR(XCAR(obj)))
#define XCADR(obj)   (XCAR(XCDR(obj)))
#define XCDAR(obj)   (XCDR(XCAR(obj)))
#define XCDDR(obj)   (XCDR(XCDR(obj)))
#define XCAAAR(obj)  (XCAR(XCAR(XCAR(obj))))
#define XCAADR(obj)  (XCAR(XCAR(XCDR(obj))))
#define XCADAR(obj)  (XCAR(XCDR(XCAR(obj))))
#define XCADDR(obj)  (XCAR(XCDR(XCDR(obj))))
#define XCDAAR(obj)  (XCDR(XCAR(XCAR(obj))))
#define XCDADR(obj)  (XCDR(XCAR(XCDR(obj))))
#define XCDDAR(obj)  (XCDR(XCDR(XCAR(obj))))
#define XCDDDR(obj)  (XCDR(XCDR(XCDR(obj))))
#define XCAAAAR(obj) (XCAR(XCAR(XCAR(XCAR(obj)))))
#define XCAAADR(obj) (XCAR(XCAR(XCAR(XCDR(obj)))))
#define XCAADAR(obj) (XCAR(XCAR(XCDR(XCAR(obj)))))
#define XCAADDR(obj) (XCAR(XCAR(XCDR(XCDR(obj)))))
#define XCADAAR(obj) (XCAR(XCDR(XCAR(XCAR(obj)))))
#define XCADADR(obj) (XCAR(XCDR(XCAR(XCDR(obj)))))
#define XCADDAR(obj) (XCAR(XCDR(XCDR(XCAR(obj)))))
#define XCADDDR(obj) (XCAR(XCDR(XCDR(XCDR(obj)))))
#define XCDAAAR(obj) (XCDR(XCAR(XCAR(XCAR(obj)))))
#define XCDAADR(obj) (XCDR(XCAR(XCAR(XCDR(obj)))))
#define XCDADAR(obj) (XCDR(XCAR(XCDR(XCAR(obj)))))
#define XCDADDR(obj) (XCDR(XCAR(XCDR(XCDR(obj)))))
#define XCDDAAR(obj) (XCDR(XCDR(XCAR(XCAR(obj)))))
#define XCDDADR(obj) (XCDR(XCDR(XCAR(XCDR(obj)))))
#define XCDDDAR(obj) (XCDR(XCDR(XCDR(XCAR(obj)))))
#define XCDDDDR(obj) (XCDR(XCDR(XCDR(XCDR(obj)))))

#endif
