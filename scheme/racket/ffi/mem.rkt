#lang racket/base
(require "../util.rkt")
(require ffi/unsafe
         ffi/unsafe/define)
(define (gc-malloc sz) (malloc 'atomic sz))
;;This gets a handle to all C libraries loaded at the time it's called
;;all we want is access to the racket C api
(define base-lib (ffi-lib #f))
(define (scheme->pointer scm)
  (cast scm _scheme _pointer))
(define (pointer->scheme ptr)
  (cast ptr _pointer _scheme))
(define (scheme-get-type scm)
  (let ((ptr (scheme->pointer scm)))
    (ptr-ref ptr _short)))
(define-macro (define-ffi-binding lib c-name type)
  (let ((sym (string->symbol (string-replace c-name "_" "-"))))
    `(define ,sym
       (get-ffi-obj ,c-name ,lib ,type))))
(define-ffi-binding
  base-lib "scheme_get_type_name_or_null" (_fun _short -> _bytes))
(define-ffi-binding
  base-lib "scheme_get_env" (_fun _pointer -> _pointer))
;;There are a few special environments you can't get the normal way
;;(i.e module->namespace) but they have accessor functinos in the C api
(define-ffi-binding base-lib "scheme_get_foreign_env"
  (_fun -> _scheme))
(define-ffi-binding base-lib "scheme_get_unsafe_env"
  (_fun -> _scheme))
(define (scheme-get-type-name scm)
  (if (fixnum? scm)
      #"<fixnum-integer>"
      (let ((type (scheme-get-type scm)))
        (scheme-get-type-name-or-null type))))
#|
# define SCHEME_TYPE(obj)     (SCHEME_INTP(obj)?(Scheme_Type)scheme_integer_type:((Scheme_Object *)(obj))->type)
# define _SCHEME_TYPE(obj) ((obj)->type) /* unsafe version */
SHARED_OK static char **type_names;
SHARED_OK static Scheme_Type maxtype, allocmax;
char *scheme_get_type_name_or_null(Scheme_Type t)
{
  if (t < 0 || t >= maxtype)
    return "<bad-value>";
    return type_names[t]; |#
(define _scheme-type _short)
(define _scheme-invoke-proc _pointer)
;;This assumes we're using the precise gc collector
(define-cstruct _scheme-obj ((type _scheme-type)))
(define-cstruct _scheme-obj-inclhash ((so _scheme-obj)
                                      (keyex _short)))
(define-cstruct _scheme-hash-table
  ((iso _scheme-obj-inclhash) (size _intptr) (count _intptr)
   (keys _pointer) (vals _pointer);;Scheme_Object **
   (make-hash-indices _pointer) (compare _pointer);;function pointers
   (mutex _scheme) (mcount _intptr)))
(define-cstruct _scheme-bucket
  ((so _scheme-obj) (val _pointer) (key _bytes)))
(define-cstruct _scheme-module
  ((so _scheme-obj) (predefined _short)
   (phaseless _scheme) (code_key _scheme)
   (modname _scheme) (modsrc _scheme)
   (et_requires _scheme) (requires _scheme)
   (tt_requires _scheme) (dt_requires _scheme)
   (other_requires _pointer) ;;pointer to hash table
   (prim_body _scheme-invoke-proc) (prim_et_body _scheme-invoke-proc)
   (bodies _pointer) (me _pointer) (num_phases _int)
   (exp_infos _pointer) (self_module _scheme)
   (binding-names _scheme) (et_binding_names _scheme)
   (other_binding_names _scheme) (insp _scheme)
   (lang_info _scheme) (hints _scheme)
   (ii_src _scheme) (comp_prefix _pointer) (super_bxs_info _pointer)
   (sub_iidx_ptrs _pointer) (max_let_depth _int)
   (prefix _pointer) (dummy _scheme) (primitive _pointer)
   (rn_stx _scheme) (submodule_path _scheme) (pre_submodules _scheme)
   (post_submodules _scheme) (pre_submodule_names _scheme)
   (supermodule _scheme) (submodule_ancestry _scheme)))
