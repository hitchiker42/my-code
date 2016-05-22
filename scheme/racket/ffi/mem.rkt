#lang racket/base
(require "../util.rkt")
(require ffi/unsafe)
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
(define scheme-get-type-name
  (get-ffi-obj "scheme_get_type_name_or_null" base-lib
               (_fun _scheme -> _bytes)))
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
    
