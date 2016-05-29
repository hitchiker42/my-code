#lang racket/base
;;Defines racket types for various c types, also defines some
;;values/functions/macros for other ffi files to use
(require "../util.rkt" (for-syntax "../util.rkt"))
(require ffi/unsafe)
(require (rename-in racket/contract (-> -->)))
;;This gets a handle to all C libraries loaded at the time it's called
;;all we want is access to the racket C api
(define base-lib (ffi-lib #f))
;;Simple functions/macros
(define-macro (define-ffi-binding lib c-name type
                (scheme-name (string-replace c-name "_" "-")))
  (let ((sym (string->symbol scheme-name)))
    `(define ,sym
       (get-ffi-obj ,c-name ,lib ,type))))
(define-macro (define-libc-binding c-name type
                (scheme-name (format #f "libc-~a" c-name)))
  (let ((sym (string->symbol scheme-name)))
    `(define ,sym (get-ffi-obj ,c-name base-lib ,type))))

(define (scheme->pointer scm)
  (cast scm _scheme _pointer))
(define (pointer->scheme ptr)
  (cast ptr _pointer _scheme))
(define (scheme-get-type scm)
  (let ((ptr (scheme->pointer scm)))
    (ptr-ref ptr _short)))
;;Types
(define-macro (define-t-types . types)
  `(begin ,@(map (lambda (x)
                   `(define ,(format-symbol "~a_t" x) ,x)) types)))
(define-t-types _int8 _uint8 _int16 _uint16 _int32 _uint32
  _int64 _uint64 _size _ssize _ptrdiff _intptr _uintptr)
;;_byte isn't quite the same as a char in C, but then again chars in C are weird
(define _char _byte)
(define _scheme-type _short)
(define _scheme-invoke-proc _pointer)
;;These add a bit more type safey
(define-cpointer-type _function-pointer)
(define-cpointer-type _scheme-pointer)
;;This assumes we're using the precise gc collector (though it really doesn't
;;matter due to alignment)
(define-cstruct _scheme-obj
  ((type _scheme-type) (keyex _short))  #:define-unsafe)
(define-cstruct _scheme-obj-inclhash ((so _scheme-obj)
                                      (keyex _short)) #:define-unsafe)
(define-cstruct _scheme-hash-table
  ((iso _scheme-obj-inclhash) (size _intptr) (count _intptr)
   (keys _pointer) (vals _pointer);;Scheme_Object **
   (make-hash-indices _pointer) (compare _pointer);;function pointers
   (mutex _scheme) (mcount _intptr)) #:define-unsafe)
(define-cstruct _scheme-hash-tree
  ((iso _scheme-obj-inclhash) (bitmap _uint)
   (count _intptr_t)
   (els _pointer)) #:define-unsafe) ;;flexable array of scheme objects
(define-cstruct _scheme-bucket
  ((so _scheme-obj) (val _pointer) (key _bytes)) #:define-unsafe)
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
   (supermodule _scheme) (submodule_ancestry _scheme)) #:define-unsafe)
(define-cstruct _scheme-port
  ((so _scheme-obj) (count-lines _byte) (was_cr _byte)
   (position _intptr) (readpos _intptr) (line-number _intptr)
   (chars-since-newline _intptr) (column _intptr) (old-column _intptr)
   (utf8-safe _int) (location-fun _scheme) (count-lines-fun _scheme)
   (buffer-mode-fun _scheme) (position-redirect _scheme)) #:define-unsafe)
;;There's a whole bunch more in both of these structs, it's just that
;;at the moment I don't need any more
(define-cstruct _scheme-output-port
  ((p _scheme-port) (closed _short) (sub-type _scheme)) #:define-unsafe)
(define-cstruct _scheme-input-port
  ((p _scheme-port) (slow _byte) (closed _byte)
   (pending-eof _byte) (sub-type _scheme)) #:define-unsafe)
;;simple objects
(define-cstruct _scheme-simple-object-str
  ((str _string/ucs-4) (tag-val _intptr)))
(define-cstruct _scheme-simple-object-bytes
  ((bytes _bytes) (tag-val _intptr)))
(define-cstruct _scheme-simple-object-cons
  ((car _scheme) (cdr _scheme)))
(define-cstruct _scheme-simple-object-cptr
  ((ptr _pointer) (type _scheme)))
(define-cstruct _scheme-simple-object-ptrs
  ((ptr1 _pointer) (ptr2 _pointer)))
(define-cstruct _scheme-simple-object-longs
  ((int1 _intptr) (int2 _intptr)))
;;putting more than 3 objects in this union makes racket segfault for some reason
(define _scheme-simple-object-union
  (_union _scheme-simple-object-ptrs
          _scheme-simple-object-longs
          _scheme-simple-object-bytes))
;;          _scheme-simple-object-str
;;          _scheme-simple-object-cons
;;          _scheme-simple-object-cptr))
 (define-cstruct _scheme-simple-object
   ((iso _scheme-obj-inclhash)
    (u _scheme-simple-object-union)))
(define _off_t _size_t)
(define _socklen_t _int)
(define _sock-data (_array _uint8 16))
(define-cstruct _sockaddr
  ((sa_family _ushort) (sa_data _sock-data)))
(define-cstruct _addrinfo
  ((ai_flags _int) (ai_family _int)
   (ai_socktype _int) (ai_protocol _int)
   (ai_addrlen _socklen_t) (ai_addr _sockaddr-pointer)
   (ai_cannonname _bytes) (ai_next _addrinfo-pointer)))
(define-cstruct _in_addr
  ((s_addr _uint32_t)))
(define-cstruct _sockaddr_in
  ((sin_port _uint16_t) (_sin_addr _in_addr)));;plus some padding
(provide (all-defined-out)
         (all-from-out ffi/unsafe)
         (all-from-out "../util.rkt"))
