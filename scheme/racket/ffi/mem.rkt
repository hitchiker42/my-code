#lang racket/base
(require ffi/unsafe)
(require (rename-in racket/contract (-> -->)))
(require "../util.rkt")
(require (for-syntax "../util.rkt")
         (for-meta 2 "../util.rkt"))
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
(define-macro (define-ffi-binding lib c-name type
                (scheme-name (string-replace c-name "_" "-")))
  (let ((sym (string->symbol scheme-name)))
    `(define ,sym
       (get-ffi-obj ,c-name ,lib ,type))))
(define-macro (define-libc-binding c-name type)
  (let ((sym (format-symbol "libc-~a" c-name)))
    `(define ,sym (get-ffi-obj ,c-name base-lib ,type))))
;;if the given type is outside the range of types this returns
;;#<bad-value>, it only returns null for types that are in the range
;;of valid types, and don't exist
(define-ffi-binding base-lib
  "scheme_get_type_name_or_null"(_fun _short -> _bytes))
;;(define-ffi-binding base-lib
;;  "scheme_get_env" (_fun _pointer -> _pointer))
;;There are a few special environments you can't get the normal way
;;(i.e module->namespace) but they have accessor functinos in the C api
;;(define-ffi-binding base-lib "scheme_get_foreign_env"
;;  (_fun -> _scheme))
;;(define-ffi-binding base-lib "scheme_get_unsafe_env"
;;  (_fun -> _scheme))
(define (scheme-get-type-name scm)
  (if (fixnum? scm)
      #"<fixnum-integer>"
      (let ((type (scheme-get-type scm)))
        (scheme-get-type-name-or-null type))))
(define-macro (define-t-types . types)
  `(begin ,@(map (lambda (x)
                   `(define ,(format-symbol "~a_t" x) ,x)) types)))
(define-t-types _int8 _uint8 _int16 _uint16 _int32 _uint32
  _int64 _uint64 _size _ssize _ptrdiff)

(define _scheme-type _short)
(define _scheme-invoke-proc _pointer)
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
(define _scheme-simple-object-union
  (_union _scheme-simple-object-bytes
             _scheme-simple-object-str
             _scheme-simple-object-cons))
(define-cstruct _scheme-simple-object
  ((iso _scheme-obj-inclhash)
   (u _scheme-simple-object-union)))
;;this sortof already exists, make-sized-byte-string is the same as
;;make_sized_byte_string with copy set to 0
(define-ffi-binding base-lib
  "scheme_make_sized_byte_string"
  (_fun _pointer _intptr _int -> _scheme)
  "scheme-make-byte-string")
(define pointer->bytevector
  (case-lambda
    ((ptr len)
     (scheme-make-byte-string ptr len 0))
    ((ptr offset len)
     (scheme-make-byte-string (ptr-add ptr offset) (- len offset) 0))))
(define (bytevector->pointer bv (start 0))
  (let* ((obj (cast bv _scheme _scheme-simple-object-pointer))
         (byte-string (union-ref (scheme-simple-object-u obj) 0))
         (ptr (scheme-simple-object-bytes-bytes byte-string)))
    (ptr-add ptr start)))
(define (allocate-bytevector sz)
  (let ((ptr (malloc sz 'atomic-interior)))
    (pointer->bytevector ptr sz)))
(define (list->bytevector list)
  (let* ((sz (length list))
         (bv (allocate-bytevector sz))
         (i 0))
    (for-each (lambda (x) (bytes-set! bv i x) (incf i)) list)
    bv))
(define (copy-bytevector bv) ;;vs bytevector-copy
  (let* ((sz (bytevector-length bv))
         (new (malloc sz 'atomic-interior))
         (old (bytevector->pointer bv)))
    (memcpy new old sz)
    (pointer->bytevector new sz)))

;;Be careful with using this with bytevectors allocated from racket
;;since they don't trace interior pointers, this only matters if
;;start is non-zero and the slice will outlive the original
(define (bytevector-slice bv start (end (bytevector-length bv)))
  (if (or (> end (bytevector-length bv))
            (> start end))
      (raise-arguments-error 'bytevector-slice
                             "start <= end <= bytevector-length"
                             "start" start "end" end)
      (let ((ptr (bytevector->pointer bv)))
        (pointer->bytevector ptr start end))))
(define (bytevector-extend bv sz (mode 'atomic))
  (let* ((old-len (bytevector-length bv))
         (new-len (+ old-len sz))
         (old (bytevector->pointer bv))
         (new (malloc mode new-len)))
    (memcpy new old old-len)
    (memset new old-len 0 sz)
    new))
(struct svector
  (length size ptr)
  #:mutable #:transparent)
(define-syntax-rule (destructure-svector svec)
  (values (svector-ptr svec) (svector-length svec) (svector-size svec)))
(define/contract (svector-ref svec idx)
  (--> svector? (and/c fixnum? positive?) any)
  (if (>= idx (svector-length svec))
      (raise-range-error 'svector-ref "svector" ""
                         idx svec 0 (svector-length svec))
      (ptr-ref (svector-ptr _scheme idx))))
(define (svector-check-length svec (nelts 1))
  (let-values (((ptr length size) (destructure-svector svec)))
    (when (>= (+ length nelts) size)
      (let ((new (malloc (* 2 size) 'nonatomic)))
        (memcpy new ptr size _scheme)
        (set-svector-ptr! svec new)
        (set-svector-size! svec (* 2 size))
        (svector-check-length svec nelts)))))
(define/contract (svector-push! elt svec)
  (--> any/c svector? any)
  (svector-check-length svec)
  (let-values (((ptr length size) (destructure-svector svec)))
    (ptr-set! ptr _scheme length elt)
    (set-svector-length! svec (1+ length))))
(define/contract (svector-push-multipush! svec seq)
  (--> any/c svector? any)
  (svector-check-length svec (sequence-length seq)))
  ;;do a memcpy)
(provide
 (except-out (all-defined-out) scheme-get-type-name-or-null)
 (all-from-out ffi/unsafe))
