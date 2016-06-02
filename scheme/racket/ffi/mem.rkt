#lang racket/base
(require ffi/unsafe)
(require (rename-in racket/contract (-> -->)))
(require (only-in '#%foreign ffi-callback))
(require "../util.rkt")
(require "types.rkt")
(require (for-syntax "../util.rkt")
         (for-meta 2 "../util.rkt")
         (for-syntax ffi/unsafe))
;;if the given type is outside the range of types this returns
;;#<bad-value>, it only returns null for types that are in the range
;;of valid types, and don't exist
(define-ffi-binding base-lib
  "scheme_get_type_name_or_null"(_fun _short -> _bytes))
;;(define-ffi-binding base-lib
;;  "scheme_get_env" (_fun _pointer -> _pointer))
;;There are a few special environments you can't get the normal way
;;(i.e module->namespace) but they have accessor functinos in the C api
(define-ffi-binding base-lib "scheme_get_foreign_env"
 (_fun -> _scheme))
(define-ffi-binding base-lib "scheme_get_unsafe_env"
  (_fun -> _scheme))
(define-ffi-binding base-lib "scheme_get_futures_env"
  (_fun -> _scheme))
(define (scheme-get-type-name scm)
  (if (fixnum? scm)
      #"<fixnum-integer>"
      (let ((type (scheme-get-type scm)))
        (scheme-get-type-name-or-null type))))
;;this sortof already exists, make-sized-byte-string is the same as
;;make_sized_byte_string with copy set to 0
(define-ffi-binding base-lib
  "scheme_make_sized_byte_string"
  (_fun _pointer _intptr _int -> _scheme)
  "scheme-make-byte-string")
(define-ffi-binding base-lib
  "scheme_make_sized_char_string"
  (_fun _pointer _intptr _int -> _scheme)
  "scheme-make-char-string")
(define-macro (make-pointer-cast type fxn (elt-type _uint8))
  `(define
     (,(format-symbol "pointer->~a" type) ptr len (offset #f) #:copy (copy 0))
     (if (not offset)
         (,fxn ptr len copy)
         (let ((len offset) (offset len))
           (,fxn (ptr-add ptr offset ,elt-type) (- len offset) copy)))))
(make-pointer-cast bytevector scheme-make-byte-string)
(make-pointer-cast string scheme-make-char-string _mzchar)
(define (simple-object->pointer rkt-obj (start 0) (type _uint8))
  (let* ((obj (cast rkt-obj _scheme _scheme-simple-object-ptrs-pointer))
         (ptr (scheme-simple-object-ptrs-ptr1 obj)))
    (ptr-add ptr start type)))
(define-alias bytevector->pointer simple-object->pointer)
(define (string->pointer str (start 0))
  (simple-object->pointer str start _mzchar))

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
;;There is a reason racket doesn't natively provide functions to get
;;pointers into strings/bytevectors/vectors, the garbage collector by
;;default doesn't trace interior pointers. So be careful using these
(define-macro (gen-slice-fxn type)
  (let ((type-slice (format-symbol "~a-slice" type))
        (type-length (format-symbol "~a-length" type))
        (type->pointer (format-symbol "~a->pointer" type))
        (pointer->type (format-symbol "pointer->~a" type)))
    `(define (,type-slice x start (end (,type-length x)))
       (if (or (> end (,type-length x))
               (> start end))
               (raise-arguments-error ',type-slice
                                      ,(format #f "start <= end <= ~a"
                                               (symbol->string type-length))
                                      "start" start "end" end)
               (let ((ptr (,type->pointer x)))
                 (,pointer->type ptr start end))))))
(gen-slice-fxn bytevector)
(gen-slice-fxn string)
;; (define (bytevector-slice bv start (end (bytevector-length bv)))
;;   (if (or (> end (bytevector-length bv))
;;             (> start end))
;;       (raise-arguments-error 'bytevector-slice
;;                              "start <= end <= bytevector-length"
;;                              "start" start "end" end)
;;       (let ((ptr (bytevector->pointer bv)))
;;         (pointer->bytevector ptr start end))))
;; (define (string-slice str start (end (string-length str)))
;;   (if (or (> end (string-length str))
;;           (> start end))
;;       (raise-arguments-error 'string-slice
;;                              "start <= end <= string-length"
;;                              "start" start "end" end)
;;       (let ((ptr (string->pointer str)))
;;         (pointer->string ptr start end))))
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

;;process threads (aka real threads)
(define _mzrt_thread_id _uintptr);;aka pthread_t
(define-cstruct _mz_proc_thread
  ((thread-id _mzrt_thread_id) (refcount _int)))
(define _mz_proc_thread_start (_fun _pointer -> _pointer))
(define-ffi-binding base-lib
  "mz_proc_thread_create"
  (_fun _pointer _pointer -> _mz_proc_thread-pointer))
(define-ffi-binding base-lib
  "mz_proc_thread_wait" (_fun _mz_proc_thread-pointer -> _pointer))
(define-ffi-binding base-lib
  "mz_proc_thread_detach" (_fun _mz_proc_thread-pointer -> _int))
(define-ffi-binding base-lib
  "mz_proc_thread_exit" (_fun _pointer -> _void))
;;semaphores
(define-ffi-binding base-lib
  "mzrt_sema_create" (_fun _pointer _int -> _int))
(define-ffi-binding base-lib
  "mzrt_sema_post" (_fun _pointer -> _int))
(define-ffi-binding base-lib
  "mzrt_sema_wait" (_fun _pointer -> _int))
(define-ffi-binding base-lib
  "mzrt_sema_destroy" (_fun _pointer -> _int))
;(load-extension "racket_util.so")
;; (define hello (lambda x (println "Hello, World!\n")))
;; (define test-callback (ffi-callback hello (list _pointer) _pointer))
;; (define thread (mz-proc-thread-create test-callback #f))

(provide
 (except-out (all-defined-out) scheme-get-type-name-or-null))
