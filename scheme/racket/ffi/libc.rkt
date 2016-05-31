#lang racket/base
(require "types.rkt")
(require ffi/unsafe/cvector)
;;low level io
(define-libc-binding "read" (_fun _int _pointer _size_t -> _ssize_t))
(define-libc-binding "pread" (_fun _int _pointer _size_t _off_t -> _ssize_t))
(define-libc-binding "write" (_fun _int _pointer _size_t -> _ssize_t))
(define-libc-binding "pwrite" (_fun _int _pointer _size_t -> _ssize_t))
(define-libc-binding "close" (_fun _int -> _int))
;;FILE io
(define-libc-binding "fputs" (_fun _string _pointer -> _int))
(define-libc-binding "getaddrinfo"
  (_fun _bytes _bytes _addrinfo-pointer _pointer -> _int))
(define-libc-binding "strtol"
  (_fun _bytes (_ptr o _pointer) (_int = 0) -> _long))
(define-libc-binding "getcwd"
  (_fun (_bytes = (make-bytes 256)) (_size_t = 256) -> _string))

;;Obviously using the libc qsort is silly, but it's a good way to test/demonstrate
;;how to call a c function with a callback
(define-libc-binding "qsort"
  (_fun (vec : _cvector) (_size_t = (cvector-length vec))
        (_size_t = (ctype-sizeof (cvector-type vec)))
        (_fun _pointer _pointer -> _int) -> _void))
(define test (list->cvector (iota 10.0 0.0 -1.0) _scheme))
(define c-cmp (lambda (x y) (cmp (ptr-ref x _scheme)
                                 (ptr-ref y _scheme))))
(define cmp (lambda (x y) (if (= x y) 0 (if (< x y) -1 1))))
;;Racket doesn't provide much support for temporary files, and I'd rather use
;;the ones from C
(define-libc-binding "mkstemp" (_fun _bytes -> _int))
;;threads
(define pthread-attr-size 64);;actually only 56 bytes, but rounded up
(define posix-sem-size 32)
(define pthread-size 8)

(define-libc-binding "pthread_create"
  (_fun _pointer _pointer (_fun _pointer -> _pointer) -> _pointer))
(define-libc-binding "pthread_attr_init" (_fun _pointer -> _int))
(define-libc-binding "pthread_attr_destroy" (_fun _pointer -> _int))
(define-libc-binding "pthread_attr_setdetachstate" (_fun _pointer _int -> _int))

;; (define (get-pthread-type-sizes . types)
;;   (require racket/system)
;;   (let ((print-stmts (map (lambda (type) (format #f "print_sizeof(~a);\n" type))))
;;         (header
;;          #<<         EOF
;;          #include <stdio.h>
;;          #include <pthread.h>
;;          #include <semaphore.h>
;;          #define print_sizeof(type)\
;;          printf("%s %d", #type, sizeof(type));
;;          EOF
;;          )

(provide (all-defined-out))
