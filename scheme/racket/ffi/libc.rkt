#lang racket/base
(require "types.rkt")
(require ffi/unsafe/cvector)
(define-libc-binding "read" (_fun _int _pointer _size_t -> _ssize_t))
(define-libc-binding "pread" (_fun _int _pointer _size_t _off_t -> _ssize_t))
(define-libc-binding "write" (_fun _int _pointer _size_t -> _ssize_t))
(define-libc-binding "pwrite" (_fun _int _pointer _size_t -> _ssize_t))
(define-libc-binding "getaddrinfo"
  (_fun _bytes _bytes _addrinfo-pointer _pointer -> _int))
(define-libc-binding "strtol"
  (_fun _bytes (_ptr o _pointer) (_int = 0) -> _long))
(define-libc-binding "getcwd"
  (_fun (_bytes = (make-bytes 256)) (_size_t = 256) -> _string))

;;threads
(define-libc-binding "pthread_create"
  (_fun _pointer _pointer (_fun _pointer -> _pointer) -> _pointer))
(define-libc-binding "qsort"
  (_fun _pointer _size_t _size_t (_fun _pointer _pointer -> _int) -> _void))
(provide (all-defined-out))

(define test (list->cvector (iota 10.0 0.0 -1.0) _scheme))
(define c-cmp (lambda (x y) (cmp (ptr-ref x _scheme)
                                 (ptr-ref y _scheme))))
(define cmp (lambda (x y) (if (= x y) 0 (if (< x y) -1 1))))
(libc-qsort (cvector-ptr test) 10 8 c-cmp)
