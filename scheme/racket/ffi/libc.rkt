#lang racket/base
(require "types.rkt")
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
(provide (all-defined-out))
