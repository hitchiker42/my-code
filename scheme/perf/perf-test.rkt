#lang racket/base
(require racket/function)
(require "heap.rkt")
(define vec-100000 (with-input-from-file "../vec-100000" (thunk (read))))
(define vec-1000000 (with-input-from-file "../vec-1000000" (thunk (read))))
(define vec-10000000 (with-input-from-file "../vec-10000000" (thunk (read))))

