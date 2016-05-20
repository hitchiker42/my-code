#lang racket/base
(require "util.rkt")
(require ffi/unsafe)
(define (gc-malloc sz) (malloc 'atomic sz))
