#lang racket/base
(require "util.rkt" "ffi/ffi.rkt" "io.rkt")
(require racket/place)
(define box (malloc-immobile-cell 0))
(format #t "box = ~a\n" (ptr-ref box _scheme))
(define test-place (dynamic-place "test-place.rkt" 'start-proc))
(place-channel-put test-place box)
(place-wait test-place)
(format #t "box = ~a\n" (ptr-ref box _scheme))

