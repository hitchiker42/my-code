#lang racket/base
(require "util.rkt" "ffi/ffi.rkt" "io.rkt")
(require racket/place)
(define (start-proc channel)
  (let ((ptr (place-channel-get channel)))
    (ptr-set! ptr _scheme (1+ (ptr-ref ptr _scheme)))))
(provide start-proc)
