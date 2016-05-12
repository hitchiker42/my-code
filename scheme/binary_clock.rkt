#lang racket/gui
(struct binary-clock (size clock-time last-updated))
(define (make-binary-clock [size 63]
                           [clock-time (current-seconds)]
                           [last-updated (current-seconds)])
  (binary-clock size clock-time last-updated))
(module* main #f)
