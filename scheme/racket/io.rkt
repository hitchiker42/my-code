#lang racket/base
(require "util.rkt")
(struct input/output-port (in out)
        #:guard (lambda (in out name)
                  ;;one of in and out needs to be non-null
                  (unless (or (or (input-port? in) (not in))
                              (or (output-port? out) (not out)))
                    (error (format #f "invalid arguments to ~a" name0))))
        #:constructor-name make-input/output-port)
