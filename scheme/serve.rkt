#lang racket
(define-syntax-rule (defun name args . body)
  (define name #<undefined>)
  `(set! ',name (lambda ,args ,@body)))
(defun fac (n) (if (< n 0) 1 (* n (fac (- n 1)))))
