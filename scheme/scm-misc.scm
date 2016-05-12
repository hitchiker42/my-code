(define-module (misc)
  #:export '()
  #:use-module '())

(define-syntax-rule (make-predicate type)
  (let ((type-name (symbol->string type)))
    (string->symbol (string-append/shared type-name "?"))))
(make-predicate number)
(define-syntax define-typecase
  (lambda (x)
    (syntax-case x
        (_ name default)
        #'(define-syntax name
            (lambda (y)
              (syntax-case y
                  (_ expr case case* ...)
                (let ((type expr))
                  #`(cond
                     
