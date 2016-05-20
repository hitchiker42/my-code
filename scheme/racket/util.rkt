#lang racket/base
;;Use only the basic language and explicitly import other modules, re-export
;;any modules that I would generally import. This will let me use racket base
;;as the language and then import util to get all the modules I generally need
;;without importing all of racket

;;Formatting note, lines should be 102 characters max (preferably < 80)
(require racket/bytes ;;byte vectors
         rnrs/bytevectors-6 ;;r6rs bytevectors, for compatibility
         racket/function ;;higher order function helpers (idenitiy, thunk, etc)
         racket/future ;;futures are the only (real) concurrency mechanism
         racket/string ;;String functions, including string-join
         racket/math ;;pi, hyperbolic trig, exact-{round, floor, ceiling}
         racket/sequence ;;Generic sequence functions
         racket/vector ;;vector map/filter
         racket/syntax ;;format-id
         racket/port) ;;with-{input,output}-to-{file,string,...}

;;To use a function at macroexpansion time we need to tell racket that we the
;;binding to be available at expansion time, not just run time
(require (for-syntax racket/base))
(require (for-meta 2 racket/base))
(require (for-syntax racket/string))
(require (for-syntax racket/syntax))

(define-syntax-rule (identifier-syntax id) (make-rename-transformer #'id))
(define-syntax-rule (define-alias alias symbol)
  (define-syntax alias (make-rename-transformer #'symbol)))

(define-alias prog1 begin0)
(define-alias progn begin)
(define-syntax my-if
  (syntax-rules ()
    ((_ cond then) (when cond then))
    ((_ cond then else) (if cond then else))
    ((_ cond then else rest ...) (if cond then (begin else rest ...)))))

;;TODO: Figure out how continuation prompts work and use them to
;;allow breaking out of a dolist/dotimes
(define-syntax-rule (dolist (var list) exp exp* ...)
  (for-each (lambda (x) (let ((var x)) exp exp* ...)) list))
;;written in a way that should make it easy to optimze
(define-syntax-rule (dotimes (var count) exp exp* ...)
  (let acc ((var 0))
    (when (< var count)
      exp exp* ...
      (acc (1+ var)))))
(define-syntax-rule (until test body ...)
  (while (not test) body ...))

(define-syntax-rule (pop! ls)
  (prog1 (cl-car ls) (set! ls (cl-cdr ls))))
(define-syntax-rule (push! elt place)
  (set! place (cons elt place)))
 (define (car-safe obj)
   ;;"My-Ifobj is a pair return (car obj) otherwise return '()"
   (my-if(pair? obj) (car obj) '()))
 (define (cl-car ls)
;;   ;;"my-ifls is null? return null, otherwise return (car ls)"
   (my-if(null? ls) ls (car ls)))
 (define (cdr-safe obj)
;;   ;;"My-Ifobj is a pair return (cdr obj) otherwise return '()"
   (my-if(pair? obj) (cdr obj) '()))
 (define (cl-cdr ls)
;;   ;;"my-ifls is null? return null, otherwise return (cdr ls)"
   (my-if(null? ls) ls (cdr ls)))
(define-alias 1- sub1)
(define-alias 1+ add1)
(define-syntax-rule (incf place)
  (set! place (1+ place)))
(define-syntax-rule (decf place)
  (set! place (1- place)))
;; A lot of these are taken from the guile source
(define-syntax while
  (lambda (x)
    (syntax-case x ()
      ((while cond body ...)
       #`(let ((break-tag (make-continuation-prompt-tag 'break))
               (continue-tag (make-continuation-prompt-tag 'continue)))
           (call-with-continuation-prompt
            (lambda ()
              ;;Figure out how to fix this so I can break without a value
              (define-syntax #,(datum->syntax #'while 'break)
                (lambda (x)
                  (syntax-case x ()
                    ((_ arg (... ...))
                     #'(abort-current-continuation break-tag arg (... ...)))
                    (_
                     #'(lambda args
                         (apply abort-current-to-prompt break-tag args))))))
              (let lp ()
                (call-with-continuation-prompt
                 (lambda () 
                   (define-syntax #,(datum->syntax #'while 'continue)
                     (lambda (x)
                       (syntax-case x ()
                         ((_)
                          #'(abort-to-prompt continue-tag))
                         ((_ . args)
                          (syntax-violation 'continue "too many arguments" x))
                         (_
                          #'(lambda ()
                              (abort-to-prompt continue-tag))))))
                   (do () ((not cond) #f) body ...))
                 continue-tag
                 (lambda (k) (lp)))))
            break-tag
            (lambda (k . args)
              (my-if(null? args)
                  #t  
                (apply values args)))))))))
(define-syntax define-macro
  (lambda (x)
;;    "Define a defmacro."
    (syntax-case x ()
      ((_ (macro . args) doc body1 body ...)
       (string? (syntax->datum #'doc))
       #'(define-macro macro doc (lambda args body1 body ...)))
      ((_ (macro . args) body ...)
       #'(define-macro macro #f (lambda args body ...)))
      ((_ macro transformer)
       #'(define-macro macro #f transformer))
      ((_ macro doc transformer)
       (or (string? (syntax->datum #'doc))
           (not (syntax->datum #'doc)))
       #'(define-syntax macro
           (lambda (y)
             doc
             #((macro-type . defmacro)
               (defmacro-args args))
             (syntax-case y ()
               ((_ . args)
                (let ((v (syntax->datum #'args)))
                  (datum->syntax y (apply transformer v)))))))))))
;;There's not really a way around this my-ifI want macro defining macros 
(begin-for-syntax
 (define-syntax define-macro
  (lambda (x)
;;    "Define a defmacro."
    (syntax-case x ()
      ((_ (macro . args) doc body1 body ...)
       (string? (syntax->datum #'doc))
       #'(define-macro macro doc (lambda args body1 body ...)))
      ((_ (macro . args) body ...)
       #'(define-macro macro #f (lambda args body ...)))
      ((_ macro transformer)
       #'(define-macro macro #f transformer))
      ((_ macro doc transformer)
       (or (string? (syntax->datum #'doc))
           (not (syntax->datum #'doc)))
       #'(define-syntax macro
           (lambda (y)
             doc
             #((macro-type . defmacro)
               (defmacro-args args))
             (syntax-case y ()
               ((_ . args)
                (let ((v (syntax->datum #'args)))
                  (datum->syntax y (apply transformer v))))))))))))
;; (define-syntax-rule (define-macro* body ...)
;;   (begin (define-macro body ...)
;;          (begin-for-syntax (define-macro body ...))))
(define-syntax defmacro
  (lambda (x)
;;    "Define a defmacro, with the old lispy defun syntax."
    (syntax-case x ()
      ((_ macro args doc body1 body ...)
       (string? (syntax->datum #'doc))
       #'(define-macro macro doc (lambda args body1 body ...)))
      ((_ macro args body ...)
       #'(define-macro macro #f (lambda args body ...))))))

(define-macro (concat . args) `(string-join ,args ""))
(define-macro (concat-lit . args) (string-join args ""))
(define-macro (build-symbol . args)
  `(string->symbol
    (string-join
    (map (lambda (x) (my-if (symbol? x) (symbol->string x) x))
         (list ,@args)) "")))
(define (module-ref mod var)
  (namespace-variable-value var (module->namespace mod)))
(define-for-syntax (module-ref mod var)
  (namespace-variable-value var (module->namespace mod)))
;; (begin-for-syntax
;;  (define-macro (module-ref mod var)
;;   `(namespace-variable-value ',var (module->namespace ',mod))))
;; (begin-for-syntax
;;  (define-macro (module-ref mod var)
;;   `(namespace-variable-value ',var (module->namespace ',mod))))
;;I took this from the racket reference, it just appends a ? to an identifier
;;which, given a type, generally gives a type predicate
(define-syntax (make-pred stx)
  (syntax-case stx ()
    ((_ name)
     (format-id #'name "~a?" (syntax-e #'name)))))
(define-macro (format out fmt . rest)
  (cond
   ((eq? out #t) `(fprintf (current-output-port) ,fmt ,@rest))
   ((eq? out #f) `(,(module-ref 'racket/base 'format) ,fmt ,@rest))
   (else `(fprintf ,out ,fmt ,@rest))))

(require racket/pretty)
(define-alias pprint pretty-print)
(define (macroexpand body) (syntax->datum (expand body)))
(provide (all-defined-out)
         (all-from-out
          racket/bytes rnrs/bytevectors-6 racket/function racket/future racket/port
          racket/string racket/math racket/sequence racket/vector racket/syntax)
         (for-syntax (all-from-out racket/base racket/string racket/syntax))
         (for-meta 2 (all-from-out racket/base)))
