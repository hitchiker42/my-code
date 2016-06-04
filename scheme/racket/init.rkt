#lang racket/base
;;Define basic imports and syntax rules, none of these should rely on each other,
;;this allows me to import this into util.rkt with for-syntax and avoid defining
;;things like define-macro twice.


;;Use only the basic language and explicitly import other modules, re-export
;;any modules that I would generally import. This will let me use racket base
;;as the language and then import util to get all the modules I generally need
;;without importing all of racket

;;Formatting note, lines should be 102 characters max (preferably < 80)
(require racket/bytes ;;byte vectors
         rnrs/bytevectors-6 ;;r6rs bytevectors, for compatibility
         racket/function ;;higher order function helpers (idenitiy, thunk, etc)         
         racket/future ;;futures, simple (mostly useless) concurrency
         racket/place ;;limited wrapper around os threads
         racket/string ;;String functions, including string-join
         racket/math ;;pi, hyperbolic trig, exact-{round, floor, ceiling}
         racket/sequence ;;Generic sequence functions
         racket/vector ;;vector map/filter
         racket/syntax ;;format-id
;;         racket/port ;;with-{input,output}-to-{file,string,...}
         rnrs/io/ports-6 ;;racket doesn't have input/output ports, somehow
         racket/match ;;ml style pattern matching
         racket/unsafe/ops
         racket/pretty
         (except-in racket/list last);;I define my own version that works with null
         syntax/location
         srfi/48 ;;format (the (format port fmt args ...)  version)
         srfi/71 ;;unifies let and let-values
         )

;;To use a function at macroexpansion time we need to tell racket that we the
;;binding to be available at expansion time, not just run time
(require (for-syntax racket/base))
(require (for-meta 2 racket/base))
(require (for-syntax racket/string))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/location))

(define-syntax-rule (identifier-syntax id) (make-rename-transformer #'id))
(define-syntax-rule (define-alias alias symbol)
  (define-syntax alias (make-rename-transformer #'symbol)))

(define-syntax my-if
  (syntax-rules ()
    ((_ cond then) (when cond then))
    ((_ cond then else) (if cond then else))
    ((_ cond then else rest ...) (if cond then (begin else rest ...)))))
;;; while and define-macro are taken from guile and slightly modifed
;;; to work with racket
(define-syntax (while stx)
  (syntax-case stx ()
    ((while cond body ...)
     #`(let ((break-tag (make-continuation-prompt-tag 'break))
             (continue-tag (make-continuation-prompt-tag 'continue)))
         (call-with-continuation-prompt
          (lambda ()
            ;;Figure out how to fix this so I can break without a value
            (define-syntax #,(datum->syntax #'while 'break)
              (lambda (x)
                (syntax-case x ()
                  ((_)
                   #'(abort-current-continuation break-tag #f))
                  ((_ arg args (... ...))
                   #'(abort-current-continuation break-tag arg args (... ...)))
                  (_
                   #'(lambda args
                       (apply abort-current-continuation break-tag args))))))
            (let lp ()
              (call-with-continuation-prompt
               (lambda ()
                 (define-syntax #,(datum->syntax #'while 'continue)
                   (lambda (x)
                     (syntax-case x ()
                       ((_)
                        #'(abort-current-continuation continue-tag))
                       ((_ . args)
                        (error 'continue "too many arguments" x))
                       (_
                        #'(lambda ()
                            (abort-current-continuation continue-tag))))))
                 (do () ((not cond) #f) body ...))
               continue-tag
               (lambda (k) (lp)))))
          break-tag
          (lambda (arg . args)
            (my-if (not arg)
                   #t
                   (apply values (list* arg args)))))))))
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
(define (module-ref mod var)
  (namespace-variable-value var (module->namespace mod)))
(define (symbol-ref var)
  (namespace-variable-value var (current-namespace)))
(define-syntax (symbol-bound? stx)
  (syntax-case stx ()
    ((_ sym)
     (if (identifier-binding #'sym) (syntax #t) (syntax #f)))))

(define (macroexpand body) (syntax->datum (expand body)))
(define (macroexpand-1 body) (syntax->datum (expand-once body)))

(struct exn:fail:assertion exn:fail (srcloc)
        #:property prop:exn:srclocs
        (lambda (x) (list (exn:fail:assertion-srcloc x)))
        #:extra-constructor-name make-exn:fail:assertion
        #:transparent)
(struct exn:fail:assertion-simple exn:fail ()
        #:extra-constructor-name make-exn:fail:assertion-simple
        #:transparent)

(define-syntax (here stx)
    #`(list (quote-source-file #,stx)
            (quote-line-number #,stx)
            (quote-column-number #,stx)
            (quote-character-position #,stx)
            (quote-character-span #,stx)))
(define-syntax (assert! args)
  (syntax-case args ()
    ((_ expr)
     (quasisyntax/loc args
       (unless expr
         (raise (make-exn:fail:assertion-simple
                 (format #f "Assertation failure: ~s~%~s:~s:~s:~s:~s" 'expr
                         (quote-source-file expr)
                         (quote-line-number expr)
                         (quote-column-number expr)
                         (quote-character-position expr)
                         (quote-character-span expr))
                 (current-continuation-marks))))))))
(define-syntax-rule (with-caught-exceptions f body ...)
  (let ((prompt (make-continuation-prompt-tag)))
    (call-with-continuation-prompt
     call-with-exception-handler;;call this
     prompt;;with this prompt
     f;;and this handler
     ;;and these args
     (lambda (exn) (abort-current-continuation prompt exn))
     (lambda () body ...))))

(provide (all-defined-out)
         (all-from-out
          racket/bytes rnrs/bytevectors-6 rnrs/io/ports-6 racket/base racket/match
          racket/function racket/future racket/string racket/math racket/list
          racket/pretty racket/sequence srfi/48 srfi/71 racket/vector racket/syntax
          racket/unsafe/ops))



;;An attempt to write while using syntax-parameters, it doesn't work

;; (define-syntax-parameter break
;;   (lambda (stx)
;;     (raise-syntax-error (syntax-e stx) "can only be used in a while loop")))
;; (define-syntax-parameter continue
;;   (lambda (stx)
;;     (raise-syntax-error (syntax-e stx) "can only be used in a while loop")))
;; (define-syntax (while stx)
;;   (syntax-case stx ()
;;     ((while cond body ...)
;;      #`(let ((break-tag (make-continuation-prompt-tag 'break))
;;              (continue-tag (make-continuation-prompt-tag 'continue)))
;;          (call-with-continuation-prompt
;;           (lambda ()
;;             ;;Figure out how to fix this so I can break without a value
;;             ;;(define-syntax #,(datum->syntax #'while 'break)
;;             (syntax-parameterize 
;;                 ((break (lambda (x)
;;                           (syntax-case x ()
;;                             ((_)
;;                              #'(abort-current-continuation break-tag))
;;                             ((_ arg (... ...))
;;                              #'(abort-current-continuation
;;                                 break-tag arg (... ...)))
;;                             (_
;;                              #'(lambda args
;;                                  (apply abort-current-continuation
;;                                         break-tag args)))))))
;;               (let lp ()
;;                 (call-with-continuation-prompt
;;                  (lambda ()
;;                    (syntax-parameterize
;;                        ((continue
;;                          (lambda (x)
;;                            (syntax-case x ()
;;                              ((_)
;;                               #'(abort-current-to-prompt continue-tag))
;;                              ((_ . args)
;;                               (error 'continue "too many arguments" x))
;;                              (_
;;                               #'(lambda ()
;;                                   (abort-current-to-prompt continue-tag)))))))
;;                      (do () ((not cond) #f) body ...))
;;                    continue-tag
;;                    (lambda (k) (lp)))))
;;               break-tag
;;               (lambda (k . args)
;;                 (my-if (null? args)
;;                        #t
;;                        (apply values args))))))))))
