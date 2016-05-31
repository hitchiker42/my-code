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
;;         racket/port ;;with-{input,output}-to-{file,string,...}
         rnrs/io/ports-6 ;;racket doesn't have input/output ports, somehow
         racket/match ;;ml style pattern matching
         racket/unsafe/ops
         (except-in racket/list last)
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

(define-alias prog1 begin0)
(define-alias progn begin)
(define-alias string-strip string-trim)
(define-alias list->values unlist)
(define-alias vector->values unvector)
(define-alias string->bytes string->bytes/latin-1)
(define-alias find memf)
(define-alias false? not)
(define-alias concat string-append)
(define (true? x) (not (not x)))
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
;;TODO: look at set! transformers and try and use those here
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
(define (last l)
  (if (list? l)
      (if (pair? l)
          (let loop ((l l) (x (cdr l)))
            (if (pair? x)
                (loop x (cdr x))
                (car l)))
          null)
      (raise-argument-error 'last "list?" l)))
(define-alias 1- sub1)
(define-alias 1+ add1)
(define-syntax-rule (incf place)
  (let ((ret (1+ place)))
    (set! place ret)
    ret))
(define-syntax-rule (decf place)
  (let ((ret (1- place)))
    (set! place ret)
    ret))
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
;;A form of while without break/continue support, should be faster
(define-syntax-rule (while-do test body1 body2 ...)
  (do () ((not test) #f) body1 body2 ...))
(define-syntax-rule (do-while test body1 body2 ...)
  (begin
    body1 body2 ...
    (do () ((not test) #f) body1 body2 ...)))
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
;;There's not really a way around this my I want macro defining macros
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

(define-macro (concat-lit . args) (apply string-append args))
;;since we have format-symbol I probably don't need this
(define-macro (build-symbol . args)
  `(string->symbol
    (string-join
    (map (lambda (x) (my-if (symbol? x) (symbol->string x) x))
         (list ,@args)) "")))
(define (module-ref mod var)
  (namespace-variable-value var (module->namespace mod)))
(define-for-syntax (module-ref mod var)
  (namespace-variable-value var (module->namespace mod)))
(define (symbol-ref var)
  (namespace-variable-value var (current-namespace)))
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
(define-macro (type-case expr . clauses)
  (let ((val (gensym)))
    `(let ((,val ,expr))
       (cond
        ,@(map (lambda (x)
                (if (eq? 'else (car x))
                    (list* 'else (cdr x))
                    (list* (list (format-symbol "~a?" (car x)) val)
                           (cdr x)))) clauses)))))
(define-macro (pred-case expr . clauses)
  (let ((val (gensym)))
    `(let ((,val ,expr))
       (cond
        ,@(map (lambda (x)
                 (if (eq? 'else (car x))
                     (list* 'else (cdr x))
                     (list* (list (car x) val)
                            (cdr x)))) clauses)))))
(define-syntax (aif stx)
  (with-syntax ((it (datum->syntax stx 'it)))
    (syntax-case stx ()
      ((_ cond then)
       #'(let ((it cond)) (if it then (void))))
      ((_ cond then else)
       #'(let ((it cond)) (if it then else)))
      ((_ cond then else rest ...)
       #'(let ((it cond))
           (if it then (begin else rest ...)))))))
(define-syntax (symbol-bound? stx)
  (syntax-case stx ()
    ((_ sym)
     (if (identifier-binding #'sym) (syntax #t) (syntax #f)))))

(require racket/pretty)
(define-alias pprint pretty-print)
(define (macroexpand body) (syntax->datum (expand body)))

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
(begin-for-syntax
 (define-syntax (here stx)
    #`(list (quote-source-file #,stx)
            (quote-line-number #,stx)
            (quote-column-number #,stx)
            (quote-character-position #,stx)
            (quote-character-span #,stx))))
;; (define-macro (assert! expr)
;;   `(unless ,expr
;;      (raise (make-exn:fail:assertion-simple
;;              (format #f "Assertation-failure: ~s~%~s"
;;                      'expr ,(here))
;;              (current-continuation-marks)))))
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
(define-alias iota range)
(define (hash->alist ht)
  (hash-map ht
            (lambda (key value)
              (if (hash? value)
                  (cons key (hash->alist value))
                  (cons key value)))))
(define-syntax hash-ref-multi
  (syntax-rules ()
    ((_ ht key) (hash-ref ht key))
    ((_ ht key keys ...) (hash-ref (hash-ref-multi ht key) keys ...))))
;;Returns the result of body, if any exceptions are raised they
;;are caught and #f is returned
(define-syntax-rule (false-if-exception body ...)
  (let ((prompt (make-continuation-prompt-tag)))
    (call-with-continuation-prompt
     call-with-exception-handler
     prompt
     (lambda () #f)
     (lambda (exn) (abort-current-continuation prompt))
     (lambda () (begin body ...)))))
(define (nth n list)
  (let loop ((ls list) (n n))
    (if (null? ls) ls
        (if (zero? n) (car ls)
            (loop (cdr ls) (1- n))))))

(define-syntax-rule (make-future body1 body2 ...)
  (future (lambda () (begin body1 body2 ...))))
;;this is a kinda lame way to do this, but I can't think of another way
;;since you can't define a top level variable inside of a conditional
(define-syntax define-once
  (syntax-rules ()
    ((_ name body)
     (define name (if (symbol-bound? name) (symbol-ref 'name) body)))
    ((_ (name args ...) body1 body2 ...)
     (define (name args ...)
       (if (symbol-bound? name) ((symbol-ref 'name) args ...)
           (begin body1 body2 ...))))))

(define (string->char str)
  (string-ref str 0))
(define (char->string char)
  (string char))

;; (define-macro (regexp-bind pat input . body)
;;   `(let ((matched (regexp-match ,pat ,input)))
;;      (unless (null? matched)
;;        (when (< (length matched) 10)
;;          (set! (matched (append matched
;;                                 (make-list (- 10 (length matched)) #f)))))
;;        (match matched
;;          ((list-rest $@ $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $0)
;;(define internal-xorshift-state (make-bytevector 16))
;;(define xorshift-rand ((state internal-xorshift-state))

(provide (all-defined-out)
         (all-from-out
          racket/bytes rnrs/bytevectors-6 rnrs/io/ports-6 racket/base racket/match
          racket/function racket/future racket/string racket/math racket/list
          racket/sequence srfi/48 srfi/71 racket/vector racket/syntax racket/unsafe/ops))
;;         (for-syntax (all-from-out racket/base racket/string racket/syntax))
;;         (for-meta 2 (all-from-out racket/base)))
