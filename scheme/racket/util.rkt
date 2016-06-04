#lang racket/base
(require "init.rkt"
         (for-syntax "init.rkt"))
;;;Aliases
(define-alias prog1 begin0)
(define-alias progn begin)
(define-alias string-strip string-trim)
(define-alias list->values unlist)
(define-alias vector->values unvector)
(define-alias string->bytes string->bytes/latin-1)
(define-alias find memf)
(define-alias false? not)
(define-alias concat string-append)
(define-alias 1- sub1)
(define-alias 1+ add1)
(define-alias iota range)
(define (true? x) (not (not x)))

;;;Control-forms
;;TODO: rewrite these using delimited contunations + syntax paramaters
;;(or rewrite them in terms of while, but I'd still like to rewrite while using
;; syntax parameters, first)
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
;;Due to internal definitions you can't conditionally define a top level
;;symbol using normal if stmts, this is about as close as you can get to that
(define-macro (static-if cond then . else)  
  (if cond
      `(begin ,then)
      `(begin ,@else)))
;;Returns the result of body, if any exceptions are raised they
;;are caught, the context of the exception is escapes and the result
;;of calling f with the exception is returned
(define-syntax-rule (false-if-exception body ...)
  (with-caught-exceptions (lambda (exn) #f) body ...))
(define-syntax-rule (return-exceptions body ...)
  (with-caught-exceptions (lambda (exn) exn) body ...))
(define-syntax-rule (false-if-exception body ...)
   (with-caught-exceptions (lambda (exn) #f) body ...)))
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

(define-syntax-rule (incf place)
  (let ((ret (1+ place)))
    (set! place ret)
    ret))
(define-syntax-rule (decf place)
  (let ((ret (1- place)))
    (set! place ret)
    ret))

;;A form of while without break/continue support, should be faster
(define-syntax-rule (while-do test body1 body2 ...)
  (do () ((not test) #f) body1 body2 ...))
(define-syntax-rule (do-while test body1 body2 ...)
  (begin
    body1 body2 ...
    (do () ((not test) #f) body1 body2 ...)))

(define-macro (concat-lit . args) (apply string-append args))

;;since we have format-symbol I probably don't need this
(define-macro (build-symbol . args)
  `(string->symbol
    (string-join
    (map (lambda (x) (my-if (symbol? x) (symbol->string x) x))
         (list ,@args)) "")))

;; (define-for-syntax (module-ref mod var)
;;   (namespace-variable-value var (module->namespace mod)))
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

;; (begin-for-syntax
;;  (define-syntax (symbol-bound? stx)
;;   (syntax-case stx ()
;;     ((_ sym)
;;      (if (identifier-binding #'sym) (syntax #t) (syntax #f))))))



;; (begin-for-syntax
;;  (define-syntax (here stx)
;;     #`(list (quote-source-file #,stx)
;;             (quote-line-number #,stx)
;;             (quote-column-number #,stx)
;;             (quote-character-position #,stx)
;;             (quote-character-span #,stx))))
;; (define-macro (assert! expr)
;;   `(unless ,expr
;;      (raise (make-exn:fail:assertion-simple
;;              (format #f "Assertation-failure: ~s~%~s"
;;                      'expr ,(here))
;;              (current-continuation-marks)))))

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

(define (nth n list)
  (let loop ((ls list) (n n))
    (if (null? ls) ls
        (if (zero? n) (car ls)
            (loop (cdr ls) (1- n))))))
(define (sloppy-assf proc list)
  (let loop ((ls list))
    (if (null? ls) #f
        (if (and (list? (car ls))
                 (proc (caar ls)))
            (car ls)
            (loop (cdr ls))))))
(define (sloppy-assq elt list)
  (sloppy-assf (lambda (x) (eq? x elt)) list))
(define (sloppy-assv elt list)
  (sloppy-assf (lambda (x) (eqv? x elt)) list))
(define (sloppy-assoc elt list)
  (sloppy-assf (lambda (x) (equal? x elt)) list))
(define-syntax-rule (make-future body1 body2 ...)
  (future (lambda () (begin body1 body2 ...))))
;;pushes value onto a list stored in key in ht
;;a list is created if one dosent't
(define (hash-push! ht key value)
  (hash-update! ht key (lambda (x) (cons value x)) null))
;;define-once is way more complicated that it should be
;;this is a kinda lame way to do this, but I can't think of another way
;;since you can't define a top level variable inside of a conditional
(define-macro (define-once var expr)
  `(define ,var (or (false-if-exception ,var) ,expr)))
  ;; (let ((boundp (false-if-exception (or var (not var)))))
  ;;   (if boundp
  ;;       `(define ,var ,var)
  ;;       `(define ,var ,expr))))
  ;; (syntax-rules ()
  ;;   ((_ name body)
  ;;    (define name (if (symbol-bound? name) (symbol-ref 'name) body)))
  ;;   ((_ (name args ...) body1 body2 ...)
  ;;    (define (name args ...)
  ;;      (if (symbol-bound? name) ((symbol-ref 'name) args ...)
  ;;          (begin body1 body2 ...))))))

(define (string->char str)
  (string-ref str 0))
(define (char->string char)
  (string char))
;;Plumbers are rackets means of calling functions at process exit, this is
;;a simple wrapper around them to emulate libc's atexit (which doesn't work
;;via ffi due to issues related to dynamic loading)
(define (racket-atexit proc . args)
  (let ((f (lambda (x) (plumber-flush-handle-remove! x);;proobably unecessary
                   (apply proc args))))
    (plumber-add-flush! (current-plumber) f)))
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
;;         (for-syntax (all-from-out racket/base racket/string racket/syntax))
;;         (for-meta 2 (all-from-out racket/base)))
(provide (all-defined-out)
         (all-from-out "init.rkt"))
