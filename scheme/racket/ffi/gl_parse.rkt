#lang racket/base
(require "../util.rkt" "../io.rkt" "ffi.rkt")
(require (for-syntax racket/base racket/syntax))
(require xml)

;;This is unused, mostly since an xml file doesn't really convert to a
;;hash table because it doesn't have unique element names
;;The xml file that this is parsing doesn't have any 'entities' or 'cdata'
(define (xml->hash source)
  ;;for now assume source is a document struct, I can extend this later
  ;;to take a file/string as well.
  (let loop ((elt (document-element source)) (ht #f))
    (let*-values (((attributes) (element-attributes elt))
                    ((elements rest)
                     (partition element? (element-content elt)))
                    ((data)
                     (string-join
                      (map (lambda (x) (string-trim (pcdata-string x)))
                           (filter pcdata? rest)) "")))
      (if (and (null? attributes) (null? elements))
          data
          (begin
            (set! ht (make-hash))
            (for-each
             (lambda (x)
               (hash-set! ht (attribute-name x) (attribute-value x)))
             attributes)
            (for-each
             (lambda (x)
               (aif (hash-ref ht (element-name x) #f)
                    (hash-set! ht (element-name x)
                               (cons (loop x #f) (cons it null)))
                    (hash-set! ht (element-name x) (loop x #f))))
             elements)
            (when (non-empty-string? data)
                (hash-set! ht 'content data))
            ht)))))
;;These are defined mostly in the order they apear in the xml file
(define (un-camel-case id)
  (let ((matcher (make-regexp-matcher #rx"[a-z0-9]([A-Z])(.)" id))
        (result null))
    (while (regexp-matcher-next-match matcher)
      (
      
(define (translate-ids id)
  (match id
     ;;name of an enum, keep GL uppercase, downcase everything else and '_'->'-'
    ((regexp #rx"GL_(.+)" (list _ name))
     (concat "GL-" (string-downcase name)))
    ((
     
;;;The file starts with typedefs
(define gl-typedef-regex
  #px"typedef *((?:un)?signed)? *(\\*+) *")
(define (do-struct-typedef . args) (void))
(define-macro (string->ctype str unsigned)
  (if unsigned
      `(format-symbol "_u~a" ,str)
      `(format-symbol "_~a" ,str)))
(define (parse-typedef str)
  (let* ((re-match (regexp-match gl-typedef-regex str))
         ((values $0 $1 $2 $3) (list->values re-match)))
    (cond
     (($3) _pointer)
     ;;I'll probably end up ignoring these
     ((equal? $2 void) _function-pointer)
     (else
      (let ((unsigned (equal? $1 "unsigned")))
        (string->ctype $3 unsigned))))))

(define (parse-type xml)
  ;;This ignores types for gles
  (unless (ormap (lambda (x)
               ;;might needto be 'requires
               (and (equal? (attribute-name x) 'requires)
                    (equal? (attribute-value x) 'khrplatform))))
  (match (element-content)
    ((list typedef typename semicolon)
     (let ((ctype (parse-typedef (pcdata-string typedef))))
       (if (not ctype) null
           `(define ,(format-symbol "_~a" typename) ,ctype))))
    (_ null))))
;;;Next are a set of enum groups which we ignore since the same information
;;is provided with the actual definitions of the enums
(define (parse-enum enum)
  (let (((values value name . rest) (list->values (element-attributes enum))))
    (assert! (eq? 'value (attribute-name value)))
    (assert! (eq? 'name (attribute-name name)))
    `(define ,(string->symbol (attribute-value name))
       ,(libc-strtol (string->bytes (attribute-value value))))))
(define (parse-enums enums)
  (let ((group (find (lambda (x) (eq? (attribute-name x) 'group)))))
    `(define ,(string->symbol (regexp-replace 
             
