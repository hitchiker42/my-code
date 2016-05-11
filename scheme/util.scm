(define-module (util)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (system foreign)
  #:use-module (ice-9 hash-table)
  ;;Most stuff I export using define-pubilc, these are mostly macros
  ;;and inlineable procedures
  #:export (progn list* prog1 typecase dolist as-list pop push concat
            incf decf concat-lit car-safe cdr-safe bytevector-slice
            bytevector->string equal-any? print-hash-table build-hash-table
            hash-table keyword->symbol->string list->hashtable))

;;;;Macros
;;; Symbol aliases
(define-syntax progn
;;  "Alias for begin"
  (identifier-syntax begin))
(define-syntax list*
;;  "Alias for cons*"
  (identifier-syntax cons*))

;;;Control structures
(define-syntax-rule (prog1 first rest ...)
  ;;"Evaluate first and rest sequentally, return the value of first"
  (let ((ret first)) (begin rest ...) ret))

(define-syntax-rule (dolist (var list) exp exp* ...)
  (for-each (lambda (x) (let ((var x)) exp exp* ...)) list))

;;For what it's worth I tried to do this with syntax-case
;;The fact I need to do so much symbol manipulation means
;;its a lot eaiser to use define-macro
(define-macro (typecase expr . clauses)
  (let ((val (gensym)))
    `(let ((,val ,expr))
       (cond
        ,@(map (lambda (x)
                (if (eq? #t (car x))
                    (list* #t (cdr x))
                    (list* (list (string->symbol
                                  (concat (symbol->string (car x)) "?")) val)
                           (cdr x)))) clauses)))))
(define-syntax-rule (define-public* (name args ...) . body)
  (begin (define* (name args ...) body) (export name)))
;;;Function-like macros
(define-syntax-rule (as-list x)
  ;;  "If x is not a list return (list x) otherwise return x"
  (if (list? x) x (list x)))
(define-syntax-rule (pop! ls)
  (prog1 (car ls) (set! ls (cdr ls))))
(define-syntax-rule (push elt place)
  (set! place (cons elt place)))
(define-syntax-rule (decf y)
  (set! y (1- y)))
(define-syntax-rule (incf y)
  (set! y (1+ y)))
(define-syntax-rule (concat string strings ...)
  (string-join (list string strings ...) ""))

;;Concatenate strings at macroexpansion time, allows writing long string
;;constants without looking ugly or suffering a performance penalty

;;This needs to be use define-macro since it needs to act on code, not
;;syntax objects
(define-macro (concat-lit . strings)
  (string-join strings ""))

;;These are needed for using #, to read hashtables, but are useful
;;on their own
(define-syntax build-hash-table
  (syntax-rules ()
    ((_ table (key value) rest ...)
     (begin
       (hash-set! table key value)
       (build-hash-table table rest ...)))
    ((_ table)
     table)))

(define-syntax-rule (hash-table (key value) ...)
  (let ((table (make-hash-table)))
    (build-hash-table table (key value) ...)))

;;;;Utility Functions
;;;Sequence functions
(define-inlinable (car-safe obj)
  "If obj is a pair return (car obj) otherwise return '()"
  (if (pair? obj) (car obj) '()))
(define-inlinable (cdr-safe obj)
  "If obj is a pair return (cdr obj) otherwise return '()"
  (if (pair? obj) (cdr obj) '()))
;;;String Functions
(define-public (string-split str delim)
  "Split string into substrings delimited by delim.
Returns a list of substrings"
  (let acc ((index 0)
            (output '()))
    (let ((temp (string-index str delim (1+ index))))
      (if temp
          (acc temp (cons (substring str index temp) output))
          (reverse! (cons (substring str index) output))))))
(define-public (string-strip str)
  "returns a copy of str with leading and trailing whitespace removed"
  (let ((start (string-skip str char-set:whitespace))
        (end (string-skip-right str char-set:whitespace)))
    (substring str start (1+ end))))
;;;IO functions
(define-public* (print obj #:optional (port (current-output-port)))
  "write obj to port, followed by a newline"
  (write obj port)
  (newline port))
(define-public* (pprint obj #:optional (port (current-output-port)))
  "display obj on port, followed by a newline"
  (display obj port)
  (newline port))
(define-public (read-from-string str)
  (with-input-from-string str (lambda () (read))))
(define-public (read-from-string-and-eval! str)
  (with-input-from-string str (lambda () (read-and-eval!))))
;;;Type conversions/Value creation
(define-public (integer->bitvector n)
  "Convert an integer to a bitvector such that the most significant
bit in the integer is the first bit in the bitvector"
  (let* ((num-bits (integer-length n))
         (bit (1- num-bits))
         (vec (make-bitvector num-bits)))
    (while (> bit 0)
      (bitvector-set! vec (- num-bits 1 bit) (logbit? bit n))
      (decf bit))
    vec))

(define-inlinable (bytevector-slice bv start len)
  "Return a view into the bytevector bv starting at index 'start' for 'len' bytes"
  (when (> (+ start len) (bytevector-length bv))
    (scm-error 'out-of-range "bytevector-slice"
               "bv-len:~a, start:~a len:~a"
               (list (bytevector-length bv) start len) #f))
  (let ((ptr (bytevector->pointer bv start)))
    (pointer->bytevector ptr len)))
(define-inlinable (bytevector->string bv start len)
  (utf8->string (bytevector-slice bv start len)))
(define (keyword->symbol->string key)
  "Return a string representation of the symbol with the same name as key"
  (symbol->string (keyword->symbol key)))
;; (define (list->hash-table ls)
;;   "Convert the list ls into a hash table.
;; elements of the list should have the form (key value)")


;;;Networking
(define (get-ip-addr host)
  "Convert a hostname into an ipv4 ip address"
  (sockaddr:addr (addrinfo:addr (car (getaddrinfo host)))))
(define* (connect-utf8-tcp host port)
  (when (string? host)
    (set! host (get-ip-addr host)))
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (connect sock AF_INET host port)
    (set-port-encoding! sock "UTF-8")
    sock))


;;;Predicates
;;This should be a macro but its too hard to make it work
(define (equal-any? val args)
  (if (not (pair? args))
      (equal? val args)
      (let ((retval #f))
        (while (pair? args)
          (if (equal? val (pop args))
              (set! retval #t)
              break))
        retval)))
;;Define read syntax for hashtable literals of the form:
;;#,(hash (key value)...)
;;This is global so doesn't need to be exported, but instead
;;needs to be evalueated when this module is loaded
(define* (print-hash-table ht #:optional (port (current-output-port)))
  ;;Use a named let for recursion so we can print a newline at the
  ;;end, but not for any intermediate values
  (let print-ht ((ht ht) (port port))
    (display "#,(hash" port)
    (hash-for-each
     (lambda (key val)
       (if (hash-table? val)
           (begin
             (format port "\n\t(~s\n\t" key)
             (print-ht val port)
             (display ")" port))
           (format port "\n\t(~s ~s)" key val))) ht)
    (display ")" port))
  (display "\n" port))

(eval-when (eval load compile expand)
  (use-modules (srfi srfi-10))
  (define-reader-ctor 'hash
    (lambda elems
      `(hash-table ,@elems))))
