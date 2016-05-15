(define-module (util)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (system foreign)
  #:use-module (ice-9 hash-table)
  ;;Most stuff I export using define-pubilc, these are mostly macros
  ;;and inlineable procedures
  #:export (progn list* prog1 typecase dolist as-list pop! push! concat
            incf decf concat-lit car-safe cdr-safe cl-car cl-cdr aref vref
            thunk case-equal bytevector->string bytevector-memcpy
            equal-any? print-hash-table build-hash-table hash-table
            keyword->symbol->string list->hashtable))

(define null '())
;;;;Macros
;;; Symbol aliases
(define-syntax progn
;;  "Alias for begin"
  (identifier-syntax begin))
(define-syntax list*
;;  "Alias for cons*"
  (identifier-syntax cons*))
(define-syntax aref (identifier-syntax array-ref))
(define-syntax vref (identifier-syntax vector-ref))

;;;Control structures
(define-syntax-rule (prog1 first rest ...)
  ;;"Evaluate first and rest sequentally, return the value of first"
  (let ((ret first)) (begin rest ...) ret))
(define-syntax-rule (dolist (var list) exp exp* ...)
  (for-each (lambda (x) (let ((var x)) exp exp* ...)) list))
;; This should let you break out of a dolist, but it doesn't
;; (let ((ls list))
;;   (let ((var (pop! ls)))
;;     (while (not (null? var))
;;       exp exp* ...
;;       (set! var (pop! ls))))))

(define-syntax-rule (thunk exp ...)
  (lambda () exp ...))

;;For what it's worth I tried to do this with syntax-case
;;The fact I need to do so much symbol manipulation means
;;its a lot eaiser to use define-macro
(define-macro (typecase expr . clauses)
  (let ((val (gensym)))
    `(let ((,val ,expr))
       (cond
        ,@(map (lambda (x)
                (if (eq? 'else (car x))
                    (list* 'else (cdr x))
                    (list* (list (string->symbol
                                  (concat (symbol->string (car x)) "?")) val)
                           (cdr x)))) clauses)))))
(define-macro (case-equal expr . clauses)
  ;;Like case but compare with equal, also allow the car of
  ;;each clause to be an atom instead of a list
  (let ((val (gensym)))
    ;;Don't evaluate expr more than once
    `(let ((,val ,expr))
       (cond
        ,@(map (lambda (x)
                 (if (eq? 'else (car x))
                     (list* 'else (cdr x))
                     (list* (if (list? (car x))
                                   `(or ,@(map (lambda (x)
                                                 `(equal? ,expr ,x)) (car x)))
                                   `(equal? ,expr ,(car x)))
                            (cdr x)))) clauses)))))
;;Using define-syntax makes docstrings not work
(define-macro (define-public* def . body)
  (let ((name (car def)))
    `(begin (define* ,def ,@body) (export ,name))))
;;;Function-like macros
(define-syntax-rule (as-list x)
  ;;  "If x is not a list return (list x) otherwise return x"
  (if (list? x) x (list x)))
(define-syntax-rule (pop! ls)
  (prog1 (cl-car ls) (set! ls (cl-cdr ls))))
(define-syntax-rule (push! elt place)
  (set! place (cons elt place)))
(define-syntax-rule (decf y)
  (set! y (1- y)))
(define-syntax-rule (incf y)
  (set! y (1+ y)))
(define-syntax-rule (concat string strings ...)
  (string-join (list string strings ...) ""))
(define-syntax-rule (concat-path string strings ...)
  (string-join (list string strings ...) file-name-separator-string))
(define-syntax-rule (assert expr)
  (when (not expr)
    (throw 'assert (format #f "~s" 'expr))))
;;Concatenate strings at macroexpansion time, allows writing long string
;;constants without looking ugly or suffering a performance penalty

;;This needs to be use define-macro since it needs to act on code, not
;;syntax objects
(define-macro (concat-lit . strings)
  (string-join strings ""))

;;These used to be defined really elegently using syntax-rules,
;;pattern matching and recursion, but that caused a stack overflow when
;;reading large hash tables (apperently macros aren't tail recursive)
(define-macro (build-hash-table table . rest)
  `(begin
     (when ',rest
       (for-each (lambda (x) (hash-set! ,table (car x) (cdr x))) ',rest))
     ,table))
(define-macro (hash-table . rest)
  (let ((table (gensym)))
    `(let ((,table (make-hash-table)))
       (build-hash-table ,table ,@rest))))
;;;;Utility Functions
;;;Sequence functions
(define-inlinable (car-safe obj)
  "If obj is a pair return (car obj) otherwise return '()"
  (if (pair? obj) (car obj) '()))
(define-inlinable (cl-car ls)
  "if ls is null? return null, otherwise return (car ls)"
  (if (null? ls) ls (car ls)))
(define-inlinable (cdr-safe obj)
  "If obj is a pair return (cdr obj) otherwise return '()"
  (if (pair? obj) (cdr obj) '()))
(define-inlinable (cl-cdr ls)
  "if ls is null? return null, otherwise return (cdr ls)"
  (if (null? ls) ls (cdr ls)))
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
(define-public* (print-err obj)
  "write obj to the current error port, followed by a newline"
  (let ((port (current-error-port)))
    (write obj port)
    (newline port)))
(define-public (read-from-string str)
  (with-input-from-string str (lambda () (read))))
(define-public (read-from-string-and-eval! str)
  (with-input-from-string str (lambda () (read-and-eval!))))
(define-public (read-from-file filename)
  (with-input-from-file filename (lambda () (read))))
(define-public (read-from-file-and-eval filename)
  (with-input-from-file filename (lambda () (read-and-eval!))))
;;;Low level operations
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

(define-public* (bytevector-slice bv start #:optional (end (bytevector-length bv)))
  "Return a view into the bytevector bv starting at index 'start' for 'len' bytes"
  (when (> end (bytevector-length bv))
    (scm-error 'out-of-range "bytevector-slice" "invalid length" #f #f))
  (let ((ptr (bytevector->pointer bv start)))
    (pointer->bytevector ptr (- end start))))

;;Define a scheme function name which provides a thin wrapper over the
;;C function of the same name (with - replaced by _ first).
(define-syntax-rule (gen-c-wrapper name ret-type arg-types)
  (define-public name
    (pointer->procedure
     ret-type
     (dynamic-func
      (string-map (lambda (x) (if (eq? x #\-) #\_ x))
                  (symbol->string 'name)) (dynamic-link))
     arg-types)))
(gen-c-wrapper scm-gc-malloc-pointerless '* (list size_t '*))
(gen-c-wrapper scm-gc-malloc '* (list size_t '*))
(gen-c-wrapper scm-gc-realloc '* (list '* size_t size_t '*))
(gen-c-wrapper memcpy '* (list '* '* size_t))
(gen-c-wrapper memset '* (list '* int size_t))

(define-public (gc-malloc-pointerless sz)
  (scm-gc-malloc-pointerless sz (string->pointer "bytevector")))
(define-public (gc-malloc sz)
  (scm-gc-malloc sz (string->pointer "bytevector")))
(define-public (gc-realloc ptr old-sz new-sz)
  (scm-gc-realloc ptr old-sz new-sz (string->pointer "bytevector")))
;; (define-macro (gc-realloc! ptr old-sz new-sz)
;;   `(set! ',ptr
;;     (scm-gc-realloc ,ptr old-sz new-sz (string->pointer "bytevector"))))
(define-public (pointer-offset ptr offset)
  ;;I love that guile let's me do pointer arithmetic in scheme
  (let ((addr (pointer-address ptr)))
    (make-pointer (+ addr offset))))

(define (bytevector-memcpy-explicit dest dest-offset src src-offset len)
  (when (or (< (bytevector-length dest) (+ dest-offset len))
            (< (bytevector-length src) (+ src-offset len)))
    (scm-error 'out-of-range "bytevector-memcpy" "invalid length" #f #f))
  (let ((dest-ptr (bytevector->pointer dest dest-offset))
        (src-ptr (bytevector->pointer src src-offset)))
    (memcpy dest-ptr src-ptr len)))
;;syntax-rules is kinda awesome, I could write this with define-macro
;;but it'd have a lot of (if (pair? src) ...) (if (pair? dest) ...)
(define-syntax bytevector-memcpy
  (syntax-rules ()
    ((_ (dest dest-offset) (src src-offset) len)
     (bytevector-memcpy-explicit dest dest-offset src src-offset len))
    ((_ dest (src src-offset) len)
     (bytevector-memcpy (dest 0) (src src-offset) len))
    ((_ (dest dest-offset) src len)
     (bytevector-memcpy (dest dest-offset) (src 0) len))
    ((_ dest src len)
     (bytevector-memcpy (dest 0) (src 0) len))
    ((_ dest src)
     (let ((len (min (bytevector-length src) (bytevector-length dest))))
       (bytevector-memcpy dest src len)))))
;;Kind of a misleading name, it doesn't extend bv, it creates a new bytevector
;;sz bytes longer than bv, with the same initial contents as bv
(define-public (bytevector-extend bv sz)
  (let* ((old-length (bytevector-length bv))
         (new-length (+ old-length sz))
         (ptr (gc-malloc-pointerless new-length)))
    (memcpy ptr (bytevector->pointer bv) old-length)
    (memset (pointer-offset ptr old-length) 0 sz)
    (pointer->bytevector ptr new-length)))
(define-inlinable (bytevector->string bv start end)
  (utf8->string (bytevector-slice bv start end)))
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
(define-public (equal-any? val args)
  (if (not (pair? args))
      (equal? val args)
      (let ((retval #f))
        (while (pair? args)
          (if (equal? val (pop! args))
              (set! retval #t)
              break))
        retval)))
(define-public (true? x)
  (if x #t #f))
(define-public (false? x)
  (not x))
;;Define read syntax for hashtable literals of the form:
;;#,(hash (key value)...)
;;This is global so doesn't need to be exported, but instead
;;needs to be evalueated when this module is loaded
(define* (print-hash-table ht #:optional (port (current-output-port)))
  ;;Use a named let for recursion so we can print a newline at the
  ;;end, but not for any intermediate values
  (let print-ht ((ht ht) (port port) (indent "    "))
    (display "#,(hash" port)
    (hash-for-each
     (lambda (key val)
       (if (hash-table? val)
           (begin
             (format port "\n~a(~s\n~a" indent key indent)
             (print-ht val port (concat indent "    "))
             (display ")" port))
           (format port "\n~a(~s '~s)" indent key val))) ht)
    (display ")" port))
  (display "\n" port))
(define-public (hash-table-size ht)
  (hash-count (const #t) ht))
(define-public (hash-table-merge a b)
  (hash-for-each (lambda (key value)
                   (unless (hash-ref a key)
                     (hash-set! a key value))) b))
(define-public (hash-map-keys->list ht f)
  (hash-map->list (lambda (key val) (f key)) ht))
(define-public (hash-map-values->list ht f)
  (hash-map->list (lambda (key val) (f key)) ht))
(define-public (hash-ref-multi ht . ref)
  (let acc ((ht ht) (ref ref))
    (if (null? ref) ht
        (acc (hash-ref ht (car ref)) (cdr ref)))))
(define-public* (array-map f #:rest arrs)
  (let ((ret (apply make-array #f (array-rank (car arrs)))))
    (apply array-map! ret f arrs)
    ret))
(eval-when (eval load compile expand)
  (use-modules (srfi srfi-10))
  (define-reader-ctor 'hash
    (lambda elems
      `(hash-table ,@elems))))
