#lang racket/base

;; ;;; The Computer Language Benchmarks Game
;; ;;; http://benchmarksgame.alioth.debian.org/
;; ;;; Derived from the Chicken variant by Sven Hartrumpf
;; ;;; contributed by Eli Barzilay

;; (require racket/cmdline racket/require (for-syntax racket/base)
;;          (filtered-in (lambda (name) (regexp-replace #rx"unsafe-" name ""))
;;                       racket/unsafe/ops))

;; (struct *leaf (val))
;; (struct *node *leaf (left right))

;; (define-syntax leaf  (make-rename-transformer #'*leaf))
;; (define-syntax leaf? (make-rename-transformer #'*leaf?))
;; (define-syntax node  (make-rename-transformer #'*node))
;; (define-syntax node? (make-rename-transformer #'*node?))
;; (define-syntax-rule (leaf-val l)   (struct-ref l 0))
;; (define-syntax-rule (node-left n)  (struct-ref n 1))
;; (define-syntax-rule (node-right n) (struct-ref n 2))

;; (define (make item d)
;;   (if (fx= d 0)
;;     (leaf item)
;;     (let ([item2 (fx* item 2)] [d2 (fx- d 1)])
;;       (node item (make (fx- item2 1) d2) (make item2 d2)))))

;; (define (check t)
;;   (let loop ([t t] [acc 0])
;;     (let ([acc (fx+ (leaf-val t) acc)])
;;       (if (node? t)
;;         (loop (node-left t)
;;               (fx- acc (loop (node-right t) 0)))
;;         acc))))

;; (define min-depth 4)

;; (define (main n)
;;   (let ([max-depth (max (+ min-depth 2) n)])
;;     (let ([stretch-depth (+ max-depth 1)])
;;       (printf "stretch tree of depth ~a\t check: ~a\n"
;;               stretch-depth
;;               (check (make 0 stretch-depth))))
;;     (let ([long-lived-tree (make 0 max-depth)])
;;       (for ([d (in-range 4 (+ max-depth 1) 2)])
;;         (let ([iterations (expt 2 (+ (- max-depth d) min-depth))])
;;           (printf "~a\t trees of depth ~a\t check: ~a\n"
;;                   (* 2 iterations)
;;                   d
;;                   (for/fold ([c 0]) ([i (in-range iterations)])
;;                     (fx+ c (fx+ (check (make i d))
;;                                 (check (make (fx- 0 i) d))))))))
;;       (printf "long lived tree of depth ~a\t check: ~a\n"
;;               max-depth
;;               (check long-lived-tree)))))

;; (command-line #:args (n) (main (string->number n)))

(require racket/cmdline (for-syntax racket/base) racket/require
         (only-in racket/list range) srfi/1 srfi/48
         (filtered-in (lambda (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops))

;;; The Computer Language Benchmarks Game
;;; http://benchmarksgame.alioth.debian.org/
;;; Derived from the Chicken variant by Sven Hartrumpf
;;; contributed by Eli Barzilay

(struct *leaf (val))
(struct *node *leaf (left right))

(define-syntax leaf  (make-rename-transformer #'*leaf))
(define-syntax leaf? (make-rename-transformer #'*leaf?))
(define-syntax node  (make-rename-transformer #'*node))
(define-syntax node? (make-rename-transformer #'*node?))
(define-syntax-rule (leaf-val l)   (struct-ref l 0))
(define-syntax-rule (node-left n)  (struct-ref n 1))
(define-syntax-rule (node-right n) (struct-ref n 2))

(define (make item d)
  (if (= d 0)
    (leaf item)
    (let ((item2 (* item 2)) (d2 (- d 1)))
      (node item (make (- item2 1) d2) (make item2 d2)))))

(define (check t)
  (let loop ((t t) (acc 0))
    (let ((acc (+ (leaf-val t) acc)))
      (if (node? t)
        (loop (node-left t)
              (- acc (loop (node-right t) 0)))
        acc))))

(define min-depth 4)

(define (main n)
  (let ((max-depth (max (+ min-depth 2) n)))
    (let ((stretch-depth (+ max-depth 1)))
      (format #t "stretch tree of depth ~a\t check: ~a\n"
              stretch-depth
              (check (make 0 stretch-depth))))
    (let ((long-lived-tree (make 0 max-depth)))
      (let loop ((d 4))
        (when (<= d max-depth)          
          (let ((iterations (expt 2 (+ (- max-depth d) min-depth)))
                (c 0))
            (format #t "~a\t trees of depth ~a\t check: ~a\n"
                    (* 2 iterations)
                    d
                    (do ((i 0 (+ i 1))) ((> i iterations) c)
                      (set! c (+ c (+ (check (make i d))
                                      (check (make (- 0 i) d))))))))
          (loop (+ d 2))))
      (format #t "long lived tree of depth ~a\t check: ~a\n"
              max-depth
              (check long-lived-tree)))))

(command-line #:args (n) (time (main (string->number n))))
