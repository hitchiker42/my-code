#! /usr/bin/guile \
-e main -s
!#
(use-modules (srfi srfi-1))

;;; The Computer Language Benchmarks Game
;;; http://benchmarksgame.alioth.debian.org/
;;; Derived from the Chicken variant by Sven Hartrumpf
;;; contributed by Eli Barzilay

(define (*leaf val) (make-struct (make-vtable "pr") 0 val))
(define (*node item left right)
  (make-struct (make-vtable "prprpr")
               0 item left right))

(define-syntax leaf  (identifier-syntax *leaf))
(define-syntax node  (identifier-syntax *node))
(define-syntax-rule (node? n)
  (eq? 'prprpr (struct-ref (struct-vtable n) vtable-index-layout)))
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

(define (do-trees n)
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
(define (main args)
  (do-trees (string->number (cadr args))))
