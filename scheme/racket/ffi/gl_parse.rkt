#lang racket/base
(require "../util.rkt" "../io.rkt")
(require xml)
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
;(define (parse-typedef str)
;  (#px"typedef *((?:un)?signed)? *(\\w+) *(\\*+)"
