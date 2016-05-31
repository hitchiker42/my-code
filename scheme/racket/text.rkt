#lang racket/base
(require "util.rkt")
(require "ffi/ffi.rkt")
;;regexps aren't really io, but it seems silly to make another file

;;The point of this struct & associated functions is to allow
;;looping through all the matches of a regexp in a string, basically:
;;(while (re-matcher-next-match matcher) (do something with match))
(struct re-matcher
  (pattern string (last-match #:mutable #:auto)))
(define (maybe-compile-regexp pattern)
  (if (regexp? pattern) pattern
      (pregexp pattern)))
(define (re-matcher-next-match matcher)
  (let ((positions
         (regexp-match-positions (re-matcher-pattern matcher)
                                 (re-matcher-string matcher)
                                 (aif (re-matcher-last-match matcher)
                                      (cdar it) 0))))
    (if positions
        (begin
          (set-re-matcher-last-match! matcher positions) #t)
        (begin
          ;;We set this to null instead of #f so we can tell between a
          ;;matcher that hasn't started and one that's finished
          (set-re-matcher-last-match! matcher null) #f))))
;;Try to match pat at the current location of the matcher, it is
;;assumed that the matcher has already made at least one match, if not
;;an error will be raised. Match data is set only when a match is made
(define (re-matcher-try-match matcher pat
                              (start (cdar (re-matcher-last-match matcher)))
                              (end (string-length (re-matcher-string matcher))))
                                      
  (let* ((re (maybe-compile-regexp pat))
         (maybe-match (regexp-match-positions re (re-matcher-string matcher)
                                              start end)))
    (if maybe-match
        (begin0 #t
          (set-re-matcher-last-match! matcher maybe-match))
        #f)))
;;The same as the normal constructor except if pat is a string it gets
;;compiled into a re
(define (make-re-matcher pat input)
  (let* ((re (maybe-compile-regexp pat)))
    (re-matcher re input)))
 (define (re-matcher-match-string matcher num)
   (if (not (re-matcher-last-match matcher)) #f
       (match (nth num (re-matcher-last-match matcher))
         ((cons start end)
                ;;TODO: write a substring function that doesn't copy
                (string-slice (re-matcher-string matcher) start end)))))
(define (re-matcher-match-start matcher num)
  (car-safe (nth num (re-matcher-last-match matcher))))
(define (re-matcher-match-end matcher num)
  (cdr-safe (nth num (re-matcher-last-match matcher))))
(define (re-matcher-match-positions matcher num)
  (nth num (re-matcher-last-match matcher)))
;;This is effectively the same as (re-matcher-try-match matcher #rx"$")
;;but should be faster since it doesn't have to do a regex-match
(define (re-matcher-at-end? matcher)
  (or (null? (re-matcher-last-match matcher))
      (eq? (cdr-safe (last (re-matcher-last-match matcher)))
           (string-length (re-matcher-string matcher)))))
(define (un-camel-case id (sep "-"))
  (let ((matcher (make-re-matcher #rx"[a-z0-9]([A-Z])" id))
        (start 0) (end 0)
        (result null))
    (while (re-matcher-next-match matcher)
      (set! start (re-matcher-match-start matcher 1))
      ;;adds the characters from the end of the last match to the start of
      ;;this match to the result
      (set! result (list* sep
                          (string-slice (re-matcher-string matcher) end start)
                          result))
      ;;If there are multiple uppercase characters in a row keep them as
      ;;uppercase
      (if (re-matcher-try-match matcher #rx"^[A-Z][A-Z]+" start)
          (begin
            (push! (re-matcher-match-string matcher 0) result)
            (unless (re-matcher-at-end? matcher)
              (push! sep result)))
          (push!
           (string-downcase
            (re-matcher-match-string matcher 1)) result))
      (set! end (re-matcher-match-end matcher 0)))
    (unless (eq? end (string-length id))
      (push! (string-slice id end (string-length id)) result))
    (apply concat (reverse result))))
(provide (all-defined-out))
