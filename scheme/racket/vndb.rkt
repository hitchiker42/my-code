#lang racket/base
(require "util.rkt" "io.rkt" "ffi/ffi.rkt")
(require json)
(define vndb-port 19534)
(define vndb-tls-port 19535)
(define vndb-hostname "api.vndb.org")
(define vndb-cache-file (build-path (current-directory) "vndb.cache.json"))
(define vndb-tags-file (build-path (current-directory) "vndb.tags.json"))
(define vndb-traits-file (build-path (current-directory) "vndb.traits.json"))
(define (json->scheme (what (current-input-port)) #:null (jsnull json-null))
  (cond
   ((path? what) (read-json (open-input-file what)))
   ((string? what) (string->jsexpr what jsnull))
   ((bytes? what) (bytes->jsexpr what jsnull))
   ((port? what) (read-json what jsnull))))
(define (read-cache) (json->scheme vndb-cache-file))
(define-once vndb-cache #f)
(define-once vndb-tags #f)
(define-once vndb-traits #f)
(define (load-cache-files)
  (values (false-if-exception (read-cache))
          (false-if-exception (json->scheme vndb-tags-file))
          (false-if-exception (json->scheme vndb-traits-file))))
(require racket/place)
(define cache-future
  (make-future
   (set! vndb-cache (place-channel-get
                     (place out (place-channel-put out (load-cache-files)))))))
