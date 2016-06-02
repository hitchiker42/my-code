#lang racket/base
;;Various functions to be used as the start functions to a dynamic-place form

;;parallelism in racket is weird, basically you need to put the function to start
;;a thread in a seperate file for it to work, and you have to use message passing.
;;However you can send c pointers as messages, and threads are still in a shared memory
;;space. Since pointers can be moved during garbage collection you can't just send a
;;pointer to scheme data, you need to allocate an immutable-box (a pointer to a pointer
;;that won't ever be gc'd or moved, and send that.
(require "util.rkt" "io.rkt" "ffi/ffi.rkt")
(require json)
(require racket/place)
(define vndb-cache-file (build-path (current-directory) "vndb.cache.json"))
(define vndb-tags-file (build-path (current-directory) "vndb.tags.json"))
(define vndb-traits-file (build-path (current-directory) "vndb.traits.json"))
(define (load-vndb-cache-files)
  (values (false-if-exception (read-json (open-input-file vndb-cache-file)))
          (false-if-exception (read-json (open-input-file vndb-tags-file)))
          (false-if-exception (read-json (open-input-file vndb-traits-file)))))

;;this should probably accept either paths or file descriptors in case I change
;;the name/location of one of the files
(define (load-vndb-cache-proc in)
  (let* (((values cache tags traits) (load-vndb-cache-files))
        (cache-ptr (cast cache _scheme _pointer))
        (cache-box (place-channel-get in))
        (tags-ptr (cast tags _scheme _pointer))
        (tags-box (place-channel-get in))
        (traits-ptr (cast tags _scheme _pointer))
        (traits-box (place-channel-get in)))
    (register-finalizer cache (lambda x (println "freeing cache")))
    (ptr-set! cache-box _scheme cache)
    (format #t "loaded cache at address ~x\n"
            (cast (ptr-ref (ptr-ref cache-box _pointer) _pointer) _pointer _uint64))
    (ptr-set! tags-box _scheme tags)
    (format #t "loaded tags at address ~x\n" (ptr-ref tags-box _uint64))
    (ptr-set! traits-box _scheme traits)
    (format #t "loaded traits at address ~x\n" (ptr-ref tags-box _uint64))
    (place-channel-put in cache-ptr)
    (place-channel-put in tags-ptr)
    (place-channel-put in traits-ptr)))
    ;;make sure these don't get gc'd
         
;; (define (run-closure-place-proc in)
;;   (let ((fn-ptr (place-channel-get in)))
;;     (
(provide load-vndb-cache-proc)
