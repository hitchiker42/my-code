#lang racket/base
(require "util.rkt" "ffi/ffi.rkt")
(require racket/port (rename-in racket/contract (-> -->))
         (only-in racket/file file->string file->bytes))


;;;Functions to convert racket ports to rnrs ports, since racket doesn't
;;;have a native input/output port type.
;;We need to use a contract since we're going to do something
;;unsafe, which would cause a segfault on a wrong type
(define/contract (scheme-port-sub-type port)
  (--> port? symbol?)
  (ptr-ref (cast port _scheme _pointer) _scheme 'abs
           scheme-output-port-sub-type-offset))
(define (port-sub-type port)
  ;;The symbol returned by scheme-port-sub-type isn't interned so we need
  ;;to create an interned symbol with the same name as it.
  (let ((subtype
         (symbol->string
          (scheme-port-sub-type port))))
    (case subtype
      (("<string-input-port>" "<string-output-port>") '<string-port>)
      (("<file-input-port>" "<file-output-port>") '<file-port>)
      (("<stream-input-port>" "<stream-output-port>") '<stream-port>)
      (("<user-input-port>" "<user-output-port>") '<user-port>)
      (("<pipe-input-port>" "<pipe-output-port>") '<pipe>)
      (("<null-output-port>") "<null-port>")
      (("<tcp-input-port>" "<tcp-output-port>") '<tcp-port>)
      (("<console-input-port>") '<console-port>))))
;;This would be a lot eaiser if I could just get definations internal to
;;ports.rkt, but I don't know how to do that, and I've spent way too long
;;trying to figure out how

;;I already have the subtype when I call this so I may as well use it
(define (make-position-funs port subtype)
  (if (or (eq? subtype '<string-port>)
          (eq? subtype '<file-port>)
          (eq? subtype '<stream-port>))
      (values (lambda () (file-position port))
              (lambda (pos) (file-position port pos)))
      (values #f #f)))
(define (make-read-fun input)
  (lambda (bv start count)
    (let ((nbytes
           (read-bytes! bv input start (+ start count))))
      (if (eof-object? nbytes) 0 nbytes))))
(define (make-write-fun output)
  (lambda (bv start count)
    (write-bytes-avail bv output start (+ start count))))
(define (make-binary-input/output-port id input output subtype)
  (let-values (((get-pos set-pos) (make-position-funs input subtype))
               ((read) (make-read-fun input))
               ((write) (make-write-fun output))
               ((close) (lambda ()
                          (close-output-port output)
                          (close-input-port input))))
    (make-custom-binary-input/output-port id read write get-pos set-pos close)))
(define (make-binary-input-port id input subtype)
  (let-values (((get-pos set-pos) (make-position-funs input subtype))
               ((read) (make-read-fun input))
               ((close) (lambda () (close-input-port input))))
    (make-custom-binary-input-port id read get-pos set-pos close)))
(define (make-binary-output-port id output subtype)
  (let-values (((get-pos set-pos) (make-position-funs output subtype))
               ((write) (make-write-fun output))
               ((close) (lambda () (close-output-port output))))
    (make-custom-binary-output-port id read get-pos set-pos close)))
(define (ports->r6rs-port input (output #f))
  (--> port? port? port?)
  (let ((subtype (port-sub-type input)))
    (cond
     (output
      (if (not (and (input-port? input)
                    (output-port? output)
                    (eq? subtype (port-sub-type output))))
        (raise-argument-error 'ports->r6rs-port
                              "Invalid port types"
                              "input" input "output" output)
        (make-binary-input/output-port "<input/output port>"
                                       input output subtype)))
     ((input-port? input)
      (make-binary-input-port "<input port>" input subtype))
     ((output-port? input)
      (make-binary-output-port "<output port>" input subtype)))))
(define (read-delim read-fn port msgbuf delim)
  (let ((nbytes (read-fn port msgbuf)))
    (if (eq? (bytevector-u8-ref msgbuf (1- nbytes)))
        (bytevector-copy (bytevector-slice msgbuf 0 nbytes))
        (let* ((buf (bytevector-copy msgbuf))
               (buflen nbytes))
          ;;loop until we end a read on delim
          (while (not (eq? (bytevector-u8-ref buf (1- buflen)) delim))
            (set! nbytes (read-fn port msgbuf))
            (when (< (bytevector-length buf) (+ buflen nbytes))
              (set! buf (bytevector-extend buf (bytevector-length buf))))
            ;;dest offset source count
            (memcpy (bytevector->pointer buf) buflen
                    (bytevector->pointer msgbuf) nbytes)
            (set! buflen (+ nbytes buflen)))
          (bytevector-slice buf 0 buflen)))))
;;no-create: error if it exists, otherwise truncate
;;no-fail: no-error, truncate if exists
;;no-truncate: if the file would be truncated (due to other options) don't
(define (exists-flags->file-options flag)
  (case flag
    (('error) (file-options))
    (('replace 'truncate 'truncate/replace)
     (file-options no-fail))
    (('must-truncate) (file-options no-create))
    (('update) (file-options no-create no-truncate))
    (('can-update #f) (file-options no-fail no-truncate))
    ((#t) (error "unknown exists flag"))))
(define (open-file filename mode (buffering 'block) (exists #f))
  (let ((port
         (case mode
           ((in input r read) (open-input-file filename))
           ((out output w write)
            (open-output-file filename #:exists (or exists 'can-update)))
           ((append)
            (open-output-file filename #:exists 'append))
           ((creat create)
            (open-output-file filename #:exists 'error))
           ((trunc truncate)
            (open-output-file filename #:exists 'truncate))
           ((r/w i/o read/write input/output rdwr)
            (if (eq? exists 'append)
                (let-values (((in out)
                              (open-input-output-file
                               filename #:exists 'append)))
                  (file-stream-buffer-mode in 'none)
                  (file-stream-buffer-mode out 'none)
                  (ports->r6rs-port in out))
                (open-file-input/output-port
                 filename (exists-flags->file-options exists) buffering)))
           ((#t) (error "unknown open mode")))))
         ;;I can't see a way to set the buffer mode of an r6rs port after
         ;;creation, so we can only set the buffer mode of racket ports here
    (unless (and (input-port? port) (output-port? port))
      (file-stream-buffer-mode port buffering))
    port))
;;;ffi stuff (Most i/o ffi stuff is with the rest of the ffi stuff)
;;MZ_EXTERN intptr_t scheme_get_port_fd(Scheme_Object *p);
(define-ffi-binding base-lib
  "scheme_get_port_fd" (_fun _scheme -> _intptr))
;;MZ_EXTERN int scheme_get_port_socket(Scheme_Object *p, intptr_t *_s);
(define-ffi-binding base-lib
  "scheme_get_port_socket" (_fun _scheme (fd : (_ptr o _intptr))
                                 -> _int -> fd))
;;I'm providing the text functions from here to avoid having a bunch
;;of requires in other files
(require "text.rkt")
(require racket/provide)
(provide (except-out (all-defined-out)
                     (matching-identifiers-out
                      #rx"make-.*-funs?" (all-defined-out)))
         (all-from-out racket/port racket/file)
         (all-from-out "text.rkt"))
