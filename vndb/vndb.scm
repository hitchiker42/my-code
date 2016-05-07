#!/usr/bin/guile -s
!#
;;receive is basically multiple-value-bind
(use-modules (rnrs bytevectors) (ice-9 receive) (ice-9 readline)
             (ice-9 getopt-long) (json) (rnrs io ports))
(define *vndb-cache-dir* "/var/cache/vndb")
(define *vndb-port* 19534)
(define *vndb-tls-port* 19534)
;;I really should use getaddrinfo, but oh well
(define *vndb-server* (car (hostent:addr-list
                            (gethostbyname "api.vndb.org"))))
(define *client-name* "vndb-scm")
(define *client-version* 0.1)

(define *vndb-socket* #f)
;;read upto 4k at at timex
(define *output-buf* (make-bytevector 4096))

(define-syntax progn (identifier-syntax begin))


(define (vndb-connect)
  (set! *vndb-socket* (socket PF_INET SOCK_STREAM 0))
  (connect *vndb-socket* AF_INET *vndb-server* *vndb-port*)
  (set-port-encoding! *vndb-socket* "UTF-8"))
(define* (vndb-login #:optional username password)
  ;;this is clearly not the best way to do this, but it'll do for now
  (let ((cmd (scm->json-string
              (if password
                  (json (object ("protocol" 1)
                                ("client" ,*client-name*)
                                ("clientver" ,*client-version*)
                                ("username" ,username)
                                ("password" ,password)))
                  (json (object ("protocol" 1)
                                ("client" ,*client-name*)
                                ("clientver" ,*client-version*)))))))
    (send *vndb-socket* (string-join (list "login" cmd "\x04")))
    (let* ((nbytes (recv! *vndb-socket* *output-buf*))
           (output (make-bytevector nbytes)))
      ;;make a macro to simplify this
      (bytevector-copy! *output-buf* 0 output 0 nbytes)
      (utf8->string output))))
(define (vndb-dbstats)
  (send *vndb-socket* "dbstats\x04")
  ;;discard the "dbstats " portion of the result
  (let* ((nbytes (recv! *vndb-socket* *output-buf*))
         (output (make-bytevector nbytes)))
    (bytevector-copy! *output-buf* 0 output 0 nbytes)
    (utf8->string output)))
