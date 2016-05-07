#!/usr/bin/guile -s
!#
;;receive is basically multiple-value-bind
(use-modules (rnrs bytevectors) (ice-9 receive) (ice-9 readline)
             (ice-9 getopt-long) (json) (rnrs io ports) (util))
(define *vndb-cache-dir* "/var/cache/vndb")
(define *vndb-port* 19534)
(define *vndb-tls-port* 19534)
;;The equivalent using gethostbyname
;; (define *vndb-server* (car (hostent:addr-list
;;                             (gethostbyname "api.vndb.org"))))
(define *vndb-server* (sockaddr:addr
                       (addrinfo:addr
                        (car (getaddrinfo "api.vndb.org" #f)))))
(define *client-name* "vndb-scm")
(define *client-version* 0.1)

(define *vndb-socket* #f)
;;read upto 4k at at timex
(define *output-buf* (make-bytevector 4096))

(define* (vndb-recv! #:optional (sock *vndb-socket*) (buf *output-buf*))
  "Read from sock into buf, raise an error of type vndb-error
in case of an 'error' response"
  (let ((nbytes (recv! sock buf)))
    (if (equal? "error" (utf8->string (bytevector-slice buf 0 5)))
        (throw 'vndb-error (utf8->string (bytevector-slice buf 5 (- nbytes 5))))
        nbytes)))
(define (vndb-connect)
  (set! *vndb-socket* (socket PF_INET SOCK_STREAM 0))
  (connect *vndb-socket* AF_INET *vndb-server* *vndb-port*)
  (set-port-encoding! *vndb-socket* "UTF-8"))
(define (vndb-disconnect)
  (close *vndb-socket*))
(define (vndb-login-anon)
  (let ((cmd (scm->json-string
              (json (object ("protocol" 1)
                            ("client" ,*client-name*)
                            ("clientver" ,*client-version*))))))
    (send *vndb-socket* (string-join (list "login" cmd "\x04")))
    (let ((nbytes (vndb-recv! *vndb-socket* *output-buf*)))
      (utf8->string (bytevector-slice *output-buf* 0 nbytes)))))
;;This should use tls
(define (vndb-login-user username password)
  (let ((cmd (scm->json-string
              (json (object ("protocol" 1)
                            ("client" ,*client-name*)
                            ("clientver" ,*client-version*)
                            ("username" ,username)
                            ("password" ,password))))))
    (send *vndb-socket* (string-join (list "login" cmd "\x04")))
    (let ((nbytes (vndb-recv! *vndb-socket* *output-buf*)))
      (utf8->string (bytevector-slice *output-buf* 0 nbytes)))))

;;The form of the responce of the dbstats command is "dbstats json-obj"
;;which is why we skip the first 8 bytes of response 
(define (vndb-dbstats)
  (send *vndb-socket* "dbstats\x04")
  (let ((nbytes (vndb-recv! *vndb-socket* *output-buf*)))
    (utf8->string (bytevector-slice *output-buf* 8 (- nbytes 8)))))
