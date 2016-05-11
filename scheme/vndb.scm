#!/usr/bin/guile -s
!#
;;If we don't load the extension lib when compiling we get warnings
(eval-when (compile load eval)
  (add-to-load-path (getcwd))
  (load-extension
   (string-join (list (getcwd) "/libguile-openssl") "") "init_openssl"))
(use-modules (rnrs bytevectors) (ice-9 receive) (ice-9 readline) (json)
             (srfi srfi-1) (ice-9 hash-table) (rnrs io ports) (util))
(define *vndb-cache-dir* "/var/cache/vndb")
(define *vndb-port* 19534)
(define *vndb-tls-port* 19535)
;;The equivalent using gethostbyname
;; (define *vndb-server* (car (hostent:addr-list
;;                             (gethostbyname "api.vndb.org"))))
(define *vndb-hostname* "api.vndb.org")
(define *vndb-server* (sockaddr:addr
                       (addrinfo:addr
                        (car (getaddrinfo *vndb-hostname* #f)))))
(define *client-name* "vndb-scm")
(define *client-version* 0.1)

;;These next 3 are global variables, I know it's bad practice to use them
;;but this is an application just for me and it makes life so much eaiser

(define *vndb-socket* #f)
(define *vndb-tls* #f);;tls session
;;read upto 4k at at timex
(define *output-buf* (make-bytevector 4096))

;;this is explicitly defined since I end up sending it a bunch in testing stuff
(define login-anon-cmd
  (concat "login" (scm->json-string
                   (json (object ("protocol" 1)
                                 ("client" ,*client-name*)
                                 ("clientver" ,*client-version*)))) "\x04"))
;;Utility functions specific to this program/module
(define (alist->json alist)
  (let ((ht (alist->hash-table alist)))
    (scm->json ht)))
(define (alist->json-string alist)
  (let ((ht (alist->hash-table alist)))
    (scm->json-string ht)))

(define (vndb-send msg)
  (if *vndb-tls*
      (tls-send *vndb-tls* msg)
      (send *vndb-socket* msg)))

(define (vndb-recv!)
  "Read from sock into buf, raise an error of type vndb-error
in case of an 'error' response"
  ;;What this needs to do is:
  ;;(while (not (eq? (bytevector-ref *output-buf* (1- nbytes)) ?x04))
  ;;       (set! nbytes (recv ...))
  ;;       (cp *output-buf* to string))
  (let ((nbytes (if *vndb-tls*
                    (tls-recv! *vndb-tls* *output-buf*)
                    (recv! *vndb-socket* *output-buf*))))
    (if (equal? "error" (utf8->string (bytevector-slice *output-buf* 0 5)))
        (throw 'vndb-error (utf8->string (bytevector-slice *output-buf* 0 nbytes)))
        (utf8->string (bytevector-copy (bytevector-slice *output-buf* 0 nbytes))))))
(define (vndb-connect)
  (set! *vndb-socket* (socket PF_INET SOCK_STREAM 0))
  (connect *vndb-socket* AF_INET *vndb-server* *vndb-port*)
  (set-port-encoding! *vndb-socket* "UTF-8"))
(define (vndb-connect-tls)
  (set! *vndb-tls* (tls-connect *vndb-hostname* *vndb-tls-port*))
  (set! *vndb-socket* (tls-get-fd *vndb-tls*)))
(define (vndb-disconnect)
  (close *vndb-socket*)
  (set! *vndb-tls* #f))
(define (vndb-login-anon)
  (vndb-send login-anon-cmd)
  (vndb-recv!))
;;This should use tls
(define (vndb-login-user username password)
  (let ((cmd (scm->json-string
              (json (object ("protocol" 1)
                            ("client" ,*client-name*)
                            ("clientver" ,*client-version*)
                            ("username" ,username)
                            ("password" ,password))))))
    (vndb-send (string-join (list "login" cmd "\x04")))
    (vndb-recv!)))
(define* (vndb-login #:optional username password)
  (if password
      (begin (vndb-connect-tls)
             (vndb-login-user username password))
      (begin (vndb-connect)
             (vndb-login-anon))))

;;The form of the responce of the dbstats command is "dbstats json-obj"
;;which is why we skip the first 8 bytes of response
(define (vndb-dbstats)
  (vndb-send "dbstats\x04")
  (substring (vndb-recv!) 8))

;; (define* (vndb-filter list #:optional (op 'and))
;;   (when (and (not (eq? op 'and) (eq? op 'or)))
;;     (error "seperator must be either 'and or 'or"))
;;   (let acc ((ls list) (filter '()) (sep op))
;;     (if (null? ls)
;;         (concat "(" (string-join filter
;;                                  (concat " " (symbol->string sep) " ")) ")")
;;         (let ((l (pop ls)))
;;           (if (list? l)
;;               (push (acc l '() (if (eq? sep 'and) 'or 'and)) filter)
;;               (push l filter))))))
(define* (key-test #:optional a b #:key x y (z 0) #:rest r)
  (pprint r))
(define* (vndb-get type vndb-filter #:optional (flags '("basic"))
                   #:key page results sort reverse #:rest options)
  ;;Process filter, flags and options into strings
  ;;(when (list? filter) (set! filter (vndb-filter filter)))
  (if (not (null? options))
      (begin
        (pprint options)
        (set! options
          (scm->json-string
           (alist->hash-table
            (filter (lambda (x) (cdr x))
                    (list (cons "page" page) (cons "results" results)
                          (cons "sort" sort) (cons "reverse" reverse)))))))
      (set! options ""))
  (set! flags (string-join flags ","))
  (let ((cmd
         (format #f "get ~a ~a ~a ~a\x04" type flags vndb-filter options)))
    (print cmd)
  (vndb-send cmd)
  (substring (vndb-recv!) 8)))
(define (vndb-set type id fields)
  (let ((cmd (format #f "set ~a ~a ~a\x04" type id
                     (scm->json-string (alist->hash-table fields)))))
    (vndb-send cmd)
    (vndb-recv!)))
;;Some functions to parse returned data
(define (vnlist-get-vns vnlist)
  "Given a response from the \"get vnlist\" command return
a filter to select those vns in a \"get vn\" command"
  (let* ((ht (json-string->scm vnlist))
         (vn-ids (map (lambda (x) (hash-ref x "vn")) (hash-ref ht "items"))))
    (format #f "(id = [~a])" (string-join (map number->string vn-ids) ","))))
(define (get-items str)
  "Return a list of hashtables containing the items in str,
which is a responce from a \"get\" \"foo\" command"
  (hash-ref (json-string->scm str) "items"))
