(define-module (networking)
  #:export '()
  #:use-module '())
;;dotted string -> int is inet-pton
(getaddrinfo "localhost")
;;getaddrinfo returns a list, get elements via:
;;addrinfo:{flags,fam,socktype,protocol,addr,canonname}
(define (make-inet-socket host port)
  (
