;;; This file was automatically generated by SWIG (http://www.swig.org).
;;; Version 3.0.2
;;;
;;; Do not make changes to this file unless you know what you are doing--modify
;;; the SWIG interface file instead.


;;;SWIG wrapper code starts here
(ql:quickload :cffi)
(defpackage socket
  (:use :cl :cffi))
(cl:in-package :socket)
(cffi:load-foreign-library "libc.so.6")

(cl:defmacro defanonenum (cl:&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here


(cl:defconstant _SYS_SOCKET_H 1)

(cffi:defcenum __socket_type
	(:SOCK_STREAM #.1)
	(:SOCK_DGRAM #.2)
	(:SOCK_RAW #.3)
	(:SOCK_RDM #.4)
	(:SOCK_SEQPACKET #.5)
	(:SOCK_DCCP #.6)
	(:SOCK_PACKET #.10)
	(:SOCK_CLOEXEC #.#o2000000)
	(:SOCK_NONBLOCK #.#o0004000))

(defanonenum 
	(MSG_OOB #.#x01)
	(MSG_PEEK #.#x02)
	(MSG_DONTROUTE #.#x04)
	(MSG_CTRUNC #.#x08)
	(MSG_PROXY #.#x10)
	(MSG_TRUNC #.#x20)
	(MSG_DONTWAIT #.#x40)
	(MSG_EOR #.#x80)
	(MSG_WAITALL #.#x100)
	(MSG_FIN #.#x200)
	(MSG_SYN #.#x400)
	(MSG_CONFIRM #.#x800)
	(MSG_RST #.#x1000)
	(MSG_ERRQUEUE #.#x2000)
	(MSG_NOSIGNAL #.#x4000)
	(MSG_MORE #.#x8000)
	(MSG_WAITFORONE #.#x10000)
	(MSG_FASTOPEN #.#x20000000)
	(MSG_CMSG_CLOEXEC #.#x40000000))

(defanonenum 
	(SCM_RIGHTS #.#x01))

(cffi:defcstruct osockaddr
	(sa_family :unsigned-short)
	(sa_data :pointer))

(defanonenum 
	(SHUT_RD #.0)
	SHUT_WR
	SHUT_RDWR)

(cffi:defcfun ("socket" socket) :int
  (__domain :int)
  (__type :int)
  (__protocol :int))

(cffi:defcfun ("socketpair" socketpair) :int
  (__domain :int)
  (__type :int)
  (__protocol :int)
  (__fds :pointer))

(cffi:defcfun ("bind" bind) :int
  (__fd :int)
  (__addr :pointer)
  (__len :unsigned-int))

(cffi:defcfun ("getsockname" getsockname) :int
  (__fd :int)
  (__addr :pointer)
  (__len :pointer))

(cffi:defcfun ("connect" connect) :int
  (__fd :int)
  (__addr :pointer)
  (__len :unsigned-int))

(cffi:defcfun ("getpeername" getpeername) :int
  (__fd :int)
  (__addr :pointer)
  (__len :pointer))

(cffi:defcfun ("send" send) :int
  (__fd :int)
  (__buf :pointer)
  (__n :unsigned-long)
  (__flags :int))

(cffi:defcfun ("recv" recv) :int
  (__fd :int)
  (__buf :pointer)
  (__n :unsigned-long)
  (__flags :int))

(cffi:defcfun ("sendto" sendto) :int
  (__fd :int)
  (__buf :pointer)
  (__n :unsigned-long)
  (__flags :int)
  (__addr :pointer)
  (__addr_len :unsigned-int))

(cffi:defcfun ("recvfrom" recvfrom) :int
  (__fd :int)
  (__buf :pointer)
  (__n :unsigned-long)
  (__flags :int)
  (__addr :pointer)
  (__addr_len :pointer))

(cffi:defcfun ("sendmsg" sendmsg) :int
  (__fd :int)
  (__message :pointer)
  (__flags :int))

(cffi:defcfun ("recvmsg" recvmsg) :int
  (__fd :int)
  (__message :pointer)
  (__flags :int))

(cffi:defcfun ("getsockopt" getsockopt) :int
  (__fd :int)
  (__level :int)
  (__optname :int)
  (__optval :pointer)
  (__optlen :pointer))

(cffi:defcfun ("setsockopt" setsockopt) :int
  (__fd :int)
  (__level :int)
  (__optname :int)
  (__optval :pointer)
  (__optlen :unsigned-int))

(cffi:defcfun ("listen" socket-listen) :int
  (__fd :int)
  (__n :int))

(cffi:defcfun ("accept" accept) :int
  (__fd :int)
  (__addr :pointer)
  (__addr_len :pointer))

(cffi:defcfun ("shutdown" shutdown) :int
  (__fd :int)
  (__how :int))

(cffi:defcfun ("sockatmark" sockatmark) :int
  (__fd :int))

(cffi:defcfun ("isfdtype" isfdtype) :int
  (__fd :int)
  (__fdtype :int))

(cl:defconstant _NETINET_IN_H 1)

(cffi:defcstruct in_addr
	(s_addr :unsigned-int))

(defanonenum 
	(IPPROTO_IP #.0)
	(IPPROTO_ICMP #.1)
	(IPPROTO_IGMP #.2)
	(IPPROTO_IPIP #.4)
	(IPPROTO_TCP #.6)
	(IPPROTO_EGP #.8)
	(IPPROTO_PUP #.12)
	(IPPROTO_UDP #.17)
	(IPPROTO_IDP #.22)
	(IPPROTO_TP #.29)
	(IPPROTO_DCCP #.33)
	(IPPROTO_IPV6 #.41)
	(IPPROTO_RSVP #.46)
	(IPPROTO_GRE #.47)
	(IPPROTO_ESP #.50)
	(IPPROTO_AH #.51)
	(IPPROTO_MTP #.92)
	(IPPROTO_BEETPH #.94)
	(IPPROTO_ENCAP #.98)
	(IPPROTO_PIM #.103)
	(IPPROTO_COMP #.108)
	(IPPROTO_SCTP #.132)
	(IPPROTO_UDPLITE #.136)
	(IPPROTO_RAW #.255)
	IPPROTO_MAX)

(defanonenum 
	(IPPROTO_HOPOPTS #.0)
	(IPPROTO_ROUTING #.43)
	(IPPROTO_FRAGMENT #.44)
	(IPPROTO_ICMPV6 #.58)
	(IPPROTO_NONE #.59)
	(IPPROTO_DSTOPTS #.60)
	(IPPROTO_MH #.135))

(defanonenum 
	(IPPORT_ECHO #.7)
	(IPPORT_DISCARD #.9)
	(IPPORT_SYSTAT #.11)
	(IPPORT_DAYTIME #.13)
	(IPPORT_NETSTAT #.15)
	(IPPORT_FTP #.21)
	(IPPORT_TELNET #.23)
	(IPPORT_SMTP #.25)
	(IPPORT_TIMESERVER #.37)
	(IPPORT_NAMESERVER #.42)
	(IPPORT_WHOIS #.43)
	(IPPORT_MTP #.57)
	(IPPORT_TFTP #.69)
	(IPPORT_RJE #.77)
	(IPPORT_FINGER #.79)
	(IPPORT_TTYLINK #.87)
	(IPPORT_SUPDUP #.95)
	(IPPORT_EXECSERVER #.512)
	(IPPORT_LOGINSERVER #.513)
	(IPPORT_CMDSERVER #.514)
	(IPPORT_EFSSERVER #.520)
	(IPPORT_BIFFUDP #.512)
	(IPPORT_WHOSERVER #.513)
	(IPPORT_ROUTESERVER #.520)
	(IPPORT_RESERVED #.1024)
	(IPPORT_USERRESERVED #.5000))

(cl:defconstant IN_CLASSA_NET #xff000000)

(cl:defconstant IN_CLASSA_NSHIFT 24)

(cl:defconstant IN_CLASSA_MAX 128)

(cl:defconstant IN_CLASSB_NET #xffff0000)

(cl:defconstant IN_CLASSB_NSHIFT 16)

(cl:defconstant IN_CLASSB_MAX 65536)

(cl:defconstant IN_CLASSC_NET #xffffff00)

(cl:defconstant IN_CLASSC_NSHIFT 8)

(cl:defconstant IN_LOOPBACKNET 127)

(cffi:defcstruct in6_addr
	(__in6_u :pointer))

(cffi:defcunion in6_addr___in6_u
	(__u6_addr8 :pointer)
	(__u6_addr16 :pointer)
	(__u6_addr32 :pointer))

(cffi:defcvar ("in6addr_any" in6addr_any)
 in6_addr)

(cffi:defcvar ("in6addr_loopback" in6addr_loopback)
 in6_addr)

(cl:defconstant INET_ADDRSTRLEN 16)

(cl:defconstant INET6_ADDRSTRLEN 46)

(cffi:defcstruct sockaddr_in
	(sin_family :unsigned-short)
	(sin_port :unsigned-short)
	(sin_addr in_addr)
	(sin_zero :pointer))

(cffi:defcstruct sockaddr_in6
	(sin6_family :unsigned-short)
	(sin6_port :unsigned-short)
	(sin6_flowinfo :unsigned-int)
	(sin6_addr in6_addr)
	(sin6_scope_id :unsigned-int))

(cffi:defcstruct ip_mreq
	(imr_multiaddr in_addr)
	(imr_interface in_addr))

(cffi:defcstruct ip_mreq_source
	(imr_multiaddr in_addr)
	(imr_interface in_addr)
	(imr_sourceaddr in_addr))

(cffi:defcstruct ipv6_mreq
	(ipv6mr_multiaddr in6_addr)
	(ipv6mr_interface :unsigned-int))

(cffi:defcstruct group_req
	(gr_interface :unsigned-int)
	(gr_group :pointer))

(cffi:defcstruct group_source_req
	(gsr_interface :unsigned-int)
	(gsr_group :pointer)
	(gsr_source :pointer))

(cffi:defcstruct ip_msfilter
	(imsf_multiaddr in_addr)
	(imsf_interface in_addr)
	(imsf_fmode :unsigned-int)
	(imsf_numsrc :unsigned-int)
	(imsf_slist :pointer))

(cffi:defcstruct group_filter
	(gf_interface :unsigned-int)
	(gf_group :pointer)
	(gf_fmode :unsigned-int)
	(gf_numsrc :unsigned-int)
	(gf_slist :pointer))

(cffi:defcfun ("ntohl" ntohl) :unsigned-int
  (__netlong :unsigned-int))

(cffi:defcfun ("ntohs" ntohs) :unsigned-short
  (__netshort :unsigned-short))

(cffi:defcfun ("htonl" htonl) :unsigned-int
  (__hostlong :unsigned-int))

(cffi:defcfun ("htons" htons) :unsigned-short
  (__hostshort :unsigned-short))

(cffi:defcfun ("bindresvport" bindresvport) :int
  (__sockfd :int)
  (__sock_in :pointer))

(cffi:defcfun ("bindresvport6" bindresvport6) :int
  (__sockfd :int)
  (__sock_in :pointer))

(cl:defconstant _ARPA_INET_H 1)

(cffi:defcfun ("inet_addr" inet_addr) :unsigned-int
  (__cp :string))

(cffi:defcfun ("inet_lnaof" inet_lnaof) :unsigned-int
  (__in in_addr))

(cffi:defcfun ("inet_makeaddr" inet_makeaddr) in_addr
  (__net :unsigned-int)
  (__host :unsigned-int))

(cffi:defcfun ("inet_netof" inet_netof) :unsigned-int
  (__in in_addr))

(cffi:defcfun ("inet_network" inet_network) :unsigned-int
  (__cp :string))

(cffi:defcfun ("inet_ntoa" inet_ntoa) :string
  (__in in_addr))

(cffi:defcfun ("inet_pton" inet_pton) :int
  (__af :int)
  (__cp :string)
  (__buf :pointer))

(cffi:defcfun ("inet_ntop" inet_ntop) :string
  (__af :int)
  (__cp :pointer)
  (__buf :string)
  (__len :unsigned-int))

(cffi:defcfun ("inet_aton" inet_aton) :int
  (__cp :string)
  (__inp :pointer))

(cffi:defcfun ("inet_neta" inet_neta) :string
  (__net :unsigned-int)
  (__buf :string)
  (__len :unsigned-long))

(cffi:defcfun ("inet_net_ntop" inet_net_ntop) :string
  (__af :int)
  (__cp :pointer)
  (__bits :int)
  (__buf :string)
  (__len :unsigned-long))

(cffi:defcfun ("inet_net_pton" inet_net_pton) :int
  (__af :int)
  (__cp :string)
  (__buf :pointer)
  (__len :unsigned-long))

(cffi:defcfun ("inet_nsap_addr" inet_nsap_addr) :unsigned-int
  (__cp :string)
  (__buf :pointer)
  (__len :int))

(cffi:defcfun ("inet_nsap_ntoa" inet_nsap_ntoa) :string
  (__len :int)
  (__cp :pointer)
  (__buf :string))

(cl:defconstant STDIN_FILENO 0)

(cl:defconstant STDOUT_FILENO 1)

(cl:defconstant STDERR_FILENO 2)

(cl:defconstant R_OK 4)

(cl:defconstant W_OK 2)

(cl:defconstant X_OK 1)

(cl:defconstant F_OK 0)

(cffi:defcfun ("access" access) :int
  (__name :string)
  (__type :int))

(cffi:defcfun ("faccessat" faccessat) :int
  (__fd :int)
  (__file :string)
  (__type :int)
  (__flag :int))

(cl:defconstant SEEK_SET 0)

(cl:defconstant SEEK_CUR 1)

(cl:defconstant SEEK_END 2)

(cl:defconstant L_SET 0)

(cl:defconstant L_INCR 1)

(cl:defconstant L_XTND 2)

(cffi:defcfun ("lseek" lseek) :long
  (__fd :int)
  (__offset :long)
  (__whence :int))

(cffi:defcfun ("close" c-close) :int
  (__fd :int))

(cffi:defcfun ("read" c-read) :int
  (__fd :int)
  (__buf :pointer)
  (__nbytes :unsigned-long))

(cffi:defcfun ("write" c-write) :int
  (__fd :int)
  (__buf :pointer)
  (__n :unsigned-long))

(cffi:defcfun ("pread" pread) :int
  (__fd :int)
  (__buf :pointer)
  (__nbytes :unsigned-long)
  (__offset :long))

(cffi:defcfun ("pwrite" pwrite) :int
  (__fd :int)
  (__buf :pointer)
  (__n :unsigned-long)
  (__offset :long))

(cffi:defcfun ("pipe" pipe) :int
  (__pipedes :pointer))

(cffi:defcfun ("alarm" alarm) :unsigned-int
  (__seconds :unsigned-int))

(cffi:defcfun ("sleep" c-sleep) :unsigned-int
  (__seconds :unsigned-int))

(cffi:defcfun ("ualarm" ualarm) :unsigned-int
  (__value :unsigned-int)
  (__interval :unsigned-int))

(cffi:defcfun ("usleep" usleep) :int
  (__useconds :unsigned-int))

(cffi:defcfun ("pause" pause) :int)

(cffi:defcfun ("chown" chown) :int
  (__file :string)
  (__owner :unsigned-int)
  (__group :unsigned-int))

(cffi:defcfun ("fchown" fchown) :int
  (__fd :int)
  (__owner :unsigned-int)
  (__group :unsigned-int))

(cffi:defcfun ("lchown" lchown) :int
  (__file :string)
  (__owner :unsigned-int)
  (__group :unsigned-int))

(cffi:defcfun ("fchownat" fchownat) :int
  (__fd :int)
  (__file :string)
  (__owner :unsigned-int)
  (__group :unsigned-int)
  (__flag :int))

(cffi:defcfun ("chdir" chdir) :int
  (__path :string))

(cffi:defcfun ("fchdir" fchdir) :int
  (__fd :int))

(cffi:defcfun ("getcwd" getcwd) :string
  (__buf :string)
  (__size :unsigned-long))

(cffi:defcfun ("getwd" getwd) :string
  (__buf :string))

(cffi:defcfun ("dup" dup) :int
  (__fd :int))

(cffi:defcfun ("dup2" dup2) :int
  (__fd :int)
  (__fd2 :int))

(cffi:defcvar ("__environ" __environ)
 :pointer)

(cffi:defcfun ("execve" execve) :int
  (__path :string)
  (__argv :pointer)
  (__envp :pointer))

(cffi:defcfun ("fexecve" fexecve) :int
  (__fd :int)
  (__argv :pointer)
  (__envp :pointer))

(cffi:defcfun ("execv" execv) :int
  (__path :string)
  (__argv :pointer))

(cffi:defcfun ("execle" execle) :int
  (__path :string)
  (__arg :string)
  &rest)

(cffi:defcfun ("execl" execl) :int
  (__path :string)
  (__arg :string)
  &rest)

(cffi:defcfun ("execvp" execvp) :int
  (__file :string)
  (__argv :pointer))

(cffi:defcfun ("execlp" execlp) :int
  (__file :string)
  (__arg :string)
  &rest)

(cffi:defcfun ("nice" nice) :int
  (__inc :int))

(cffi:defcfun ("_exit" _exit) :void
  (__status :int))

(cffi:defcfun ("pathconf" pathconf) :long
  (__path :string)
  (__name :int))

(cffi:defcfun ("fpathconf" fpathconf) :long
  (__fd :int)
  (__name :int))

(cffi:defcfun ("sysconf" sysconf) :long
  (__name :int))

(cffi:defcfun ("confstr" confstr) :unsigned-long
  (__name :int)
  (__buf :string)
  (__len :unsigned-long))

(cffi:defcfun ("getpid" getpid) :int)

(cffi:defcfun ("getppid" getppid) :int)

(cffi:defcfun ("getpgrp" getpgrp) :int)

(cffi:defcfun ("__getpgid" __getpgid) :int
  (__pid :int))

(cffi:defcfun ("getpgid" getpgid) :int
  (__pid :int))

(cffi:defcfun ("setpgid" setpgid) :int
  (__pid :int)
  (__pgid :int))

(cffi:defcfun ("setpgrp" setpgrp) :int)

(cffi:defcfun ("setsid" setsid) :int)

(cffi:defcfun ("getsid" getsid) :int
  (__pid :int))

(cffi:defcfun ("getuid" getuid) :unsigned-int)

(cffi:defcfun ("geteuid" geteuid) :unsigned-int)

(cffi:defcfun ("getgid" getgid) :unsigned-int)

(cffi:defcfun ("getegid" getegid) :unsigned-int)

(cffi:defcfun ("getgroups" getgroups) :int
  (__size :int)
  (__list :pointer))

(cffi:defcfun ("setuid" setuid) :int
  (__uid :unsigned-int))

(cffi:defcfun ("setreuid" setreuid) :int
  (__ruid :unsigned-int)
  (__euid :unsigned-int))

(cffi:defcfun ("seteuid" seteuid) :int
  (__uid :unsigned-int))

(cffi:defcfun ("setgid" setgid) :int
  (__gid :unsigned-int))

(cffi:defcfun ("setregid" setregid) :int
  (__rgid :unsigned-int)
  (__egid :unsigned-int))

(cffi:defcfun ("setegid" setegid) :int
  (__gid :unsigned-int))

(cffi:defcfun ("fork" fork) :int)

(cffi:defcfun ("vfork" vfork) :int)

(cffi:defcfun ("ttyname" ttyname) :string
  (__fd :int))

(cffi:defcfun ("ttyname_r" ttyname_r) :int
  (__fd :int)
  (__buf :string)
  (__buflen :unsigned-long))

(cffi:defcfun ("isatty" isatty) :int
  (__fd :int))

(cffi:defcfun ("ttyslot" ttyslot) :int)

(cffi:defcfun ("link" link) :int
  (__from :string)
  (__to :string))

(cffi:defcfun ("linkat" linkat) :int
  (__fromfd :int)
  (__from :string)
  (__tofd :int)
  (__to :string)
  (__flags :int))

(cffi:defcfun ("symlink" symlink) :int
  (__from :string)
  (__to :string))

(cffi:defcfun ("readlink" readlink) :int
  (__path :string)
  (__buf :string)
  (__len :unsigned-long))

(cffi:defcfun ("symlinkat" symlinkat) :int
  (__from :string)
  (__tofd :int)
  (__to :string))

(cffi:defcfun ("readlinkat" readlinkat) :int
  (__fd :int)
  (__path :string)
  (__buf :string)
  (__len :unsigned-long))

(cffi:defcfun ("unlink" unlink) :int
  (__name :string))

(cffi:defcfun ("unlinkat" unlinkat) :int
  (__fd :int)
  (__name :string)
  (__flag :int))

(cffi:defcfun ("rmdir" rmdir) :int
  (__path :string))

(cffi:defcfun ("tcgetpgrp" tcgetpgrp) :int
  (__fd :int))

(cffi:defcfun ("tcsetpgrp" tcsetpgrp) :int
  (__fd :int)
  (__pgrp_id :int))

(cffi:defcfun ("getlogin" getlogin) :string)

(cffi:defcfun ("getlogin_r" getlogin_r) :int
  (__name :string)
  (__name_len :unsigned-long))

(cffi:defcfun ("setlogin" setlogin) :int
  (__name :string))

(cffi:defcfun ("gethostname" gethostname) :int
  (__name :string)
  (__len :unsigned-long))

(cffi:defcfun ("sethostname" sethostname) :int
  (__name :string)
  (__len :unsigned-long))

(cffi:defcfun ("sethostid" sethostid) :int
  (__id :long))

(cffi:defcfun ("getdomainname" getdomainname) :int
  (__name :string)
  (__len :unsigned-long))

(cffi:defcfun ("setdomainname" setdomainname) :int
  (__name :string)
  (__len :unsigned-long))

(cffi:defcfun ("vhangup" vhangup) :int)

(cffi:defcfun ("revoke" revoke) :int
  (__file :string))

(cffi:defcfun ("profil" profil) :int
  (__sample_buffer :pointer)
  (__size :unsigned-long)
  (__offset :unsigned-long)
  (__scale :unsigned-int))

(cffi:defcfun ("acct" acct) :int
  (__name :string))

(cffi:defcfun ("getusershell" getusershell) :string)

(cffi:defcfun ("endusershell" endusershell) :void)

(cffi:defcfun ("setusershell" setusershell) :void)

(cffi:defcfun ("daemon" daemon) :int
  (__nochdir :int)
  (__noclose :int))

(cffi:defcfun ("chroot" chroot) :int
  (__path :string))

(cffi:defcfun ("getpass" getpass) :string
  (__prompt :string))

(cffi:defcfun ("fsync" fsync) :int
  (__fd :int))

(cffi:defcfun ("gethostid" gethostid) :long)

(cffi:defcfun ("sync" sync) :void)

(cffi:defcfun ("getpagesize" getpagesize) :int)

(cffi:defcfun ("getdtablesize" getdtablesize) :int)

(cffi:defcfun ("truncate" c-truncate) :int
  (__file :string)
  (__length :long))

(cffi:defcfun ("ftruncate" c-ftruncate) :int
  (__fd :int)
  (__length :long))

(cffi:defcfun ("brk" brk) :int
  (__addr :pointer))

(cffi:defcfun ("sbrk" sbrk) :pointer
  (__delta :int))

(cffi:defcfun ("syscall" syscall) :long
  (__sysno :long)
  &rest)

(cl:defconstant F_ULOCK 0)

(cl:defconstant F_LOCK 1)

(cl:defconstant F_TLOCK 2)

(cl:defconstant F_TEST 3)

(cffi:defcfun ("lockf" lockf) :int
  (__fd :int)
  (__cmd :int)
  (__len :long))

(cffi:defcfun ("fdatasync" fdatasync) :int
  (__fildes :int))

(cl:defconstant _STDIO_H 1)

(cl:defconstant __FILE_defined 1)

(cl:defconstant ____FILE_defined 1)

(cffi:defcenum __codecvt_result
	:__codecvt_ok
	:__codecvt_partial
	:__codecvt_error
	:__codecvt_noconv)

(cl:defconstant _IOFBF 0)

(cl:defconstant _IOLBF 1)

(cl:defconstant _IONBF 2)

(cl:defconstant BUFSIZ 8192)

(cl:defconstant P_tmpdir "/tmp")

(cffi:defcvar ("stdin" stdin)
 :pointer)

(cffi:defcvar ("stdout" stdout)
 :pointer)

(cffi:defcvar ("stderr" stderr)
 :pointer)

(cffi:defcfun ("remove" c-remove) :int
  (__filename :string))

(cffi:defcfun ("rename" c-rename) :int
  (__old :string)
  (__new :string))

(cffi:defcfun ("renameat" renameat) :int
  (__oldfd :int)
  (__old :string)
  (__newfd :int)
  (__new :string))

(cffi:defcfun ("tmpfile" tmpfile) :pointer)

(cffi:defcfun ("tmpnam" tmpnam) :string
  (__s :string))

(cffi:defcfun ("tmpnam_r" tmpnam_r) :string
  (__s :string))

(cffi:defcfun ("tempnam" tempnam) :string
  (__dir :string)
  (__pfx :string))

(cffi:defcfun ("fclose" fclose) :int
  (__stream :pointer))

(cffi:defcfun ("fflush" fflush) :int
  (__stream :pointer))

(cffi:defcfun ("fflush_unlocked" fflush_unlocked) :int
  (__stream :pointer))

(cffi:defcfun ("fopen" fopen) :pointer
  (__filename :string)
  (__modes :string))

(cffi:defcfun ("freopen" freopen) :pointer
  (__filename :string)
  (__modes :string)
  (__stream :pointer))

(cffi:defcfun ("fdopen" fdopen) :pointer
  (__fd :int)
  (__modes :string))

(cffi:defcfun ("fmemopen" fmemopen) :pointer
  (__s :pointer)
  (__len :unsigned-long)
  (__modes :string))

(cffi:defcfun ("open_memstream" open_memstream) :pointer
  (__bufloc :pointer)
  (__sizeloc :pointer))

(cffi:defcfun ("setbuf" setbuf) :void
  (__stream :pointer)
  (__buf :string))

(cffi:defcfun ("setvbuf" setvbuf) :int
  (__stream :pointer)
  (__buf :string)
  (__modes :int)
  (__n :unsigned-long))

(cffi:defcfun ("setbuffer" setbuffer) :void
  (__stream :pointer)
  (__buf :string)
  (__size :unsigned-long))

(cffi:defcfun ("setlinebuf" setlinebuf) :void
  (__stream :pointer))

(cffi:defcfun ("fprintf" fprintf) :int
  (__stream :pointer)
  (__format :string)
  &rest)

(cffi:defcfun ("printf" printf) :int
  (__format :string)
  &rest)

(cffi:defcfun ("sprintf" sprintf) :int
  (__s :string)
  (__format :string)
  &rest)

(cffi:defcfun ("vfprintf" vfprintf) :int
  (__s :pointer)
  (__format :string)
  (__arg :pointer))

(cffi:defcfun ("vprintf" vprintf) :int
  (__format :string)
  (__arg :pointer))

(cffi:defcfun ("vsprintf" vsprintf) :int
  (__s :string)
  (__format :string)
  (__arg :pointer))

(cffi:defcfun ("snprintf" snprintf) :int
  (__s :string)
  (__maxlen :unsigned-long)
  (__format :string)
  &rest)

(cffi:defcfun ("vsnprintf" vsnprintf) :int
  (__s :string)
  (__maxlen :unsigned-long)
  (__format :string)
  (__arg :pointer))

(cffi:defcfun ("vdprintf" vdprintf) :int
  (__fd :int)
  (__fmt :string)
  (__arg :pointer))

(cffi:defcfun ("dprintf" dprintf) :int
  (__fd :int)
  (__fmt :string)
  &rest)

(cffi:defcfun ("fscanf" fscanf) :int
  (__stream :pointer)
  (__format :string)
  &rest)

(cffi:defcfun ("scanf" scanf) :int
  (__format :string)
  &rest)

(cffi:defcfun ("sscanf" sscanf) :int
  (__s :string)
  (__format :string)
  &rest)

(cffi:defcfun ("__isoc99_fscanf" __isoc99_fscanf) :int
  (__stream :pointer)
  (__format :string)
  &rest)

(cffi:defcfun ("__isoc99_scanf" __isoc99_scanf) :int
  (__format :string)
  &rest)

(cffi:defcfun ("__isoc99_sscanf" __isoc99_sscanf) :int
  (__s :string)
  (__format :string)
  &rest)

(cffi:defcfun ("vfscanf" vfscanf) :int
  (__s :pointer)
  (__format :string)
  (__arg :pointer))

(cffi:defcfun ("vscanf" vscanf) :int
  (__format :string)
  (__arg :pointer))

(cffi:defcfun ("vsscanf" vsscanf) :int
  (__s :string)
  (__format :string)
  (__arg :pointer))

(cffi:defcfun ("__isoc99_vfscanf" __isoc99_vfscanf) :int
  (__s :pointer)
  (__format :string)
  (__arg :pointer))

(cffi:defcfun ("__isoc99_vscanf" __isoc99_vscanf) :int
  (__format :string)
  (__arg :pointer))

(cffi:defcfun ("__isoc99_vsscanf" __isoc99_vsscanf) :int
  (__s :string)
  (__format :string)
  (__arg :pointer))

(cffi:defcfun ("fgetc" fgetc) :int
  (__stream :pointer))

(cffi:defcfun ("getc" getc) :int
  (__stream :pointer))

(cffi:defcfun ("getchar" getchar) :int)

(cffi:defcfun ("getc_unlocked" getc_unlocked) :int
  (__stream :pointer))

(cffi:defcfun ("getchar_unlocked" getchar_unlocked) :int)

(cffi:defcfun ("fgetc_unlocked" fgetc_unlocked) :int
  (__stream :pointer))

(cffi:defcfun ("fputc" fputc) :int
  (__c :int)
  (__stream :pointer))

(cffi:defcfun ("putc" putc) :int
  (__c :int)
  (__stream :pointer))

(cffi:defcfun ("putchar" putchar) :int
  (__c :int))

(cffi:defcfun ("fputc_unlocked" fputc_unlocked) :int
  (__c :int)
  (__stream :pointer))

(cffi:defcfun ("putc_unlocked" putc_unlocked) :int
  (__c :int)
  (__stream :pointer))

(cffi:defcfun ("putchar_unlocked" putchar_unlocked) :int
  (__c :int))

(cffi:defcfun ("getw" getw) :int
  (__stream :pointer))

(cffi:defcfun ("putw" putw) :int
  (__w :int)
  (__stream :pointer))

(cffi:defcfun ("fgets" fgets) :string
  (__s :string)
  (__n :int)
  (__stream :pointer))

(cffi:defcfun ("gets" gets) :string
  (__s :string))

(cffi:defcfun ("__getdelim" __getdelim) :int
  (__lineptr :pointer)
  (__n :pointer)
  (__delimiter :int)
  (__stream :pointer))

(cffi:defcfun ("getdelim" getdelim) :int
  (__lineptr :pointer)
  (__n :pointer)
  (__delimiter :int)
  (__stream :pointer))

(cffi:defcfun ("getline" getline) :int
  (__lineptr :pointer)
  (__n :pointer)
  (__stream :pointer))

(cffi:defcfun ("fputs" fputs) :int
  (__s :string)
  (__stream :pointer))

(cffi:defcfun ("puts" puts) :int
  (__s :string))

(cffi:defcfun ("ungetc" ungetc) :int
  (__c :int)
  (__stream :pointer))

(cffi:defcfun ("fread" fread) :unsigned-long
  (__ptr :pointer)
  (__size :unsigned-long)
  (__n :unsigned-long)
  (__stream :pointer))

(cffi:defcfun ("fwrite" fwrite) :unsigned-long
  (__ptr :pointer)
  (__size :unsigned-long)
  (__n :unsigned-long)
  (__s :pointer))

(cffi:defcfun ("fread_unlocked" fread_unlocked) :unsigned-long
  (__ptr :pointer)
  (__size :unsigned-long)
  (__n :unsigned-long)
  (__stream :pointer))

(cffi:defcfun ("fwrite_unlocked" fwrite_unlocked) :unsigned-long
  (__ptr :pointer)
  (__size :unsigned-long)
  (__n :unsigned-long)
  (__stream :pointer))

(cffi:defcfun ("fseek" fseek) :int
  (__stream :pointer)
  (__off :long)
  (__whence :int))

(cffi:defcfun ("ftell" ftell) :long
  (__stream :pointer))

(cffi:defcfun ("rewind" rewind) :void
  (__stream :pointer))

(cffi:defcfun ("fseeko" fseeko) :int
  (__stream :pointer)
  (__off :long)
  (__whence :int))

(cffi:defcfun ("ftello" ftello) :long
  (__stream :pointer))

(cffi:defcfun ("fgetpos" fgetpos) :int
  (__stream :pointer)
  (__pos :pointer))

(cffi:defcfun ("fsetpos" fsetpos) :int
  (__stream :pointer)
  (__pos :pointer))

(cffi:defcfun ("clearerr" clearerr) :void
  (__stream :pointer))

(cffi:defcfun ("feof" feof) :int
  (__stream :pointer))

(cffi:defcfun ("ferror" ferror) :int
  (__stream :pointer))

(cffi:defcfun ("clearerr_unlocked" clearerr_unlocked) :void
  (__stream :pointer))

(cffi:defcfun ("feof_unlocked" feof_unlocked) :int
  (__stream :pointer))

(cffi:defcfun ("ferror_unlocked" ferror_unlocked) :int
  (__stream :pointer))

(cffi:defcfun ("perror" perror) :void
  (__s :string))

(cffi:defcfun ("fileno" fileno) :int
  (__stream :pointer))

(cffi:defcfun ("fileno_unlocked" fileno_unlocked) :int
  (__stream :pointer))

(cffi:defcfun ("popen" popen) :pointer
  (__command :string)
  (__modes :string))

(cffi:defcfun ("pclose" pclose) :int
  (__stream :pointer))

(cffi:defcfun ("ctermid" ctermid) :string
  (__s :string))

(cffi:defcfun ("flockfile" flockfile) :voidk
  (__stream :pointer))

(cffi:defcfun ("ftrylockfile" ftrylockfile) :int
  (__stream :pointer))

(cffi:defcfun ("funlockfile" funlockfile) :void
  (__stream :pointer))


