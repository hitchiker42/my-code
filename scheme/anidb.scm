(define-module (anidb udp))
;;Guile scheme client for the anidb UDP api
;;API defination is located at http://wiki.anidb.net/w/UDP_API_Definition

;;receive is basically multiple-value-bind
(use-modules (rnrs bytevectors) (ice-9 receive) (ice-9 readline)
             (ice-9 getopt-long) (sxml simple) (rnrs io ports)
             (ice-9 regex) (srfi srfi-1) (ice-9 format))

(load-extension "/home/tucker/anime_db/libguile-MD4" "init_MD4")
;;;;Global variables
;;I should read these from next 2 from a config file
(define *anidb-config-file*
  (string-append (getenv "HOME") "/.anidb_scmrc"))
(define *anidb-cache-dir* "/var/cache/anidb")
(define *anidb-port* 9000)
(define *anidb-server* (car (hostent:addr-list
                             (gethostbyname "api.anidb.net"))))
;;the maximum size of a packet returned from the anidb server is 1400 bytes
(define *output-buffer* (make-bytevector 1400))
(define *anidb-socket* #f)
(define *client-name* "anidbscm")
(define *client-version* 1)
(define *api-version* 3)
(define *session-key* #f)
(define *default-anime-mask* #xb2f0e0fc000000)
(define *anime-info-fields*
  #("aid" "dateflags" "year" "type" "related-aid-list"
    "related-aid-type" "category-list" "category-weight-list"

    "romaji-name" "kanji-name" "english-name" "other-name"
    "short-name-list" "synonym-list" "unused" "unused"

    "num-episodes" "max-episode-num" "num-special-episodes"
    "air-date" "end-date" "url" "picname" "cadegory-id-list"

    "rating" "vote-count" "temp-rating" "temp-vote-count"
    "avg-review" "review-count" "award-list" "is-18+"

    "anime-planet-id" "ann-id" "allcinema-id"
    "AnimeNfo-id" "unused" "unused" "unused" "date-record-updated"

    "character-id-list" "creator-id-list" "main-creator-id-list"
    "main-creator-name-list" "unused" "unused" "unused" "unused"

    "specials-count" "credits-count" "other-count"
    "trailer-count" "parody-count" "unused" "unused" "unused"))
;;;;Macros
(eval-when (expand load eval)
           (define-inlinable (my-eval expr)
             (eval expr (interaction-environment))))
(define-macro (as-list x)
  `(if (list? ,x) ,x (list ,x)))
;; (define-macro (equal-any? val args)
;;   ;;I can't get this to work any other way, so yeah
;;   `(or ,@(map (lambda (x) `(equal? ,val ,x))
;;               (as-list (my-eval args)))))
;;This should be a macro but its too hard to make it work
(define (equal-any? val args)
  (if (not (pair? args))
      (equal? val args)
      (let ((retval #f))
        (while (pair? args)
          (if (equal? val (pop args))
              (set! retval #t)
              break))
        retval)))
;;this works almast, it's horribly ugly and it doesn't work
;; (define-syntax equal-any?
;;    (lambda (x)
;;      (syntax-case x ()
;;        ((_ val args)
;;         (let ((args-datum (syntax->datum #'args))
;;               (val-datum (syntax->datum #`val)))
;;           (if (identifier? val-datum)
;;               (set! val-datum (datum->syntax x val-datum)))
;;           (if (list? args-datum)
;;               (let ((or-body (map (lambda (x) `(equal? ,val-datum ,x))
;;                                   (my-eval args-datum))))
;;                 (apply eval (list
;;                              (list 'syntax `(or ,@or-body))
;;                              (interaction-environment))))
;;               #'(equal? val args)))))))
(define-syntax progn (identifier-syntax begin))
(define-syntax prog1
  (lambda (x)
    (syntax-case x ()
      ((_ first rest ...)
       #'(let ((ret first))
           rest ...
           first)))))
(define-syntax pop
  (lambda (x)
    (syntax-case x ()
      ((_ ls)
       #'(car (let ((ret ls))
                (set! ls (cdr ls))
                ret))))))
(define-syntax push
  (lambda (x)
    (syntax-case x ()
      ((_ elt place)
       #'(set! place (cons elt place))))))
(define-syntax decf
  (lambda (x)
    (syntax-case x ()
      ((_ y)
       #'(set! y (1- y))))))
(define-syntax incf
  (lambda (x)
    (syntax-case x ()
      ((_ y)
       #'(set! y (1+ y))))))
;;I actually need this and it works perfectly like this and I have
;;no clue how to make it work in the weird define-syntax form
(define-macro (case-equal val . exprs)
  `(cond ,@(map (lambda (x)
                  (cons (list 'equal? val (car x)) (cdr x))) exprs)))
;;Concatenate strings at macroexpansion time, allows writing long string 
;;constants without looking ugly or suffering a performance penalty 
(define-macro (concat . strings)
  (string-join strings ""))
;;This implicitly introduces a few bindings, which is normally bad form, but in
;;this case I think it's fine, it's not as if this is a macro with wide applications
(define-syntax define-anidb-command
  (lambda (x)
    (syntax-case x ()
      ((_ define-clause command expected action ...)
       (with-syntax ((code (datum->syntax x 'code))
                     (msg (datum->syntax x 'msg))
                     (data (datum->syntax x 'data))
                     (cmd (datum->syntax x 'cmd)))
         #'(define* define-clause
             (let ((cmd command))
                   (send *anidb-socket* cmd)
               (receive (code msg data) (anidb-get-response cmd)
                 (if (not (equal-any? code expected))
                     (anidb-handle-error code msg data)
                     (begin action ...))))))))))
;;;;Utility functions
(define (string-split str delim)
  "Split string into substrings delimited by delim.
Returns a list of substrings"
  (let acc ((index 0)
            (output '()))
    (let ((temp (string-index str delim (1+ index))))
      (if temp
          (acc temp (cons (substring str index temp) output))
          (reverse! (cons (substring str index) output))))))
(define (string-strip str)
  "returns a copy of str with leading and trailing whitespace removed"
  (let ((start (string-skip str char-set:whitespace))
        (end (string-skip-right str char-set:whitespace)))
    (substring str start (1+ end))))
(define* (print obj #:optional (port (current-output-port)))
  "write obj to port, followed by a newline"
  (write obj port)
  (newline port))
(define* (pprint obj #:optional (port (current-output-port)))
  "display obj on port, followed by a newline"
  (display obj port)
  (newline port))
(define* (string-collect s char-pred
                         #:optional (start 0) (end (string-length s)))
  "search string s for char-pred using string-index and if found
return a substring begining at start and ending at the smaller of
end and the index of char-pred, if char-pred is not found return #f"
  (let ((index (string-index s char-pred start end)))
    (if index
        (substring s start (min index end))
        #f)))
(define (integer->bitvector n)
  "Convert an integer to a bitvector such that the most significant
bit in the integer is the first bit in the bitvector"
  (let* ((num-bits (integer-length n))
         (bit (1- num-bits))
         (vec (make-bitvector num-bits)))
    (while (> bit 0)
      (bitvector-set! vec (- num-bits 1 bit) (logbit? bit n))
      (decf bit))
    vec))
(define (logbit-reverse? index n)
  "test if bit number index is set in integer n, where index 0 corresponds
to the most significant bit"
  (let ((num-bits (integer-length n)))
    (logbit? (- num-bits 1 index) n)))
;;add unwind-protect to close port
(define (MD4-file filename)
  "Compute the MD4 sum of filename and return as a 16 byte integer"
  (let* ((io-port (open-file-input-port filename))
         (file-size (stat:size (stat filename)))
         (file-contents (get-bytevector-n io-port file-size))
         (MD4-sum (MD4 file-contents))
         (retval (bytevector-uint-ref MD4-sum 0 (endianness big) 16)))
    (close-port io-port)
    retval))
(define-inlinable (MD4-bytevector bv)
  (bytevector-uint-ref
   (MD4 bv) 0 (endianness big) 16))
(define eD2k-chunk-size (* 9500 1024))
(define (eD2k-file filename)
  "Compute the eD2k sum of filename and return as a 16 byte integer"
  (let* ((port (open-file-input-port filename))
         (size (stat:size (stat filename)))
         (result 0))
    (if (<= size eD2k-chunk-size)
        (set! result (MD4-bytevector (get-bytevector-n port size)))
        (let ((chunk (make-bytevector eD2k-chunk-size))
              (loops (modulo size eD2k-chunk-size))
              (checksums '()))
          (while (>= loops 0)
            (reverse! checksums)
            (get-bytevector-n! port chunk 0 eD2k-chunk-size)
            (push (MD4-bytevector chunk) checksums)
            (decf loops))
          ;;at this point there are < 9500k bytes left
          (push (MD4-bytevector (get-bytevector-all port)) checksums)
          (set! checksums (uint-list->bytevector (reverse! checksums)
                                                 (endianness big) 16))
          (set! result (MD4-bytevector checksums))))
    (close-port port)
    result))
(define (set-socket-nonblocking! sock)
  (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL))))
(define (wait-for-input fd timeout)
  (receive (secs usecs) (floor/ timeout 1)
    (set! usecs (round (* 1.9 usecs)))
    (select (list fd) '() '() secs usecs)))
(define (print-checksum sum)
  (format #t "~32,'0x\n" sum))
(define (format-checksum sum)
  (format #f "~32,'0x\n" sum))

;;;;anidb-commands
(define (anidb-connect)
  "Connect to the anidb server"
  (set! *anidb-socket* (socket PF_INET SOCK_DGRAM IPPROTO_UDP))
  (connect *anidb-socket* AF_INET *anidb-server* *anidb-port*)
  (set-port-encoding! *anidb-socket* "UTF-8"))
(define* (anidb-keepalive #:optional (interval 6000))
  "Initializes a timer which pings the server at specific intervals (default
is every 10 minutes)"
  (let ((handler
         (lambda (signum)
           (anidb-ping)
           (alarm interval))))
    (sigaction SIGALRM handler)
    (alarm interval)))
(define (anidb-init)
  "Connects to the anidb sever and runs initialization code"
  (let ((port *anidb-port*))
    (anidb-connect)
    (set! port (string-strip (anidb-ping 1)))
    (unless (eq? port *anidb-port*)
      (anidb-keepalive))))
(define (anidb-disconnect)
  "logout from anidb and cleanup resources"
  ;;attempt to logout, this prints a message if not logged in
  ;;but doesn't raise an error
  (anidb-logout)
  (alarm 0);;cancel any outstanding alarms
  (close *anidb-socket*));;close the socket
(define (anidb-escape-string str)
  ;;perhaps not the most efficent way to do this but oh well
  ;;also I'm just ignoring newlines, since who puts a newline into a title
  (regexp-substitute/global #f "&" str 'pre "&amp;" 'post))


(define (anidb-recv cmd)
  "Attempts to receive a responce from the anidb server for cmd. Blocks for
up to 1 second to see if there is output, then waits some ammount of time before
resending the command. The command is resent upto 3 times and the delay between
each is doubled every time"
  (let ((output-len #f))
    (do ((wait 1 (* wait 2)))
        ((or output-len (> wait 4)) output-len)
      (if (select (list *anidb-socket*) '() '() 1)
          (set! output-len (recv! *anidb-socket* *output-buffer*))
          (begin
            (sleep wait)
            (send *anidb-socket* cmd))))
    output-len))
;;Each responce has the form:
;;3-digit-return-code [data1] return-code-name(string) newline [data2]
;;the first data field is null for most responces except 200,201,271,272,504
;;I believe that return-code-name shouldn't have any spaces in it

;;returns 4 values return-code return-string data2 data'
;;in this order since data1 is least used
(define (anidb-get-response cmd)
  (let* ((output-len (anidb-recv cmd))
         (output-string (make-bytevector output-len)))
    (bytevector-copy! *output-buffer* 0 output-string 0 output-len)
    (set! output-string (utf8->string output-string))
    (let* ((index 4)
           (return-code (substring output-string 0 3))
           (data1 (if (equal-any? return-code '("200" "201" "271" "272" "504"))
                      (let ((data1-end (string-index output-string #\space 4)))
                        (set! index data1-end)
                        (substring output-string 4 index))
                      ""))
           (return-code-string
            (let ((str-end (string-index output-string #\newline index)))
              (prog1 (substring output-string index str-end)
                     (set! index str-end))))
           (data2 (substring output-string index)))
      (values return-code return-code-string data2 data1))))
;;special command, to complicated to use define-anidb-command
(define* (anidb-login #:optional username passwd)
  (if (not username)
      (set! username (readline "username: ")))
  (if (not passwd)
      (set! passwd (getpass "password: ")))
  (let ((command
         (format #f "AUTH user=~a&pass=~a&protover=~a&client=~a&clientver=~a"
                 username passwd *api-version* *client-name* *client-version*)))
    (send *anidb-socket* command)
    (receive (code msg data2 data1) (anidb-get-response command)
      (if (not (or (equal? code "200") (equal? code "201")))
        (anidb-handle-error code msg command)
        (begin (set! *session-key* data1)
               (print "Connection to anidb successful"))))))
;;special command, too simple to use define-anidb-command
(define (anidb-logout)
  (if (not *session-key*)
      (print "Logout error, not logged in")
      (begin (send *anidb-socket* (format #f "LOGOUT s=~a" *session-key*))
             (set! *session-key* #f))))
;;Encryption, notification and buddy commands  aren't supported
;;because I don't need them
(define (anidb-encrypt)
  (error "Encrypted sessions not supported"))
(define (anidb-push)
  (error "Notification commands unsupported"))
(define (anidb-notify)
  (error "Notification commands unsupported"))
(define (anidb-notifylist)
  (error "Notification commands unsupported"))
(define (anidb-notifyget)
  (error "Notification commands unsupported"))
(define (anidb-notifyack)
  (error "Notification commands unsupported"))
(define (anidb-pushack)
  (error "Notification commands unsupported"))
(define (anidb-buddyadd)
  (error "Buddy commands unimplemented"))
(define (anidb-buddydel)
  (error "Buddy commands unimplemented"))
(define (anidb-buddyaccept)
  (error "Buddy commands unimplemented"))
(define (anidb-buddydeny)
  (error "Buddy commands unimplemented"))
(define (anidb-buddylist)
  (error "Buddy commands unimplemented"))
(define (anidb-buddystate)
  (error "Buddy commands unimplemented"))

(define (amask->anime-info-fields amask)
  (let ((fields '()))
    (map (lambda (x) (when (logbit-reverse? x amask)
                       (push (vector-ref *anime-info-fields* x) fields)))
         (iota (integer-length amask)))
    (reverse! fields)))
(define (collect-fields str)
  (reverse!
   (fold-matches "([^|]*)|" str '()
                 (lambda (match fields)
                   (cons (match:substring match 1) fields)))))
(define (get-anime-info-fields anime-info . fields)
  (reverse!
   (fold
    (lambda (x last)
      (let ((value (assoc-ref anime-info x)))
        (if value (cons value last) last)))
    '() fields)))
;;this returns an alist of (field-type . value) pairs
(define-anidb-command (anidb-anime aid-or-name
                                   #:optional (amask *default-anime-mask*))
  (cond
   ((integer? aid-or-name)
    (format #f "ANIME aid=~d&amask=~x&s=~a"
            aid-or-name amask *session-key*))
   ((string? aid-or-name)
    (format #f "ANIME aname=~a&amask=~x&s=~a"
            aid-or-name amask *session-key*))
   (else (error "anidb-anime reqires an integer or a string argument")))
  "230"
  (map cons (amask->anime-info-fields amask) (collect-fields data)))

;;special command, has to recieve multiple responces
;;so can't ues define-anidb-command
(define (andbd-animedesc aid)
  "Return a description of the anime specified by aid"
  (let acc ((output '())
            (partno 0))
    (let ((command
           (format #f "ANIMEDESC aid=~a&part=~a&s=~a" aid partno *session-key*)))
      (send *anidb-socket* command)
      (receive (code msg data) (anidb-get-response command)
        (if (not (equal? code "233"))
          (anidb-handle-error code msg command)
          (receive (cur-part max-part desc) (string-split data #\|)
            (if (equal? cur-part max-part)
                (string-join (append output desc))
                (acc (append output desc) (1+ partno)))))))))
(define-anidb-command (anidb-calendar)
  (format #f "CALENDAR s=~a" *session-key*) "297"
  (string-split data "\n"))
(define-anidb-command (anidb-character id)
  (format #f "CHARACTER charid=~a&=~a" id *session-key*) "235"
  (let ((fields
         '("charid" "character name kanji" "character name transcription"-
           "pic" "blocks" "episode list" "last update date" "type" "gender")))
    (map cons fields (collect-fields data))))
(define-anidb-command (anidb-creator id)
  (format #f "CREATOR creatorid=~a&s=~a" id *session-key*) "245"
  (let ((fields
         '("creatorid" "creator name kanji" "creator name transcription"
           "type" "pic_name" "url_english" "url_japanese" "wiki_url_english"
           "wiki_url_japanese" "last update date")))
    (map cons fields (collect-fields (string-strip data)))))
(define-anidb-command (anidb-episode id-name #:optional number)
  (if number
      (format #f "EPISODE ~a=~a&epno=~a&s=~a"
              (if (number? id-name) "aid" "aname");;error checking needed
              id-name number *session-key*)
      (format #f "EPISODE eid=~a&s=~a" id-name *session-key*))
  "240"
  (let ((fields
       '("eid" "aid" "length" "rating" "votes"
         "epno" "eng" "romaji" "kanji" "aired" "type")))
  (map cons fields (collect-fields (string-strip data)))))
;;need defaults for fmask and amosk
(define-anidb-command (anidb-file filename #:optional fmask amask)
  (let ((size (stat:size (stat filename)))
        (ed2k (eD2k-file filename)))
    (format #f "FILE size=~d&ed2k=~32,'0x&fmask=~a&amask=~a&s=~a"
            size ed2k fmask amask *session-key*))
  '("220" "322")
  (let ((file-fields '())
        (anime-fields '()))
    (error "File command unimplemented")))
(define (anidb-group id-or-name)
  (format #f "GROUP ~a=~a&s=~a" (if (number? id-or-name) "gid" "gname")
          id-or-name *session-key*)
  "250"
  (let ((fields
         '("gid" "rating" "votes" "acount" "fcount" "name" "short" "irc channel"
           "irc server" "url" "picname" "foundeddate" "disbandeddate" "dateflags"
           "lastreleasedate" "lastactivitydate" "grouprelations")))
    (map cons fields (collect-fields (string-strip data)))))
(define (anidb-groupstats)
  (error "Groupstats command unimplemented"))
;;This is probably the most complicated command, then again, it is the only
;;command that let's you query your mylist so it needs to do a lot
(define-anidb-command (anidb-mylist #:key lid fid filename anime)
  "Query mylist for a file, specified by a file id, mylist id or filename,
or an anime specified by a name/anime id or a list of the form
(name/anime-id group-name/group-id [episode-number])"
  (cond
   (lid (format #f "MYLIST lid=~a&s=~a" lid *session-key*))
   (fid (format #f "MYLIST fid=~a&s=~a" fid *session-key*))
   (filename
    (let ((size (stat:size (stat filename)))
          (ed2k (eD2k-file filename)))
      (format #f "MYLIST size=~d&ed2k=~32,'0x&s=~a"
              size ed2k *session-key*)))
   (anime
    (let ((aid-or-name (if (list? anime)
                           (car anime) anime))
          (gid-or-name (if (list? anime)
                           (cadr anime) #f))
          (epno (if (pair? (cddr anime))
                    (caddr anime) #f)))
      (format #f "MYLIST ~a=~a~a~a&s=~a"
              (if (number? aid-or-name) "aid" "aname") aid-or-name
              (if gid-or-name
                  (format #f "&~a=~a"
                          (if (number? gid-or-name) "gid" "gname") gid-or-name)
                  "")
              (if epno (format #f "&epno=~a" epno) "")
              *session-key*)))
   (else (error (concat "One of lid, fid, filename, or anime must be"
                               "supplied to the mylist command"))))
  (list "221" "321")
  (let ((fields
         (if (equal? code "221")
             ;;fields retured for a single episode
             '("lid" "fid" "eid" "aid" "gid" "date" "state"
               "viewdate" "storage" "source" "other" "filestate")
             ;;non-group fields returned for multiple episodes
             ;;several group fields are returned depending on the number
             ;;of groups that translated the anime
             '("anime title" "episodes" "eps with state unknown"
               "eps with state on hhd" "eps with state on cd"
               "eps with state deleted" "watched eps")))
        (results (collect-fields (string-strip data))))
    (if (equal? code "221")
        (map cons fields results)
        (append (map cons fields results)
                (cons "groups" (drop (length fields) results))))))
(define-anidb-command (anidb-mylistadd
                       #:key fid lid filename anime;;one of these is required
                       ;;all of these are optional
                       state viewed viewdate source storage other edit)
  ;;a bit of complexity in format here, ~@[stuff] outputs stuff,
  ;;which should have one format clause, only if it's argument is not #f
  ;;if it is #f then nothing is output
  ;;TODO: Reformat other format strings like this, it's much cleaner
  (let
      ((options
        (format #f
          (concat
           "~@[&state=~a~]~@[&viewed=~a~]~@[&viewdate=~a~]~@[source=~a~]"
           "~@[&storage=~a~]~@[&other=~a~]~@[edit=~a~]&s=~a")
          state viewed viewdate source storage other edit *session-key*))
       (anime-specifier
        (cond
         (lid (format #f "lid=~a" lid))
         (fid (format #f "fid=~a" fid))
         (filename
          (let ((size (stat:size (stat filename)))
                (ed2k (eD2k-file filename)))
            (format #f "size=~d&ed2k=~32,'0x&" size ed2k)))
         (anime
          (let ((aid-or-name (if (list? anime)
                                 (car anime) anime))
                (gid-or-name (if (list? anime)
                                 (cadr anime) #f))
                (epno (if (pair? (cddr anime))
                          (caddr anime) #f)))
            (format #f "~a=~a~a~a"
                    (if (number? aid-or-name) "aid" "aname") aid-or-name
                    (if gid-or-name
                        (format #f "&~a=~a"
                                (if (number? gid-or-name) "gid" "gname") gid-or-name)
                        "")
                    (if epno (format #f "&epno=~a" epno) ""))))
         (else (error
                (concat "One of lid, fid, filename, or anime must be"
                        "supplied to the mylist command"))))))
    (format #f "MYLISTADD ~a~a" anime-specifier options))
  (list "210" "310" "311" "322")
  (case-equal code
   ;;returns the lid if one episode was added,
   ;;or else the number of episodes added
   ("210" data)
   ("310") ;;file already exists return an alist of fields
   ("311" data);;entry/entries modified, return number modified
   ;;multiple files found for an anime   ;
   ("322")))
(define (anidb-mylistdel)
  (error "Mylistdel command unimplemented"))
(define-anidb-command (anidb-myliststats)
  (format #f "MYLISTSTATS s=~a" *session-key*)
  "222"
  (let ((fields
        '("animes" "eps" "files" "size of files" "added animes" "added eps"
          "added files" "added groups" "leech %" "glory %" "viewed % of db"
          "mylist % of db" "viewed % of mylist" "number of viewed eps" "votes"
          "reviews" "viewed length in minutes"))
        (results (collect-fields (string-strip data))))
    ;;create an alist with fields as the keys
    ;;and the returned data as values, and return that
    (map cons fields results)))
(define-anidb-command (anidb-vote id-or-name value #:key group epno)
  ;;vote paramater of anidb command is an integer 100-1000, argument to the
  ;;vote function is a floating point number 
  (let ((vote (round (* 100 value))))
    (format #f "VOTE type=~a&~a=~a&value=~a~@[&epno=~a~]" (if group 3 1)
            (if (string? id-or-name) "name" "id")) id-or-name
            vote epno)
  (list "260" "261" "262" "263")
  (error "Vote command unimplemented"))

(define-anidb-command (anidb-random type)
  (format #f "RANDOM type=~a&s=~a" type *session-key*) "230"
  (map cons (amask->anime-info-fields *default-anime-mask*) (collect-fields data)))

(define (anidb-mylistexport template)
  (error "Mylistexport command unimplemented"))
(define (anidb-mylistexport-cancel)
  (error "Mylistexport command unimplemented"))
(define-anidb-command (anidb-ping #:optional nat)
  ;;no need to be logged in for this one
  (if nat "PING nat=1" "PING")
  "300"
  (if nat;;if nat=1
      data;;return the port
      #t))
(define-anidb-command (anidb-version)
  "VERSION"
  "998"
  data)
(define-anidb-command (anidb-uptime)
  (format #f "UPTIME s=~a" *session-key*)
  "208"
  data)
(define (anidb-encoding)
  (error "Encoding command unimplemented"))
(define (anidb-sendmsg)
  (error "Sendmsg command unimplemented"))
(define (anidb-user)
  (error "User command unimplemented"))
;;find the argument for the 'thing' field in an anidb command
;;matching is done case insensitively
(define (find-thing thing str)
  (let* ((thing-start (string-contains-ci str thing))
         ;;add 1 to account for the trailing = sign
         (thing-end (+ thing-start (string-length thing) 1))
         (data-end (or (string-index str #\& thing-end)
                       (string-length str))))
    ;;currently this fails to account for the case where thing is found in
    ;;another part of str, which should be handled by checking that
    ;;thing is at the start of string or (eq (string-ref (1- thing-start) #\&))
    ;;I don't know what to do if that's the case though
    (substring thing-end data-end)))
(define* (anidb-handle-error err-code err-name cmd #:optional (err-msg ""))
  ;;err-code is a 3 character string giving the error number
  (let ((err-string
        (cond
         ((eqv? (string-ref err-code 0) #\6)
          (format #f "Anidb error ~a: ~a - ~a\n"
                  err-code err-name err-msg ))
         ((equal-any? err-code '("330" "333" "334" "335" "336" "337" "338"
                                 "340" "343" "344" "345" "350" "351"
                                 "410" "411"))
          ;;these errors are all of the form NO_SUCH_THING
          (let* ((thing (substring err-name 8));;i.e anime, group, series,etc
                 (thing-name (find-thing thing cmd)));;name of the above thing
            (format #f "Ainbd error ~a: No such ~a \"~a\"\n"
                    err-code (string-capitalize thing) thing-name)))
         ((equal? err-code "598")
          (format #f "Internal error: Unknown Command ~a\n" cmd))
         ((equal? err-code "500")
          (format #f "Login failed, please try again\n"))
         ((equal? err-code "501")
          (format #f "Error: Not logged in, please login to continue\n"))
         ((equal? err-code "505")
          (format #f "Error: Illegal input or Access denied\nCommand was ~a" cmd))
         ((or (equal? err-code "502") (equal? err-code "506"))
          (format #f "Internal Error: Anidb access denied\n"))
         (else (format #f "Error ~a ~a ~a\nCommand was ~a"
                       err-code err-name err-msg cmd)))))
    (write err-string (current-error-port))
  (throw 'anidb-error err-string)))

;;Info about amask passed to anime command
;;Amask is a 7 byte bitmask indicating what information about the anime should
;;be returned, it is constructed as follows:
;;Byte1 (int aid) (int dateflags) (str year) (str type) (str related-aid-list)
;;      (str related-aid-type) (str category-list) (str category-weight-list)
;;Byte2 (str romaji-name) (str kanji-name) (str english-name) (str other-name)
;;      (str short-name-list) (str synonym-list) (unused) (unused)
;;Byet3 (int4 num-episodes) (int4 max-episode-num) (int4 num-special-episodes)
;;      (int air-date) (int end-date) (str url)
;;      (str picname) (str cadegory-id-list)
;;Byte4 (int4 rating) (int vote-count) (int4 temp-rating) (int temp-vote-count)
;;      (int4 avg-review) (int review-count) (str award-list) (bool is-18+)
;;Byte5 (int anime-planet-id) (int ann-id) (int allcinema-id)
;;      (int AnimeNfo-id) (int date-record-updated)
;;Byte6 (int character-id-list) (int creator-id-list) (int main-creator-id-list)
;;      (str main-creator-name-list) (unused) (unused)
;;Byte7 (int4 specials-count) (int4 credits-count) (int4 other-count)
;;      (int4 trailer-count) (int4 parody-count)

;;User interface

(define *anidb-commands*
  '("AUTH" "LOGOUT" "ENCRYPT";;auth cmds
    "PUSH" "NOTIFY" "NOTIFYLIST" "NOTIFYGET" "NOTIFYACK" "PUSHACK";;notify cmds
    "ANIME" "ANIMEDESC" "CALENDAR" "CHARACTER" "CREATOR" ;;data cmds 1
    "EPISODE" "FILE" "GROUP" "GROUPSTATS" ;;data cmds 2
    "MYLIST" "MYLISTADD" "MYLISTDEL" "MYLISTSTATS" "VOTE" "RANDOM" ;;mylist cmds
    "MYLISTEXPORT" "PING" "VERSION" "UPTIME" "ENCODING" "SENDMSG" "USER")) ;;misc
(define anidb-readline-completer
  (make-completion-function
   (append *anidb-commands*
           (map string-downcase *anidb-commands*)
           (map string-capitalize *anidb-commands*))))
;;just for convience
(define (anidb-readline)
  (with-readline-completion-function anidb-readline-completer readline))
;;It'd be nice to have filename completion for some commands
;;
(eval-when ()
(when running
  (set-readline-prompt "> ")
  (let ((line "")(cmd "")(args "")(results #f))
  (while #t
    (set! line (anidb-readline))
    (set! cmd (some-function line))
    (set! args (some-other-function line))
    (set! results
          (catch 'anidb-error
            (lambda () (apply (function-to-turn-cmd-into-a-function) args))
            (lambda (key args) #f)))
    (when results
      (function-to-process-results cmd results))))))
