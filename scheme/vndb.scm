#!/usr/bin/guile -s
!#
;;If we don't load the extension lib when compiling we get warnings
(eval-when (compile load eval)
  (add-to-load-path (getcwd))
  ;;I could check for the .so file and compile it if I can't find it
  ;;but that seems a bit silly
  (load-extension
   (string-join (list (getcwd) "libguile-vndb")
                file-name-separator-string) "init_vndb"))
(use-modules (rnrs bytevectors) (rnrs io ports) (ice-9 receive) (ice-9 regex)
             (ice-9 hash-table) (ice-9 futures) (srfi srfi-1)
             (web client)(json) (util))
;;Constants
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
(define *client-version* 0.2)

;;Global variables, I use a bunch of these but I have no intent to make
;;this multithreaded so it's fine

;;Currently the cache is fairly small, so writing/reading it to/from
;;disk is quick, I'm not sure for how long though
(define *vndb-cache-file* (concat (getcwd) "/vndb.cache"))
(define *vndb-tags-file* (concat (getcwd) "/vndb.tags"))
(define *vndb-traits-file* (concat (getcwd) "/vndb.traits"))


;;TODO: read/write the cache to/from disk in a seperate thread
(define (read-cache) (read-from-file-and-eval *vndb-cache-file*))
(define (write-cache) (with-output-to-file *vndb-cache-file*
                        (lambda () (print-hash-table *vndb-cache*))))
(define (write-cache-backup)
  (with-output-to-file (concat *vndb-cache-file* ".bkup")
                        (lambda () (print-hash-table *vndb-cache*))))
(define *vndb-cache* (read-cache))
(define (vnlist-cache) (hash-ref *vndb-cache* "vnlist"))
(define (vn-cache) (hash-ref *vndb-cache* "VNs"))

(define *vndb-tags*
  (false-if-exception (read-from-file-and-eval *vndb-tags-file*)))
(define *vndb-traits*
  (false-if-exception (read-from-file-and-eval *vndb-traits-file*)))

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

(define (vndb-send msg)
  (if *vndb-tls*
      (tls-send *vndb-tls* msg)
      (send *vndb-socket* msg)))
;;Read from conn into msgbuf using fn until the last byte read is
;;eq to delim, delim needs to be a byte, if it's a character this
;;will never return
(define (recv-delim fn conn msgbuf delim)
  ;;optimize the case where we get the whole message in one try
  (let ((nbytes (fn conn msgbuf)))
    (if (eq? (bytevector-u8-ref msgbuf (1- nbytes)) delim)
        (bytevector-copy (bytevector-slice msgbuf 0 nbytes))
        (let* ((buf (bytevector-copy msgbuf))
               (buflen nbytes))
          (while (not (eq? (bytevector-u8-ref buf (1- buflen)) delim))
            (set! nbytes (fn conn msgbuf))
            (when (< (bytevector-length buf) (+ buflen nbytes))
              ;;doubles the size of buf
              (set! buf (bytevector-extend buf (bytevector-length buf))))
            (bytevector-memcpy (buf buflen) msgbuf nbytes)
            (set! buflen (+ nbytes buflen)))
          (bytevector-slice buf 0 buflen)))))

(define (vndb-recv!)
  (let ((response
         (if *vndb-tls* (recv-delim tls-recv! *vndb-tls* *output-buf* 4)
             (recv-delim recv! *vndb-socket* *output-buf* 4))))
    (if (and (> (bytevector-length response) 5)
             (equal? "error" (utf8->string (bytevector-slice response 0 5))))
        (begin
          (format (current-error-port) "throwing vndb-error ~s"
                  (utf8->string (bytevector-slice response 5)))
          (throw 'vndb-error (utf8->string (bytevector-slice response 5))))
             (utf8->string response))))

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
;;The form of the response of the dbstats command is "dbstats json-obj"
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

;;TODO: if possible check the cache before making a request
(define* (vndb-get type vndb-filter #:optional (flags "basic")
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
  (let ((cmd
         (format #f "get ~a ~a ~a ~a\x04" type flags vndb-filter options)))
    (print cmd)
  (vndb-send cmd)
  (substring (vndb-recv!) 8)))

;;Haven't tested this yet
(define (vndb-set type id fields)
  (let ((cmd (format #f "set ~a ~a ~a\x04" type id
                     (scm->json-string (alist->hash-table fields)))))
    (vndb-send cmd)
    (vndb-recv!)))
(define (catch-vndb-err err-type err-msg)
  "Catch a vndb-error, if it's a throttled error wait until we can
make more queries, otherwise throw the error again"
  (format (current-error-port) "Caught error ~a" err-msg)
  (let ((err (json-string->scm err-msg)))
    (if (equal? "throttled" (hash-ref err "id"))
        (begin
          (format (current-error-port)
                  "Vndb server throttled (type = ~s), waiting ~s seconds\n"
                  (hash-ref err "type") (hash-ref err "fullwait"))
          (sleep (ceiling (hash-ref err "fullwait"))))
        (throw err-type err-msg))))
;;Collect all the results from a vndb get command
(define-macro (vndb-get-all what filter flags num-results . body)
  `(let ((page 1) (loop #t)) ;;these are delibrately accessable from the body code
     (while loop
       (catch 'vndb-error
        (lambda ()
          (let ((response (json-string->scm
                           (vndb-get ,what ,filter ,flags
                                     #:page page #:results ,num-results))))
            ,@body
            (incf page)
            ;;loop if there are more entries
            (set! loop (hash-ref response "more"))))
        catch-vndb-err))))
(define (get-vnlist)
  (vndb-get-all "vnlist" "(uid = 0)" "basic" 100
                (map cache-vnlist! (hash-ref response "items")))
  (write-cache))
(define* (get-vn filter #:optional (flags "basic"))
  (let ((results '()))
    (vndb-get-all "vn" filter flags 25
                  (begin (map cache-vn! (hash-ref response "items"))
                         (push! (hash-ref response "items")  results)))
         (write-cache)
         (let ((ret (make-hash-table)))
           (dolist (r results)
                   (for-each (lambda (x)
                               (hash-set! ret (hash-ref x "id") x)) r))
           ret)))


;; The error handling bit of the macro above may not work, so I'm leaving
;; the original code here
;; (define (get-vnlist)
;;   (let ((page 1) (results 100) (loop #t))
;;     (while loop
;;       ;;This is inside the loop in case we throttle the server, we can
;;       ;;wait a bit then continue to fetch the rest of the list
;;       (catch 'vndb-error
;;         (lambda ()
;;           (let ((response (json-string->scm
;;                            (vndb-get "vnlist" "(uid = 0)"
;;                                      #:page page #:results results))))
;;             (map cache-vnlist! (hash-ref response "items"))
;;             (incf page)
;;             ;;loop if there are more entries
;;             (set! loop (hash-ref response "more"))))
;;         (lambda (json-err)
;;           (let ((err (json-string->scm json-err)))
;;             (if (equal? "throttled" (hash-ref err "id"))
;;                 (begin
;;                   (format (current-error-port)
;;                           "Vndb server throttled (type = ~s), waiting ~s seconds\n"
;;                           (hash-ref err "type") (hash-ref err "fullwait"))
;;                   (sleep (ceiling (hash-ref err "fullwait"))))
;;                 (throw 'vndb-error json-err))))))))
(define (get-vnlist-vns)
  "Add all vns currently in the vnlist cache to the vn cache,
if they're not already cached"
  (let* ((vn-cache (vn-cache))
         (flags "basic,details,tags,stats")
         (vn-list (filter (lambda (x) (not (check-vn-cache x flags)))
                          (hash-map-keys->list (vnlist-cache) identity))))
    (vndb-get-all "vn" (format #f "(id = [~a])"
                               (string-join (map number->string vn-list) ","))
                  flags 25
                  (map cache-vn! (hash-ref response "items")))))

;;Checks if id is in the cache, with all the fields specified by flags
(define* (check-vn-cache id #:optional (flags "basic"))
  (let ((entry (hash-ref (vn-cache) id)))
    (if entry
        (if (equal? flags "basic") #t
            (let ((flags (map string-strip (string-split flags #\,))))
              ;;inefficient but it should be fast enough
              (if
               (fold (lambda (x y) (and x y)) #t
                    (map (lambda (flag)
                           (case-equal flag
                             ("details" (hash-ref entry "links"))
                             ("anime" (hash-ref entry "anime"))
                             ("relations" (hash-ref entry "relations"))
                             ("tags" (hash-ref entry "tags"))
                             ("stats" (hash-ref entry "rating"))
                             ("screens" (hash-ref entry "screens"))))
                         flags))
               #t #f)))
        #f)))

(define* (vndb-get-vns-by-id ids #:optional (flags "basic"))
  "Look up a list of vns by their ids, using cache entries if possible"
  (let ((needed (filter (compose not check-vn-cache) ids)))
    (unless (null? needed)
      (vndb-get-all "vn" (format #f "(id = [~a])"
                                 (string-join (map number->string needed) ","))
                    flags 25
                    (map cache-vn! (hash-ref response "items")))))
  (let ((vn-cache (vn-cache)))
    (map (lambda (x) (hash-ref vn-cache x)) ids)))

;;Some functions to parse returned data

;;This should probably be removed
(define (vnlist-get-vns vnlist)
  "Given a response from the \"get vnlist\" command return
a filter to select those vns in a \"get vn\" command"
  (let* ((ht (json-string->scm vnlist))
         (vn-ids (map (lambda (x) (hash-ref x "vn")) (hash-ref ht "items"))))
    (format #f "(id = [~a])" (string-join (map number->string vn-ids) ","))))
(define (get-items str)
  "Return a list of hashtables containing the items in str,
which is a response from a \"get\" \"foo\" command"
  (hash-ref (json-string->scm str) "items"))

;;;Functions to access the cache
(define (cache-vn! vn-ht)
  "Store a vn (represented as a scheme hash table) into the cache"
  (let ((id (hash-ref vn-ht "id")))
    (hash-set! (vn-cache) id vn-ht)))
(define (cache-vns! response)
  "Cache all the vns returned in a given response from the server"
  (for-each cache-vn! (get-items response)))
(define (cache-vnlist! vnlist-ht)
  "Cache all the vnlist entries in the given hash table"
  (let ((id (hash-ref vnlist-ht "vn")))
    (hash-set! (vnlist-cache) id vnlist-ht)))
(define* (empty-cache! #:optional force)
  (when (not force)
    (write "Really empty the cache (y/n): ")
    (if (eq? (read-char) #\y)
        (set! force #t)))
  (when force
    (with-output-to-file *vndb-cache-file*
      (lambda () (let ((ht (make-hash-table)))
                   (hash-set! ht "VNs" (make-hash-table))
                   (hash-set! ht "vnlist" (make-hash-table))
                   (print-hash-table ht))))))
(define (cache-lookup-vn id)
  (hash-ref (hash-ref *vndb-cache* "VNs") id))
;;When converting a string to json all non ascii unicode characters
;;are escaped, this function undoes that
(define (unescape-unicode str)
  (regexp-substitute/global #f "\\\\u([0-9a-f]{4})" str
    'pre (lambda (x)
           (integer->char (string->number (match:substring x 1) 16))) 'post))
(define (cache-print-vn id)
  (pprint
   (unescape-unicode
    (scm->json-string
     (hash-ref (hash-ref *vndb-cache* "VNs") id) #:pretty 1))))
(define (cache-list-vns)
  (hash-map->list (lambda (key val) key) (vn-cache)))
(define (cache-list-vnlist)
  (hash-map->list (lambda (key val) key) (vnlist-cache)))
(define (cache-lookup-vnlist id)
  (hash-ref (hash-ref *vndb-cache* "vnlist") id))
;;If I wrote macros to generate these functions they would be nearly as long
;;and much less readable
(define (download-tags-file)
  (receive (response body) (http-get "http://vndb.org/api/tags.json.gz")
    (put-bytevector (open-file "tags.json.gz" "w") body)))
(define (download-traits-file)
  (receive (response body) (http-get "http://vndb.org/api/traits.json.gz")
    (put-bytevector (open-file "traits.json.gz" "w") body)))
;;Optimally I'd make an array of tags indexed by id, and have
;;a hashtable which translated names to ids,
(define (parse-vndb-data-file infile outfile)
  (let ((buf (gunzip-bytevector
              (get-bytevector-all (open-file infile "r"))
              (* 1024 1024))))
    (let* ((data (json-string->scm (utf8->string buf)))
           (data-by-name
            (let ((tmp (make-hash-table (length data))))
              (for-each (lambda (x)
                          (hash-set! tmp (string-downcase
                                          (hash-ref x "name")) x)) data) tmp))
           (data-by-id
            (let ((tmp (make-hash-table (length data))))
              (for-each (lambda (x)
                          (hash-set! tmp (hash-ref x "id") x))
;;                                     (string-downcase (hash-ref x "name"))))
                          data) tmp))

           (ht (make-hash-table 2)))
      (hash-set! ht "by id" data-by-id)
      (hash-set! ht "by name" data-by-name)
      (with-output-to-file outfile (lambda () (print-hash-table ht)))
      ht)))
;;I'm not really worried about the processing time for creating duplicate
;;hash tables for ids and names, but about the fact that it uses excessive
;;storage, so it's find to make both and delete one here.
(define (parse-tags-file)
  (let* ((tags (parse-vndb-data-file "tags.json.gz" "vndb.tags"))
         (by-id (hash-ref tags "by id"))
         (ht (make-hash-table)))
    (hash-for-each (lambda (key val)
                     (hash-set! ht key
                      (string-downcase (hash-ref val "name")))) by-id)
    (hash-set! tags "by id" ht)
    (set! *vndb-tags* tags)))
;;Tags are unique (i.e there is no tag x where (name x) == (name y)
;;and (id x) != (id y)), This is not true for traits, (i.e there are
;;mutiple traits named green (hair color, eye color, etc..)), so we
;;need to make a unique name

(define (get-unique-trait-name by-id id)
  (let acc ((id id) (names '()))
    (let* ((trait (hash-ref by-id id))
           (parents (hash-ref trait "parents"))
           (name (string-downcase (hash-ref trait "name"))))
      (if (null? parents)
          (string-join (cons name names) ",")
          (acc (car parents)
               (cons name names))))))
;;should basically work for tags as well
(define (build-trait-tree traits)
  (let* ((n 2200);;should be high enough to cover all traits
         (vec (make-vector n))
         (i 0))
    (array-index-map! vec (lambda (i) (hash-ref traits i)))
    (while (< i n)
      (let ((trait (vref vec i))
            (parents #f))
        (when (not trait) (incf i) (continue))
        (set! parents (hash-ref trait "parents"))
        (for-each
         (lambda (x)
           (let ((parent (vref vec x)))
             (hash-set! parent "children"
                       (cons i (hash-ref parent "children" '())))))
         parents)
        (incf i)))
    vec))
(define (parse-traits-file)
  (let* ((traits (parse-vndb-data-file "traits.json.gz" "vndb.traits"))
         (by-id (hash-ref traits "by id"))
         (ht (make-hash-table))
         (get-unique-trait-name
          (lambda (x) (get-unique-trait-name by-id x))))
    ;;Ideally I would make this a tree with each trait having a number
    ;;of children, instead of this
    (hash-for-each (lambda (key val)
                     (hash-set! ht (get-unique-trait-name key) val)) by-id)
    (hash-set! traits "by name" ht)
    (set! *vndb-traits* traits)))

(define (lookup-tag name) (print-hash-table (get-tag-by-name name)))
(define (get-tag-by-name name) (hash-ref-multi *vndb-tags* "by name" name))
(define (get-tag-by-id id)
  (get-tag-by-name (hash-ref-multi *vndb-tags* "by id" id)))
;;The tag id's are not contiunous, presumably some tags have been deleted
;;over time. This code returns a list of all the ids that are not assigned
;; (filter identity (map
;;                   (lambda (x) (if (get-tag-by-id x) #f x))
;;                   (iota (reduce max #f
;;                                 (hash-map-keys->list
;;                                  (hash-ref *vndb-tags* "by id") identity)))))

(define (lookup-trait name) (print-hash-table (get-trait-by-name name)))
(define (get-trait-by-id id) (hash-ref-multi *vndb-traits* "by id" id))
(define (get-trait-by-name name) (hash-ref-multi *vndb-traits* "by name" name))

;;Higher level procedures for searching for vns
(define (vnlist-search re)
  "Search the names of vns in the vnlist and return any
than match the regular expression re"
  ;;Assumes all vns in the vnlist are in the cache

  ;;Make sure the regexp is precompiled
  (when (not (regexp? re))
    (set! re (make-regexp re)))
  (let ((matches '())
        (vnlist-cache (vnlist-cache))
        (vn-cache (vn-cache)))
    ;;This could be really slow, and I can probably optimize it if necessary
    (hash-for-each
     (lambda (key val)
       (let* ((vn (hash-ref vn-cache key))
              (title (hash-ref vn "title"))
              (original (hash-ref vn "original"))
              (aliases (hash-ref vn "aliases")))
         (when (or (and (string? title) (regexp-exec re title))
                   (and (string? original) (regexp-exec re original))
                   (and (string? aliases)
                        (for-each (lambda (x) (regexp-exec re x))
                                  (string-split aliases "\n"))))
           (push! vn matches))))
     vnlist-cache)
    matches))
;;(update-tags-file)
