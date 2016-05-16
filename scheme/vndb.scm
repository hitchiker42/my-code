#!/usr/bin/guile -s
!#

;;;Load modules/extensions
(eval-when (compile load eval)
  (add-to-load-path (getcwd))
  ;;I could check for the .so file and compile it if I can't find it
  ;;but that seems a bit silly
  (load-extension
   (string-join (list (getcwd) "libguile-vndb")
                file-name-separator-string) "init_vndb"))
(use-modules (rnrs bytevectors) (rnrs io ports) (ice-9 receive) (ice-9 regex)
             (ice-9 hash-table) (ice-9 futures) (ice-9 readline)
             (ice-9 buffered-input) (srfi srfi-1) (web client)(json) (util))
;;;Constants/Variables
(define *vndb-port* 19534)
(define *vndb-tls-port* 19535)
(define *vndb-hostname* "api.vndb.org")
(define *vndb-server* (sockaddr:addr
                       (addrinfo:addr
                        (car (getaddrinfo *vndb-hostname* #f)))))
(define *client-name* "vndb-scm")
(define *client-version* 0.2)
(define *vndb-cache-file* (concat (getcwd) "/vndb.cache"))
(define *vndb-tags-file* (concat (getcwd) "/vndb.tags"))
(define *vndb-traits-file* (concat (getcwd) "/vndb.traits"))

;;Functions to read/write the cache
(define (read-cache) (json->scm (open-file *vndb-cache-file* "r")))
(define* (write-cache #:optional bkup)
  (with-output-to-file *vndb-cache-file*
    (lambda () (scm->json *vndb-cache*)))
  (if bkup (with-output-to-file (concat *vndb-cache-file* ".bkup")
             (scm->json *vndb-cache*))))

;;Load cache/tags/traits, define them to false if they're undefined so
;;the variables actually exist, then load them in a seperate thread
(define-once *vndb-cache* #f)
;;Fetch these dynamically in case they get changed when adding to the cache
(define (vnlist-cache) (hash-ref *vndb-cache* "vnlist"))
(define (vn-cache) (hash-ref *vndb-cache* "VNs"))
(define-once *vndb-tags* #f)
(define-once *vndb-traits* #f)
(define (load-cache-files)
  (set! *vndb-cache* (false-if-exception (read-cache)))
  (set! *vndb-tags*
    (false-if-exception (json->scm (open-file *vndb-tags-file* "r"))))
  (set! *vndb-traits*
    (false-if-exception (json->scm (open-file *vndb-traits-file* "r")))))
;;This isn't really safe, I just kinda assume that this will finish loading
;;the files before I use any of the variables, which is usually true. It takes
;;a few (~3-5) seconds to load the caches, which is really noticable if I do
;;it it the main thread, but it's not so long that they won't be loaded before
;;I try to access them if I do it in a seperate thread
(eval-when (load)
  (call-with-new-thread load-cache-files))

(define *vndb-socket* #f)
(define *vndb-tls* #f);;tls session
;;Use a static 4k buffer for reading output from the socket
(define *output-buf* (make-bytevector 4096))

;;;Send/recv commands
;;Most of these do one thing if we're connected using tls and another
;;if we're just using a normal tcp connection
(define (vndb-send msg)
  (if *vndb-tls*
      (tls-send *vndb-tls* msg)
      (send *vndb-socket* msg)))

;;Read from conn into msgbuf using fn until the last byte read is eq to delim
(define (recv-delim fn conn msgbuf delim)
  ;;optimize the case where we get the whole message in one try
  (let ((nbytes (fn conn msgbuf)))
    (if (eq? (bytevector-u8-ref msgbuf (1- nbytes)) delim)
        (bytevector-copy (bytevector-slice msgbuf 0 nbytes))
        ;;This is exactly like reading a dynamic array in C
        (let* ((buf (bytevector-copy msgbuf))
               (buflen nbytes))
          (while (not (eq? (bytevector-u8-ref buf (1- buflen)) delim))
            (set! nbytes (fn conn msgbuf))
            (when (< (bytevector-length buf) (+ buflen nbytes))
              (set! buf (bytevector-extend buf (bytevector-length buf))))
            (bytevector-memcpy (buf buflen) msgbuf nbytes)
            (set! buflen (+ nbytes buflen)))
          (bytevector-slice buf 0 buflen)))))
;;Read a response from the server, and raise an exception if there
;;was and error response
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
;;Establish/Terminate connection to the server
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
;;;Functions corresponding to vndb api commands
(define login-command-alist
  `(("protocol" . 1)
    ("client" . ,*client-name*)
    ("clientver" . ,*client-version*)))
(define (vndb-login-anon)
  (let ((cmd (scm->json-string (alist->hash-table login-command-alist))))
    (vndb-send  (concat "login" cmd "\x04"))
    (vndb-recv!)))
(define (vndb-login-user username password)
  (let ((cmd (scm->json-string
              (alist->hash-table
               (append login-command-alist `(("username" . ,username)
                                             ("password" . ,password)))))))
    (vndb-send (concat "login" cmd "\x04"))
    (vndb-recv!)))
;;Higer level function to connect to the server and login, tls is
;;used if a username and password are provided
(define* (vndb-login #:optional username password)
  (if password
      (begin (vndb-connect-tls)
             (vndb-login-user username password))
      (begin (vndb-connect)
             (vndb-login-anon))))

;;Responses from the server all all in the form:
;;response-type { response-object }
;;We always cut off the response-type in the string we return
(define (vndb-dbstats)
  (vndb-send "dbstats\x04")
  (substring (vndb-recv!) 8))

;;Get commands return an object with 3 members:
;;num is the number of results, items is an array of the results, and
;;more indicates if more results are available (by resending the same
;;command with a higher page number)
(define* (vndb-get type vndb-filter #:optional (flags "basic")
                   #:key page results sort reverse #:rest options)
  (if (not (null? options))
      (begin
        (set! options
          (scm->json-string
           (alist->hash-table
            (filter (lambda (x) (cdr x))
                    (list (cons "page" page) (cons "results" results)
                          (cons "sort" sort) (cons "reverse" reverse)))))))
      (set! options ""))
  (let ((cmd
         (format #f "get ~a ~a ~a ~a\x04" type flags vndb-filter options)))
    ;;(print cmd)
  (vndb-send cmd)
  (substring (vndb-recv!) 8)))

(define (vndb-set type id fields)
  (let ((cmd (format #f "set ~a ~a ~a\x04" type id
                     (scm->json-string (alist->hash-table fields)))))
    (vndb-send cmd)
    (vndb-recv!)))

;;;Wrappers around the basic command functions which are eaiser to use

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
(define (get-items str)
  "Return the items field from a response command as a list" 
  (hash-ref (json-string->scm str) "items"))
;;Collect all the results from a vndb get command
;;Code in body has access to the 'response' variable which contains
;;the response object in the form of a hash table
(define-macro (vndb-get-all what filter flags num-results . body)
  `(let ((page 1) (loop #t)) ;;these are accessible in the body code
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
;;Get & cache the entire vnlist, only works if logged in, doesn't return
;;anything, the vnlist can be accessed from the cache
(define (get-vnlist)
  (vndb-get-all "vnlist" "(uid = 0)" "basic" 100
                (map cache-vnlist! (hash-ref response "items")))
  (write-cache))
(define (get-vnlist-vns)
  "Cache all vns currently in the vnlist cache and not the vn cache"
  (let* ((vn-cache (vn-cache))
         (flags "basic,details,tags,stats")
         (vn-list (filter (lambda (x) (not (check-vn-cache x flags)))
                          (hash-map-keys->list (vnlist-cache) identity))))
    (unless (null? vn-list)
      (vndb-get-all "vn" (format #f "(id = [~a])"
                                 (string-join (map number->string vn-list) ","))
                    flags 25
                    (map cache-vn! (hash-ref response "items"))))))

(define* (get-vns-by-id ids #:optional (flags "basic"))
  "Look up a list of vns by their ids, using cache entries if possible"
  (let ((needed (filter (compose not check-vn-cache) ids)))
    (unless (null? needed)
      (vndb-get-all "vn" (format #f "(id = [~a])"
                                 (string-join (map number->string needed) ","))
                    flags 25
                    (map cache-vn! (hash-ref response "items")))))
  (let ((vn-cache (vn-cache)))
    (map (lambda (x) (hash-ref vn-cache x)) ids)))

;;use the cache here
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

;;;Cache functions

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
(define (cache-vn! vn-ht)
  "Store a vn (represented as a scheme hash table) into the cache"
  (let ((id (hash-ref vn-ht "id")))
    (hash-set! (vn-cache) id vn-ht)))
(define (cache-vns! response) ;;response is a json string
  "Cache all the vns returned in a given response from the server"
  (for-each cache-vn! (get-items response)))
(define (cache-vnlist! vnlist-ht)
  "Cache all the vnlist entries in the given hash table"
  (let ((id (hash-ref vnlist-ht "vn")))
    (hash-set! (vnlist-cache) id vnlist-ht)))
(define* (empty-cache! #:optional force)
  "Empty the cache, requires conformation if force is not set"
  (when (not force)
    (write "Really empty the cache (y/n): ")
    (if (eq? (read-char) #\y)
        (set! force #t)))
  (when force
    (with-output-to-file *vndb-cache-file*
      (lambda () (let ((ht (make-hash-table)))
                   (hash-set! ht "VNs" (make-hash-table))
                   (hash-set! ht "vnlist" (make-hash-table))
                   (set! *vndb-cache* ht)
                   (scm->json ht))))))
;;Look up stuff in the cache
(define (cache-lookup-vn id)
  (hash-ref-multi *vndb-cache* "VNs" id))
(define (get-vnlist-entry id)
  (hash-ref-multi *vndb-cache* "vnlist" id))
(define (print-vnlist-entry id)
  (print-hash-table (hash-ref-multi *vndb-cache* "vnlist" id)))
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

;;;Higher level procedures to search/parse vns
(define (vnlist-search re)
  "Find vns on the vn list who's titles match the regular expression re"
  ;;Assumes all vns in the vnlist are in the cache
  (when (not (regexp? re))  ;;Make sure the regexp is precompiled
    (set! re (make-regexp re)))
  (let ((matches '())
        (vnlist-cache (vnlist-cache))
        (vn-cache (vn-cache)))
    ;;This could be really slow, and I can probably optimize it if necessary
    (hash-for-each
     (lambda (key val)
       (let* ((vn (hash-ref vn-cache key)) ;;Check all titles
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

;;;Functions to download/parse tags/traits
(define (download-tags-file)
  (receive (response body) (http-get "http://vndb.org/api/tags.json.gz")
    (put-bytevector (open-file "tags.json.gz" "w") body)))
(define (download-traits-file)
  (receive (response body) (http-get "http://vndb.org/api/traits.json.gz")
    (put-bytevector (open-file "traits.json.gz" "w") body)))
;;Optimally I'd make an array of tags indexed by id, and have
;;a hashtable which translated names to ids, Actually I could do this
;;now since I use json to serialize stuff
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
;;      (with-output-to-file outfile (lambda () (print-hash-table ht)))
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
    (set! *vndb-tags* tags))
  (with-output-to-file *vndb-tags-file*
    (lambda () (scm->json *vndb-tags*))))
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
    (set! *vndb-traits* traits))
  (with-output-to-file *vndb-traits-file*
    (lambda () (scm->json *vndb-traits*))))

;;;Lookup tags/traits
(define (lookup-tag name) (print-hash-table (get-tag-by-name name)))
(define (get-tag-by-name name) (hash-ref-multi *vndb-tags* "by name" name))
(define (get-tag-by-id id)
  (get-tag-by-name (hash-ref-multi *vndb-tags* "by id" id)))
;;The tag id's are not contiunous, presumably some tags have been deleted
;;over time. This code returns a list of all the ids that are not assigned
(define (get-num-tags/traits ht)
  (filter identity (map
                    (lambda (x) (if (get-tag-by-id x) #f x))
                    (iota (reduce max #f
                                  (hash-map-keys->list
                                   (hash-ref ht "by id") identity))))))
(define (lookup-trait name) (print-hash-table (get-trait-by-name name)))
(define (get-trait-by-id id) (hash-ref-multi *vndb-traits* "by id" id))
(define (get-trait-by-name name) (hash-ref-multi *vndb-traits* "by name" name))

(eval-when (load)
  ;;This is a somewhat clunky way to test if we're running as a script
  (let ((re (make-regexp "vndb.scm")))
    (when (find (lambda (x) (regexp-exec re x)) (command-line))
      (set-readline-prompt! "$ " "... ")
      (set-current-input-port (readline-port))
      (while #t
        (set-buffered-input-continuation?! (current-input-port) #f)
        ;;In one line, but doing it this way makes it perl rather than repl
        ;;(print (eval (read) (interaction-environment)))
        (let* ((line (read (readline-port)))
               (sexp (eval line (interaction-environment))))
          (when (eof-object? sexp) (break))
          (pprint sexp))))))
               
