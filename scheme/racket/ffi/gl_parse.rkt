#lang racket/base
(require "../util.rkt" "../io.rkt" "ffi.rkt")
(require (for-syntax racket/base racket/syntax))
(require sxml)

(define (xml->sxml input (namespace-prefixes null))
  (let ((port (if (string? input) (open-input-string input) input)))
    (ssax:xml->sxml port namespace-prefixes)))
(define (sxml:elements sxml)
  (filter sxml:element? (sxml:content sxml)))
(define xml-test-types #<<  EOF
  <types>
  <type requires="stddef">typedef ptrdiff_t <name>GLsizeiptrARB</name>;</type>
  <type requires="inttypes">typedef int64_t <name>GLint64EXT</name>;</type>
  <type requires="inttypes">typedef uint64_t <name>GLuint64EXT</name>;</type>
  </types>
  EOF
  )
(define sxml-test-types (xml->sxml xml-test-types))
(define xml-test-enum #<<  EOF
  <enums namespace="GL" group="ClientAttribMask" type="bitmask">
  <enum value="0x00000001" name="GL_CLIENT_PIXEL_STORE_BIT"/>
  <enum value="0x00000002" name="GL_CLIENT_VERTEX_ARRAY_BIT"/>
  <enum value="0xFFFFFFFF" name="GL_CLIENT_ALL_ATTRIB_BITS"/>
  </enums>
  EOF
  )
(define sxml-test-enum (xml->sxml xml-test-enum))
(define xml-test-command #<<  EOF
  <command>
  <proto>void <name>glDrawArrays</name></proto>
  <param group="PrimitiveType"><ptype>GLenum</ptype> <name>mode</name></param>
  <param><ptype>GLint</ptype> <name>first</name></param>
  <param><ptype>GLsizei</ptype> <name>count</name></param>
  <glx type="render" opcode="193"/>
  </command>
  EOF
  )
(define sxml-test-command (xml->sxml xml-test-command))
(define gl-typedef-regex
  #px"typedef *((?:un)?signed)? *(\\*+)? *(.+) *")
(define (string->ctype str unsigned)
  (if unsigned
      (format-symbol "_u~a" str)
      (format-symbol "_~a" str)))
(define (parse-typedef str)
  (let* ((re-match (regexp-match gl-typedef-regex str))
         ((values $0 $1 $2 $3) (list->values re-match)))
    (if $2 _pointer
        (format-symbol
         (if (equal? $1 "unsigned") "_u~a" "_~a") $3))))

;;The functions to parse a type/enum/command return a pair whose car is
;;a string identifing the feature and cdr is racket code to define it.
;;The functions to parse features subdivide the definations above into
;;named groups.

(define (parse-type sxml)
  (assert! (eq? (sxml:element-name sxml) 'type))
  (if (equal? (sxml:attr sxml 'requires) "khrplatform") #f
    (match (sxml:content sxml)
      ((list typedef (list 'name typename) ";")
       (let ((ctype (parse-typedef (string-trim typedef))))
         (if (not ctype) #f
             (cons typename `(define ,(format-symbol "_~a" typename) ,ctype)))))
      (_ #f))))
(define (parse-types sxml)
  (assert! (eq? (sxml:element-name sxml) 'types))
  (let ((types (filter (lambda (x) (and (sxml:element? x)
                                        (eq? (sxml:element-name x) 'type)))
                       (sxml:content sxml))))
    (filter-map parse-type types)))

(define (translate-enum-name str)
  (concat "GL-" (string-replace (string-downcase (string-slice str 3)) "_" "-")))
(define (parse-enum sxml)
  (assert! (eq? (sxml:element-name sxml) 'enum))
  (let* (((values value-str name)
          (values (sxml:attr sxml 'value) (sxml:attr sxml 'name)))
         (value (libc-strtol (string->bytes value-str))))
    (cons name `(define ,(string->symbol (translate-enum-name name)) ,value))))
(define (parse-enums sxml)
  (assert! (eq? (sxml:element-name sxml) 'enums))
  (let ((group (sxml:attr sxml 'group))
        (enums (filter (lambda (x) (and (sxml:element? x)
                                        (eq? (sxml:element-name x) 'enum)))
                       (sxml:content sxml))))
    (map parse-enum enums)))
(define (parse-command sxml)
  (assert! (eq? (sxml:element-name sxml) 'command))
  (let ((elements (sxml:elements sxml)))
    (if (not (eq? (caar elements) 'proto)) #f
        (let*
            ((proto (sxml:content (car elements)))
             ((values params extra)
              (partition (lambda (x) (eq? (car x) 'param)) (cdr elements)))
             (aliases
              (filter-map (lambda (x)
                            (if (eq? (car x) 'alias)
                                (sxml:attr x 'name) #f)) extra))
             (type-names (filter-map (lambda (x)
                                (aif (assq 'ptype (sxml:elements x))
                                     (cadr it) #f)) params))
             (types (map (lambda (x)
                           (format-symbol "_~a" x)) type-names))
             (rettype-name (aif (sloppy-assq 'ptype proto)
                                (cadr it) "void"))
             (gl-fn-name (cadr (sloppy-assq 'name proto)))
             (rettype (format-symbol "_~a" (string-trim rettype-name)))
             (fn-name (un-camel-case gl-fn-name)))
          ;;this should work even when types is null
          (cons gl-fn-name
                `(begin
                   (define ,(string->symbol fn-name)
                     ;;it's tempting to call this and insert the result, but that wouldn't work
                     (get-ffi-obj ,gl-fn-name gl-lib
                                  (_fun ,@types -> ,rettype)))
                   ,@(map (lambda (x)
                            `(define-syntax ,(string->symbol (un-camel-case x))
                               (make-rename-transformer (syntax ,fn-name)))) aliases)))))))
  ;;         (match proto
  ;;           ((
  ;; (match (sxml:elements sxml)
  ;;   ((list-rest (list (or 'proto
  ;;                         (list-rest 'proto _))
  ;;                     (or rettype-name
  ;;                         (list 'ptype rettype-name))
  ;;                     (list 'name gl-fn-name)) rest)
  ;;    (let* (((values params extra)
  ;;            (partition (lambda (x) (eq? (car x) 'param)) rest))
;;           (aliases
  ;;            (filter-map (lambda (x)
  ;;                          (if (eq? (car x) 'alias)
  ;;                              (sxml:attr x 'name) #f)) extra))
  ;;           (type-names (map (lambda (x)
  ;;                              (cadr (assq 'ptype (sxml:elements x)))) params))
  ;;           (types (map (lambda (x)
  ;;                         (format-symbol "_~a" x)) type-names))
  ;;           (rettype (format-symbol "_~a" (string-trim rettype-name)))
  ;;           (fn-name (un-camel-case gl-fn-name)))
(define (parse-commands sxml)
  (assert! (eq? (sxml:element-name sxml) 'commands))
  (let ((commands (filter (lambda (x) (and (sxml:element? x)
                                           (eq? (sxml:element-name x) 'command)))
                          (sxml:content sxml))))
    (map parse-command commands)))
(define (collect-features types enums commands)
  (let* ((ht (make-hash))
         (mapfn (lambda (x)
                  (when (list? x)
                    (hash-set! ht (car x) (cdr x))))))
    (map mapfn types)
    (map mapfn enums)
    (map mapfn commands)
    ht))
(define (parse-feature sxml ht)
  (assert! (or (eq? (sxml:element-name sxml) 'feature)
               (eq? (sxml:element-name sxml) 'extension)))
  (let ((parse-require
        (lambda (x)
          (map (lambda (y)
                 (sxml:attr y 'name)) (sxml:elements x)))))
    (cons (sxml:attr sxml 'name)
          (append
           (map parse-require
                (filter (lambda (x) (and (sxml:element? x)
                                         (eq? (sxml:element-name x) 'require)))
                        (sxml:content sxml)))))))
(define (parse-extensions sxml ht)
  (append
   (map (lambda (x) (parse-feature x ht))
        (filter (lambda (x) (and (sxml:element? x)
                                 (eq? (sxml:element-name x) 'extension)))
                (sxml:content sxml)))))
(define (parse-registry sxml)
  (assert! (eq? (car sxml) '*TOP*))
  (let ((registry (last sxml))
        (types null) (enums null) (commands null)
        (features-ht #f) (features null))
    (for-each
     (lambda (elt)
       (case (sxml:element-name elt)
         ((types) (set! types (cons (parse-types elt) types)))
         ((enums) (set! enums (cons (parse-enums elt) enums)))
         ((commands) (set! commands (cons (parse-commands elt) commands)))))
       registry)
    (list (apply append types)
          (apply append enums)
          (apply append commands))))
(define (read-gl-xml (xml-file "gl.xml"))
  (xml->sxml (open-file xml-file 'read)))
         ;; ((feature) (when (not features-ht)
         ;;              (set! features-ht (collect-features types enums commands)))
         ;;  (push! (parse-feature elt features-ht) features))
         ;; ((extensions) (append features (parse-extensions elt features-ht))))) registry)
;    (list types enums commands features)))


;;;Next are a set of enum groups which we ignore spince the same information
;;is provided with the actual definitions of the enums
;; (define (parse-enum enum)
;;   (let (((values value name . rest) (list->values (element-attributes enum))))
;;     (assert! (eq? 'value (attribute-name value)))
;;     (assert! (eq? 'name (attribute-name name)))
;;     `(define ,(string->symbol (attribute-value name))
;;        ,(libc-strtol (string->bytes (attribute-value value))))))
;; (define (parse-enums enums)
;;   (let ((group (find (lambda (x) (eq? (attribute-name x) 'group)))
;;                (element-attributes enums)))
;;     (map
;;      (lambda (enum) (enum (element-content enums))
;;              (let ((name (element-attributes enum)
;;              `(define ,(string->symbol
;;                         (concat "GL-"
;;                                 (string-replace
;;                                  (string-downcase (string-slice
