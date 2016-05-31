#lang racket/base
(require "../util.rkt" "../io.rkt" "ffi.rkt")
(require (for-syntax racket/base racket/syntax))
(require sxml)

(define (xml->sxml input (namespace-prefixes null))
  (let ((port (if (string? input) (open-input-string input) input)))
    (ssax:xml->sxml port namespace-prefixes)))
(define xml-test-types #<<  EOF
  <types>
  <type requires="stddef">typedef ptrdiff_t <name>GLsizeiptrARB</name>;</type>
  <type requires="inttypes">typedef int64_t <name>GLint64EXT</name>;</type>
  <type requires="inttypes">typedef uint64_t <name>GLuint64EXT</name>;</type>
  </types>
  EOF
  )
(define sxml-test-type (xml->sxml xml-test-type))
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
        (string->ctype $3 (equal? $1 "unsigned")))))
(define (destruct-sxml sxml)
  (values (sxml:name sxml) (sxml:attr-list sxml) (sxml:content sxml)))
(define (parse-type sxml)
  (unless (equal? (sxml:attr sxml 'requires) "khrplatform")
    (match (sxml:content sxml)
      ((list typedef (list 'name typename) ";")
       (let ((ctype (parse-typedef (string-trim typedef))))
         (if (not ctype) null
             `(define ,(format-symbol "_~a" typename) ,ctype))))
      (_ null))))
(define (translate-enum-name str)
  (concat "GL-" (string-replace (string-downcase (string-slice str 3)) "_" "-")))
(define (parse-enum sxml)
  (let* (((values value-str name)
          (values (sxml:attr sxml 'value) (sxml:attr sxml 'name)))
         (value (libc-strtol (string->bytes value-str))))
    `(define ,(string->symbol (translate-enum-name name)) ,value)))
(define (parse-enums sxml)
  (let ((group (sxml:attr sxml 'group))
        (enums (sxml:content sxml)))
    (map parse-enum enums)))
(define (parse-command sxml)
  (match (sxml:content sxml)
    ((list-rest 'proto rettype (list 'name fn-name) params))))
     
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
