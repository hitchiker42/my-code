;;;; pretty-literals.lisp - pretty hash table & vector literal syntax
;;;; inspired by and uses code from http://frank.kank.net/essays/hash.html

(in-package #:pretty-literals)

;; vector literal syntax using brackets
(set-macro-character #\[
  (lambda (str char)
    (declare (ignore char))
    (let ((*readtable* (copy-readtable *readtable* nil))
          (keep-going t))
      (set-macro-character #\] (lambda (stream char)
                                 (declare (ignore char) (ignore stream))
                                 (setf keep-going nil)))
      (let ((items (loop for value = (read str nil nil t)
                      while keep-going
                      collect value)))
        (coerce items 'vector)))))
;; hash-table literal syntax using braces
(set-macro-character #\{
  (lambda (str char)
    (declare (ignore char))
    (let ((*readtable* (copy-readtable *readtable* nil))
          (keep-going t))
      (set-macro-character #\} (lambda (stream char)
                                 (declare (ignore char) (ignore stream))
                                 (setf keep-going nil)))
      (let ((pairs (loop for key = (read str nil nil t)
                      while keep-going
                      for value = (read str nil nil t)
                      collect (list key value)))
            (retn (gensym)))
        `(let ((,retn (make-hash-table :test #'equal)))
           ,@(mapcar
              (lambda (pair)
                `(setf (gethash ,(car pair) ,retn) ,(cadr pair)))
              pairs)
           ,retn)))))
