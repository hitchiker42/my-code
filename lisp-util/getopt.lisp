(ql:quickload :cl-ppcre)
(defpackage getopt
  (:export getopt)
  (:use :cl :util :cl-ppcre))
(in-package getopt)
;;add the ability to make long/short lists
;;so something like ('(:size :length) :s 1 size)
;;would work
(defvar opt-re "[-+](\\w+|\"[^\"]+\")(?:=(\\w+|\"[^\"]+\"))?")
(defun parse-opt-spec (opt-spec ht)
  (multiple-value-bind (options arg fun) (values-list opt-spec)
    (let* ((option-vals
            (mapcar
             (lambda (opt)
               (etypecase opt
                 (keyword
                  (string-downcase (symbol-name opt)))
                 (string opt)
                 (character (string opt))))
             options))
          (arg-val
           (etypecase arg
             (number arg)
             (keyword
              (case arg
                (:required 1)
                (:optional 2)
                (t 0)))
             (boolean (if arg 1 0))))
           (fun-val
            (etypecase fun
              (function fun)
              (symbol
               (case arg-val
                 (0 (lambda () (setq fun t)))
                 (1 (lambda (x) (setq fun x)))
                 (2 (lambda (&optional (x t)) (setq fun x)))))
              (null (constantly nil)))))
      (dolist (opt option-vals)
        (setf (gethash opt ht) (list arg-val fun-val))))))

(defun getopt (args &rest options)
  (let ((opt-table (make-hash-table))
        (args (etypecase args)
                (list args)
                (string (string-split args))
                (sequence (concatenate 'list args)))
        remaning-args arg match-data)
    (dolist (opt options) (parse-opt-spec opt opt-table)
            (while (not (null args))
              (setq arg (pop args))
              (if (setq match-data (match-string opt-re arg))
                  (multiple-value-bind (has-arg fun)
                      (gethash (string-match 1 match-data) opt-table)
                    (case has-arg
                      (0 (funcall fun))
                      (1 (if (string-match 2 match-data)
                             (funcall fun (string-match 2 match-data))
                             (funcall fun (pop args))))
                      (2 (funcall fun (string-match 2 match-data)))))
                  (push arg remaning-args))))
    (reverse remaning-args)))
