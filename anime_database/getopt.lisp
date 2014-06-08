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
               (cond
                 ((keywordp opt)
                  (string-downcase (symbol-name opt)))
                 ((stringp opt) opt)
                 ((characterp opt) (string opt))
                 (t (error 'type-error))))
             options))
          (arg-val
           (cond
             ((numberp arg) arg)
             ((keywordp arg)
              (case arg
                (:required 1)
                (:optional 2)
                (t 0)))
             ((eq t arg) 1)
             ((eq nil arg) 0)
             (t (error 'type-error))))
           (fun-val
            (cond
              ((functionp fun) fun)
              ((symbolp fun)
               (case arg-val
                 (0 (lambda () (setq fun t)))
                 (1 (lambda (x) (setq fun x)))
                 (2 (lambda (&optional (x t)) (setq fun x)))))
              ((null fun) (constantly nil))
              (t (error 'type-error)))))
      (dolist (opt option-vals)
        (setf (gethash opt ht) (list arg-val fun-val))))))

(defun getopt (args &rest options)
  (let ((opt-table (make-hash-table))
        (args (cond
                ((listp args) args)
                ((stringp args) (string-split args))
                ((sequencep args) (concatenate 'list args))
                (t (error 'type-error))))
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
          
                              
