(ql:quickload :cl-ppcre)
(defpackage util
  (:export :system :run-shell-command :package-symbol-list
           :string-split :concat :string-join
           :sequencep :while :until
           :puthash :el-if :string-match :match-string
           :aif :awhen :acond :awhile :it) 
  (:use :COMMON-LISP :SB-EXT :CL-PPCRE))
(in-package :util)
(declaim (inline concat))
;;system commands
(defun system (program &rest args)
  (let ((output-stream (make-string-output-stream)))
    (run-program program args :output output-stream
                        :search t :error :output :wait t)
    (get-output-stream-string output-stream)))
(defun run-shell-command (command)
  (system "/bin/bash" "-c" command))

;;list all the external symbols in a package
;;it's unecessarlly hard to bind a macro to a variable
(defun package-symbol-list (package &optional all)
  (let ((symbol-list nil)
        (pack (find-package package)))
    (if all
        (do-symbols (sym pack)
          (when (eql (symbol-package sym) pack)
            (push sym symbol-list)))
        (do-external-symbols (sym pack) (push sym symbol-list)))
    (sort symbol-list #'string<)))
;;string utility functions
(defun string-split (str)
  (split "\\s+" str))
(defun concat (&rest args)
  (apply #'concatenate 'string args))
(defun string-join (sep &rest strings)
  ;;probably not the most efficent way to do this
  (reduce (lambda (x y) (concat x sep y)) strings))

(defun sequencep (seq) (typep seq 'sequence))
(defmacro make-seq-convert-fun (from to)
  (let ((fun-name (intern (concat (symbol-name from) "->" (symbol-name to)))))
    `(defun ,fun-name (seq)
       (concatenate ',to seq))))
(make-seq-convert-fun array list)

;;simple iteration macros
(defmacro while (test &rest body)
  `(do () ((not ,test)) ,@body))
(defmacro until (test &rest body)
  `(do () (,test) ,@body))

;;elisp compatablity
(defmacro puthash (key value table)
  `(setf (gethash ,key ,table) ,value))
(defmacro el-if (test then &rest else)
  `(if ,test ,then (progn ,@else)))
(defstruct match-data
  (string "" :type string)
  (num-groups 0 :type fixnum)
  (start nil :type (or fixnum nil))
  (end nil :type (or fixnum nil))
  (groups-start nil :type (or vector nil))
  (groups-end nil :type (or vector nil)))

(defun string-match (regexp string &optional (start 0))
  (multiple-value-bind (match-start match-end groups-start groups-end)
      (scan regexp string :start start)
    (if match-start
        (make-match-data :string string
                         :start match-start
                         :end match-end
                         :num-groups (length groups-start)
                         :groups-start groups-start
                         :groups-end groups-end)
        nil)))
(defun match-string (num match-data)
  (cond
    ((zerop num) (subseq (match-data-string match-data)
                         (match-data-start match-data)
                         (match-data-end match-data)))
    ((<= num (match-data-num-groups match-data))
     (let ((start (aref (match-data-groups-start match-data) (1- num)))
           (end  (aref (match-data-groups-end match-data) (1- num))))
       (if (and start end)
           (subseq (match-data-string match-data) start end)
           nil)))
    (t (error 'simple-error))))
;;Anaphoric macros, from onlisp
;;control flow/iteration macros which bind the results
;;of the test to the variable `it`, they kinda break referential
;;transparency, but in a controled and documented way
(defmacro aif (test-form then-form &optional else-form)
  "evaluate test-form and bind the result to the variable `it`
then evaluate then-form or else-form according to the result of test-form"
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym , (car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))
