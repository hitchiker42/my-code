(ql:quickload :cl-ppcre)
(defpackage util
  (:export system concat el-if puthash aif
           run-shell-command string-split
           sequencep while until
           match-string string-match)
  (:use :COMMON-LISP :SB-EXT :CL-PPCRE))
(in-package :util)
(defun system (program &rest args)
  (let ((output-stream (make-string-output-stream)))
    (run-program program args :output output-stream
                        :search t :error :output :wait t)
    (get-output-stream-string output-stream)))
(defun run-shell-command (command)
  (system "/bin/bash" "-c" command))
(defun string-split (str)
  (split "\\s+" str))
(defun package-symbol-list (package &optional all)
  (let ((symbol-list nil))    
    (do-external-symbols (sym package)
      (push sym symbol-list))
    (sort symbol-list #'string<)))
(defmacro aif (test-form then-form &optional else-form)
  "evaluate test-form and bind the result to the variable `it`
then evaluate then-form or else-form according to the result of test-form"
  `(let ((it ,test-form))
         (if it ,then-form ,else-form)))
(defmacro el-if (test then &rest else)
  `(if ,test ,then (progn ,@else)))
(defun concat (&rest args)
  (declare 'inline)
  (apply #'concatenate 'string args))
(defun sequencep (seq) (typep seq 'sequence))
(defmacro puthash (key value table)
  `(setf (gethash ,key ,table) value))
(defmacro make-seq-convert-fun (from to)
  (let ((fun-name (intern (concat (symbol-name from) "->" (symbol-name to)))))
    `(defun ,fun-name (seq)
       (concatenate ',to seq))))
(make-seq-convert-fun array list)
(defmacro while (test &rest body)
  `(do () ((not ,test)) ,@body))
(defmacro until (test &rest body)
  `(do () (,test) ,@body))


;;(defmacro make-alias (symbol fun)
  
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
(let ((data (string-match "(a)|(b)" "a")))
  (match-string 2 data))
