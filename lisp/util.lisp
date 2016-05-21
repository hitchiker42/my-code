(require :uiop)
(uiop:define-package :util
    (:mix :cl :uiop :iterate :alexandria)
  ;;I can't use this because of name conflicts
;;  (:reexport :uiop :iterate :alexandria)
  (:shadow :iota)
  (:export :named-let))
(in-package :util)
(defmacro named-let (name bindings &rest body)
  `(labels ((,name ,(mapcar #'car bindings)
              ,@body))
     (,name ,@(mapcar #'cadr bindings))))
(defun re-export (&rest packages)
  (dolist (package packages)
    (do-external-symbols (sym package)
      (export sym))))
;;Change iota so that the arguments make more sense
(defun iota (start &optional stop step)
  (cond
    (step
     (alexandria:iota stop :start start :step step))
    (stop
     (alexandria:iota stop :start start))
    (t (alexandria:iota start))))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (with-package-iterator (get-sym *package* :inherited)
    (iter (for (values flag sym) = (get-sym))
          (if (not flag) (finish)
              (export sym)))))
              
