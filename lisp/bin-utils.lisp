(defpackage :tucker.binutils
  (:use :cl :tucker.misc)
  (:export
   :define-binary-class
   :define-tagged-binary-class
   :define-binary-type
   :read-value
   :write-value
   :*in-progress-objects*
   :parent-of-type
   :current-binary-object
   :+null+))
(defun as-keyword (sym)
  "return keyword corrsponding to given symbol"
  (intern (string sym) :keyword))
(defun slot->defclass-slot (spec)
  "create a class slot with name (car spec)"
  (let ((name (car spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))
(defmacro define-binary-class  (name slots)
  "create a defclass for name with given slots"
  `(defclass ,name ()
     ,(mapcar #'slot->defclass-slot slots)))
