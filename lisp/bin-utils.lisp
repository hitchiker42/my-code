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
(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))
(defun mklist (x) (if (listp x) x (list x)))
(defun normalize-slot-spec (spec)
  (list (car spec) (mklist (cadr spec))))
(defmacro define-binary-class  (name slots)
  "create a defclass for name with given slots"
  (with-gensyms (typevar objectvar streamvar)
    `(progn
       (defclass ,name ()
         ,(mapcar #'slot->defclass-slot slots))
       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
         (let ((,objectvar (make-instance ',name)))
           (with-slots ,(mapcar #'car slots) ,objectvar
             ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
           ,objectvar)))))
(defgeneric read-value (type stream &key)
  (:Documentation "Read a value of the given type from the stream"))
(defgeneric write-value (type stream value &key)
  (:Documentation "Write a value as the given type to the stream"))
