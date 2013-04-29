(defpackage tucker.misc
  (:use :cl)
  (:export
   :seq
   :with-gensyms
   :once-only
   :vector->list
   :list->vector))
(defun seq (&key (start 0) (end 10) (step 1))
  "return a list of integers from start to end by step"
  (loop for i from start upto end by step collecting i))
(defun interleave (list1 list2)
  (let ((temp ())
        (a (copy-list list1))
        (b (copy-list list2)))
    (loop until (equal nil a) doing (push (pop a) temp) (push (pop b) temp))
    (reverse temp)))
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))
(defmacro vector->list (vector)
  `(coerce ,vector 'list))
(defmacro list->vector (list)
  `(coerce ,list 'vector))
