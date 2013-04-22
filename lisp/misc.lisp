(defpackage misc
  (:use :cl)
  (:export :seq
           :interleave))
(defun seq (&key (start 0) (end 10) (step 1))
  "return a list of integers from start to end by step"
  (loop for i from start upto end by step collecting i))
(defun interleave (list1 list2)
  (let ((temp ())
        (a (copy-list list1))
        (b (copy-list list2)))
    (loop until (equal nil a) doing (push (pop a) temp) (push (pop b) temp))
    (reverse temp)))
