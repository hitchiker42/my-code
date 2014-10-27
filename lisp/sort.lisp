;;given an array of items, each with an integer key
;;ranging from min to max, key is a function to get the integer
;;key from an array element
(defun counting-sort (arr max key)
  (let* ((len (length arr))
        (count (make-array max :initial-element 0))
        (output (make-array len))
         temp (total 0))
    (print "HERE")
    (dotimes (i len)
      (incf (aref count (funcall key (aref arr i)))))
    (print "HERE")
    (dotimes (i max)
      (setf temp (aref count i)
            (aref count i) total
            total (+ total temp)))
    (loop for elem across arr doing
         (progn
           (let ((ind (funcall key elem)))
             (setf (aref output (aref count ind)) elem)
             (incf (aref count ind)))))
    output))

  
