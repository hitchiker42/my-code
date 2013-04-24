;;need to make package for calculus and import it here
(use-package :calculus)
(defmacro array-len (y) `(array-total-size ,y))
(defun u-init (y) 
  (declare (type float y))
  (* -12 (expt (/ 1 (cosh y)) 2)))

(defun u-discrete (u h)
"calculate spacial deritives of u using finite differences"
  (declare (type (array float) u)
           (type float h))
  (let ((len (array-len u)))
    (make-array len :initial-contents
                (loop for i upto (- len 1)
                   collecting (+ (5pt-stencil i 3 u h) (* (- 6) (aref u i)
                                                          (5pt-stencil i 1 u h)))))))

(defun update (u time dt dx)
  (declare (type (array float) u)
           (type float time dt dx))
  (flet ((ustep (x) (u-discrete x dx)))
    (rk4-seq #'ustep dt u)))
(defun kdv-vector-sum (x y scale)
  (make-array (array-total-size x) :initial-contents
              (loop for i across x for j across y collecting (+ i (* j scale)))))
(defun rk4-kdv (u dt dx)
  (let* ((len (array-len u))
         (k-1 (u-discrete u dx))
         (k-2 (u-discrete (kdv-vector-sum u k-1 (* 0.5 dt)) dx))
         (k-3 (u-discrete (kdv-vector-sum u k-2 (* 0.5 dt)) dx))
         (k-4 (u-discrete (kdv-vector-sum u k-1 dt) dx)))
    (make-array len :initial-contents (loop for i-1 across k-1
                                           for i-2 across k-2 for i-3 across k-3
                                           for i-4 across k-4 collecting 
                                           (* (/ dt 6) (+ i-1 (* 2 i-2) (* 2 i-3) i-4))))))
(defun main ()
  "run kdv simulation"
  (let* ((dx (/ pi 8))
         (dt (expt dx 3)) (tmax 10.0)
         (len (floor (/ (* 20 pi) dx)))
         (x (map '(vector float) #'float (seq :start (* dx (/ len -2)) :end (* dx (/ len 2)) :step dx)))
         (u (make-array (+ len 1)
                        :initial-contents (loop for i across x collecting (u-init i)))))
    (macrolet ((update-u () `(update u time dt dx)))
      (with-open-file (temp (parse-namestring "temp")
                            :if-does-not-exist :create
                            :if-exists :overwrite  :direction :output)
        (loop for time from 0.0 upto tmax by dt doing 
             (progn 
               ;;write data to file, in form of u:value of u[i]
               ;;                               x:value of i    
               (format temp "At time ~e~%~{u:~e x:~e~%~}"
                       time (interleave (coerce u 'list) (coerce x 'list)))
               ;;update u
               (setf u (vector-sum u (rk4-kdv u dt dx)))
               (setf time (+ time dt))))))))

#|(defun u-discrete-i (u h i)
  (declare (type (array float) u)
           (type float h)
           (type fixnum i))
  (+ (5pt-stencil i 3 u h)
     (* (- 6) (aref u i) 
        (5pt-stencil i 1 u h))))
#|  (let* ((len (array-len u))
        (u-x (make-array len)))
    (dotimes (i len)
      (setf (svref u-x i) (+ (5pt-stencil i 3 u h)
                             (* (- 6) (aref u i) 
                                (5pt-stencil i 1 u h)))))
    u-x))|#|#
