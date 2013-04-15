(defpackage :kdv-calculus 
  (:export :5pt-stencil
           :rk4
           :secant-meth))
(defun dot-prod (x y z)
;;dot product of x and y, store in z
  (declare (type vector x y z))
  ;(let ((z (make-array (array-total-size x))))
  (dotimes (i (array-total-size x))
    (setf (svref z i) (* (svref x i) (svref y i)))))
(defun 5pt-stencil (i order x h)
;;calculate the derivitive of a function approximated by x at point i
;;through the use of a 5pt stencil finite differeren
  (declare (type fixnum i order)
           (type (vector double-float) x)
           (type float h))
  (let ((xi-2 (aref x (- i 2)))
         (xi-1 (aref x (- i 1)))
          (xi (aref x i))
         (xi+1 (aref x (+ i 1)))
         (xi+2 (aref x (+ i 2))))
    (cond ((eq order 1)(/ (+ (- xi-2) (- xi-1) xi+1 xi+2) (* 6 h));1st deritive
           (eq order 2)(/ (+ (- xi-2) (* 16 xi-1) (* -30 xi)
                             (16 * xi+1) (- xi+2)) (* 12 (expt h 2)));2nd "      "
           (eq order 3)();3rd "      "
           (eq order 4)();4th "      "
           t xi))))
(defun rk4 (f i h x time); &rest vals maybe
  (declare (type function f)
           (type fixnum i)
           (type float h x time)
           ;;maybe type vals float-list
           )
  (let ((k_1) (k_2) (k_3) (k_4))
    (setf k_1 (funcallf time x));perhaps apply f args
    (setf k_2 (funcall f (+ time (* 0.5 h)) (+ x (* 0.5 h k_1))))
    (setf k_3 (funcall f (+ time (* 0.5 h)) (+ x (* 0.5 h k_2))))
    (setf k_4 (funcall f (+ time h) (+ x (* h k_3))))
    (+ x (* h (/ 1 6) (+ k_1 k_2 k_3 k_4)))))
(defun scant-meth (x1 x2 f err)
  (declare (float x1 x2 err)
           (function f))
  (let ((f-x1 (funcall f x1)) (f-x2 (funcall f x2)))))
