(defpackage :calculus
  (:export :5pt-stencil
           :rk4
           :rk4-seq
           :vector-sum))
(defun dot-prod (x y)
;;dot product of x and y, store in z
  (declare (type vector x y))
  (make-array (array-total-size x) :initial-contents
              (loop for i across x for j across y collecting (* i j))))
(defun vector-sum (x y)
  (make-array (array-total-size x) :initial-contents
              (loop for i across x for j across y collecting (+ i j))))
(defun 5pt-stencil (i order x h)
;;calculate the derivitive of a function approximated by x at point i
;;through the use of a 5pt stencil finite differeren
  (declare (type fixnum i order)
           (type (vector float) x)
           (type float h))
  (let* ((len (array-total-size x))
      (xi-2 (if (> 0 (- i 2)) (aref x (+ i (- len 2)))
                  (aref x (- i 2))))
         (xi-1 (if (eql 0 i) (aref x (- len 1))
                   (aref x (- i 1))))
          (xi (aref x i))
         (xi+1 (if (>= (- len 1) i) (aref x 0)
                   (aref x (+ i 1))))
         (xi+2  (if (>= 2 (- len i)) (aref x (- (- len i) 1))
          (aref x (+ i 2)))))
    (cond ((eq order 1)(/ (+ (- xi-2) (- xi-1) xi+1 xi+2) (* 6 h));1st deritive
           (eq order 2)(/ (+ (- xi-2) (* 16 xi-1) (* -30 xi)
                             (* 16 xi+1) (- xi+2)) (* 12 (expt h 2)));2nd "      "
           (eq order 3)(/ (+ xi-2 (* -2 xi-1) (* 2 xi+1) (- xi+2)) (* 2 (expt h 3)));3rd "      "
           (eq order 4)();4th "      "
           t xi))))
(defun rk4 (f i dt x time); &rest vals maybe
  (declare (type function f)
           (type fixnum i)
           (type float dt x time)
           ;;maybe type vals float-list
           )
  (let ((k_1) (k_2) (k_3) (k_4))
    (setf k_1 (funcall f time x));perhaps apply f args
    (setf k_2 (funcall f (+ Time (* 0.5 dt)) (+ x (* 0.5 dt k_1))))
    (setf k_3 (funcall f (+ time (* 0.5 dt)) (+ x (* 0.5 dt k_2))))
    (setf k_4 (funcall f (+ time dt) (+ x (* dt k_3))))
    (+ x (* dt (/ 1 6) (+ k_1 k_2 k_3 k_4)))))
(defun rk4-seq (f dt x)
"time independent rk4 across a vector x"
  (let* ((len (- (array-total-size x) 1))
         (u (copy-seq x))
         ;(i (seq :end len))
         (k_1 (funcall f u));(map 'vector f u i))
         (k_2 ;(map 'vector f
          (funcall f (vector-sum u (map 'vector (lambda (x) (* 0.5 dt x)) k_1))))
         (k_3 ;(map 'vector f 
          (funcall f (vector-sum u (map 'vector (lambda (x) (* 0.5 dt x)) k_2))))
         (k_4 ;(map 'vector f 
          (funcall f (vector-sum u (map 'vector (lambda (x) (* dt x)) k_3)))))
    (map 'vector (lambda (x) (* dt (/ 1 6) x))
         (vector-sum (vector-sum k_1 k_2) (vector-sum k_3 k_4)))))
;(defun scant-meth (x1 x2 f err)
;  (declare (float x1 x2 err)
;           (function f))
;  (let ((f-x1 (funcall f x1)) (f-x2 (funcall f x2)))))

;(defun scant-meth (x1 x2 f err)
;  (declare (type float x1 x2 err)
;           (type function f))
;  (let ((f-x1 (funcall f x1)) (f-x2 (funcall f x2)))))
;;It actually seems to work, woo!
(defun newton-fxn (x_0 f df-dx err)
  (declare (type function f df-dx)
          (type float x_0 err))
  (labels ((next (x) (- x (/ (funcall f x) (funcall df-dx x))))
           (check (x y) (if (> err (abs (- x y))) y
                              (check y (next y)))))
    (check x_0 (next x_0))))

;; :<math> y''(t) = f(t, y(t), y'(t)), \quad y(t_0) = y_0, \quad y(t_1) = y_1 </math>
;; be the boundary value problem.
;; Let ''y''(''t''; ''a'') denote the solution of the initial value problem
;; :<math> y''(t) = f(t, y(t), y'(t)), \quad y(t_0) = y_0, \quad y'(t_0) = a </math>
;; Define the function ''F''(''a'') as the difference between ''y''(''t''<sub>1</sub>; ''a'') and the specified boundary value ''y''<sub>1</sub>.
;; :<math> F(a) = y(t_1; a) - y_1 \,</math>
;;in laymans terms let F(x) = y(from t=t_1 to t=a) - y(at t_1)
;;(defun shoot-fxn (
