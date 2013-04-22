
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
