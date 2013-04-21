(ns core.clojure)
;; Shooting Method
;; For a boundary value problem of a second-order [[ordinary differential equation]], the method is stated as follows.
;; Let

;; :<math> y''(t) = f(t, y(t), y'(t)), \quad y(t_0) = y_0, \quad y(t_1) = y_1 </math>

;; be the boundary value problem.
;; Let ''y''(''t''; ''a'') denote the solution of the initial value problem

;; :<math> y''(t) = f(t, y(t), y'(t)), \quad y(t_0) = y_0, \quad y'(t_0) = a </math>

;; Define the function ''F''(''a'') as the difference between ''y''(''t''<sub>1</sub>; ''a'') and the specified boundary value ''y''<sub>1</sub>.

;; :<math> F(a) = y(t_1; a) - y_1 \,</math>

;; If the boundary value problem has a solution, then ''F'' has a [[root of a function|root]],
;; and that root is just the value of ''y''<nowiki>'</nowiki>(''t''<sub>0</sub>) which yields a solution ''y''(''t'') of the boundary value problem.

;;The secant method is defined by the [[recurrence relation]]

;;:<math>x_n=x_{n-1}-f(x_{n-1})\frac{x_{n-1}-x_{n-2}}{f(x_{n-1})-f(x_{n-2})}</math>

;;As can be seen from the recurrence relation, the secant method requires two initial values, ''x''<sub>0</sub> and ''x''<sub>1</sub>, which should ideally be chosen to lie close to the root.

;;newton's method ;;x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)}
(defn newton-fn [x_0 f df-dx err]
  (let [next (fn next [x] (let [x_n
                (- x (/ (apply f [x]) (apply df-dx [x])))]
                (if (< err (- x x_n)) (x_n)
                    (recur (next [x_n])))))])
    (apply next [x_0]))
