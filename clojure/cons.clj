(declare cons? car cdr)
(defmacro raise [cond] `(throw (Exception ~(str cond))))
(definterface Internal_Cons
  (internal_car [])
  (internal_cdr [])
  (internal_set_car [x])
  (internal_set_cdr [x]))
(deftype Cons [^{:volatile-mutable true} car
               ^{:volatile-mutable true} cdr]
  Internal_Cons
  (internal_car [_] car)
  (internal_cdr [_] cdr)
  (internal_set_car [this x] (set! car x))
  (internal_set_cdr [this x] (set! cdr x))
  Object
  (toString [this]
    (let [acc (fn [cell s]
                 (if (cons? (.internal_cdr cell))
                   (recur (.internal_cdr cell) (str s (.internal_car cell) " "))
                   (if (.internal_cdr cell)
                     (str s (.internal_car cell) " . " (.internal_cdr cell) ")")
                     (str s (.internal_car cell) ")"))))]
      (acc this "("))))
(defn cons? [cell] (instance? Cons cell))
(defn cons [x y] (Cons. x y))
(defn car [cell] (if (cons? cell)
                   (.internal_car cell)
                   (raise "type error in car")))
(defn cdr [cell] (if (cons? cell)
                   (.internal_cdr cell)
                   (raise "type error in cdr")))
(defn safe-car [cell] (if (cons? cell)
                        (.internal_car cell)
                        nil))
(defn safe-cdr [cell] (if (cons? cell)
                        (.internal_cdr cell)
                        nil))
(defn caar [cell]
  (car (car cell)))
(defn cadr [cell]
  (car (cdr cell)))
(defn cddr [cell]
  (cdr (cdr cell)))
(defn set-car! [cell x] (if (cons? cell)
                          (.internal_set_car cell x)
                          (raise "type error in set-car!")))
(defn set-cdr! [cell x] (if (cons? cell)
                          (.internal_set_cdr cell x)
                          (raise "type error in set-cdr!")))
(defn push! [x cell]
  (let [temp (cons (car cell) (cdr cell))]
    (set-cdr! cell temp)
    (set-car! cell x))
  cell)
(defn pop! [cell]
  (let [temp (car cell)]
    (set-car! y (car (cdr y)))
    (set-cdr! y (cdr (cdr y)))
    temp))
;(defn list [& rest]
;  (let [acc (fn [
