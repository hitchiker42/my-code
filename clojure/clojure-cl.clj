;;Just as a warning, this is all pretty inefficent as I have to work around
;;the fact that there's no such thing as a cons cell in clojure
;;simple macros equivlant to cl special forms/macros
(ns cl
  (:refer-clojure
   :rename {cons core-cons
            list core-list
            list* core-list*
            reduce core-reduce
            nth core-nth
            last core-last
            pop! core-pop!
            take core-take
            assoc core-assoc
            drop core-drop}))
(defmacro progn [& rest] `(do ~@rest))
;;needs to be extended to support docstrings/metadata
(defmacro defun [name args & body]
  `(defn ~name ~(vec args) ~@body))
(defun eq [x y] (identical? x y))
(defmacro core-shadow [ns sym] ;works with symbols
  `(intern '~ns (symbol ~(str "core-" (name sym)))
           (eval ~(symbol "clojure.core" (name sym)))))
(def cl-format clojure.pprint/cl-format)
(defmacro cl-defmacro [name args & body]
  `(defmacro ~name ~(vec args) ~@body))
(defmacro lambda [args & body]
  `(fn ~(vec args) ~@body))
;;it baffles me why this isn't the default
(defmacro cl-if [test then & else]
  `(if ~test
    ~then
    (do ~@else)))
(defn format-string [format & args]
  (with-out-str
    (apply cl-format true format args)))
(defmacro raise [err] `(throw (Exception. ~(str err))))
(defmacro raise-fmt [format & args]
  `(throw (Exception. (format-string ~format ~@args))))
;;Consesn
(declare cons? car cdr)
;;minimal set of methods to allow access to mutable fields
;;then use functions for everything else
(definterface Internal_Cons
  (internal_car [])
  (internal_cdr [])
  (internal_set_car [x])
  (internal_set_cdr [x]))
(deftype Cons [^{:unsynchronized-mutable true} car
               ^{:unsynchronized-mutable true} cdr]
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
                   (if (nil? cell)
                     nil
                     (raise-fmt "Type error in car recieved ~a" (type cell)))))
(defn cdr [cell] (if (cons? cell)
                   (.internal_cdr cell)
                   (if (nil? cell)
                     nil
                     (raise-fmt "Type error in car recieved ~a" (type cell)))))
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
(defn cdar [cell]
  (cdr (car cell)))
(defn cddr [cell]
  (cdr (cdr cell)))
(defn caaar [cell]
  (car (car (car cell))))
(defn cdaar [cell]
  (cdr (car (car cell))))
(defn caadr [cell]
  (car (car (cdr cell))))
(defn cdadr [cell]
  (cdr (car (cdr cell))))
(defn caddr [cell]
  (car (cdr (cdr cell))))
(defn cdddr [cell]
  (cdr (cdr (cdr cell))))
(defn cddar [cell]
  (cdr (cdr (car cell))))
(defn cadar [cell]
  (car (cdr (car cell))))
(defn set-car! [cell x] (if (cons? cell)
                          (.internal_set_car cell x)
                          (raise-fmt "Type error in set-car! recieved ~a" (type cell))))
(defn set-cdr! [cell x] (if (cons? cell)
                          (.internal_set_cdr cell x)
                          (raise-fmt "Type error in set-cdr! recieved ~a" (type cell))))
(defn push! [x cell]
  (let [temp (cons (car cell) (cdr cell))]
    (set-cdr! cell temp)
    (set-car! cell x))
  cell)
(defn pop! [cell]
  (let [temp (car cell)]
    (set-car! cell (car (cdr cell)))
    (set-cdr! cell (cdr (cdr cell)))
    temp))
(defn rev [cell]
  (let [acc (fn [ls sl]
              (if (cons? ls)
                (recur (cdr ls) (cons (car ls) sl))
              (if ls
                (cons ls sl)
                sl)))]
    (acc cell nil)))
(defn rev* [cell last]
  (let [acc (fn [ls sl]
              (if (cons? ls)
                (recur (cdr ls) (cons (car ls) sl))
              (if ls
                (cons ls sl)
                sl)))]
    (acc cell last)))
(defn clj-list-to-cons-rev [ls]
  (let [acc (fn [ls cell]
              (if (not-empty ls)
                (recur (rest ls) (cons (first ls) cell))
                (rev cell)))]
    (acc ls nil)))
;useful for clojure code
;(defn listp [ls]
;  (and (list? ls) (not-empty ls)))
(defun listp (ls)
  (or (cons? ls) (nil? ls)))
(defn clj-list-to-cons [ls]
  (let [head (cons (first ls) nil)
        acc (fn [ls cell]
              (when (not-empty ls)
                (set-cdr! cell (cons (first ls) nil))
                (recur (rest ls) (cdr cell))))]
    (progn (acc (rest ls) head))
    head))
(defn clj-list-to-cons-rev* [ls]
  (let [acc (fn [ls cell]
              (if (not-empty (rest ls))
                (recur (rest ls) (cons (first ls) cell))
                (rev* cell (first ls))))]
    (acc ls nil)))
(defn clj-list-to-cons* [ls]
  (let [head (cons (first ls) nil)
        acc (fn [ls cell]
              (if (not-empty (rest ls))
                (progn (set-cdr! cell (cons (first ls) nil))
                       (recur (rest ls) (cdr cell)))
                (set-cdr! cell (first ls))))]
    (progn (acc (rest ls) head))
    head))
(defn list [& rest]
  (clj-list-to-cons rest))
(defn iota
  ([end]
     (if [< 0 end]
       (iota 0 end 1)
       (iota 0 end -1)))
  ([start end]
     (if (< start end)
       (iota start end 1)
       (iota start end -1)))
  ([start end step]
     (if (> 0 (* (- end start) step))
       nil
       (let [head (cons start nil)
             cmp (if (> start end) < >)
             acc (fn [i ls]
                   (if (cmp i end)
                     ls
                     (progn (set-cdr! ls (cons i nil))
                            (recur (+ i step) (cdr ls)))))]
         (acc (+ start step) head)
         head))))
(defn list* [& rest]
  (clj-list-to-cons* rest))
(defn last
  ([cell]
     (if (cons? (cdr cell))
       (recur (cdr cell))
       cell)))
(defn copy-cons-rev [cell]
  (let [acc (fn [old new]
              (if (cons? old)
                (recur (cdr old) (cons (car old) new))
                (rev* new old)))]
    (acc (cdr cell) (car cell))))
(defn copy-cons
  ([cell]
     (let [new-cell (cons (car cell) nil)
           acc (fn [old new]
                 (if (cons? old)
                   (progn (set-cdr! new (cons (car old) nil))
                          (recur (cdr old) (cdr new)))
                   (set-cdr! new old)))]
       (acc (cdr cell) new-cell)
       new-cell)))
(defmacro cons-acc
  ([ls f] `(cons-acc ~ls ~f (fn [_#] nil)))
  ([ls f g]
     `(let [head# (~f (car ~ls))
            acc# (fn [x# y#]
                  (if (cons? x#)
                    (progn (set-cdr! y# (~f (car x#)))
                           (recur (cdr x#) (cdr y#)))
                    (set-cdr! y# (~g x#))))]
        (acc# (cdr ~ls) head#)
        head#)))
;; (defn copy-tree [cell]
;;   (let [new-tree (cons (copy-tree (car cell)) nil)
;;         acc (fn [old new]
;;               (if (cons? old)
;;                 (if (cons? (car old))
;;                   (progn (set-cdr! new (cons (copy-tree (car old)) nil))
;;                          (recur (cdr old) (cdr new)))
;;                   (progn (set-cdr! new (cons (car old) nil))
;;                          (recur (cdr old) (cdr new))))
;;                 (set-cdr! new old)))]
;;     (acc (cdr cell) new-tree)
;;     new-tree))
(defn append [x y]
  (let [z (copy-cons x)]
    (set-cdr! (last z) y)
    z))
(defn append-rest-internal [args]
  (let [acc (fn [args acc]
              (if args
                (recur (cdr args) (append acc (car args)))
                acc))]
    (acc (cdr args) (car args))))
(defn append-rest [& rest]
  (append-rest-internal (clj-list-to-cons rest)))
(defn append! [x y]
  (set-cdr! (last x) y)
  x)
(defn mapcar [f ls]
  (let [head (cons (f (car ls)) nil)
        acc (fn [old new]
              (if (cons? old)
                (progn (set-cdr! new (cons (f (car old)) nil))
                       (recur (cdr old) (cdr new)))))]
    (acc (cdr ls) head)
    head))
(defn mapcar* [f ls]
  (let [head (cons (f (car ls)) nil)
        acc (fn [old new]
              (if (cons? old)
                (progn (set-cdr! new (cons (f (car old)) nil))
                       (recur (cdr old) (cdr new)))
                (set-cdr! new (f old))))]
    (acc (cdr ls) head)
    head))
;(defn mapcan [f ls];concatenate results of f applied to cars of ls
;  (let [head (f (car ls))
;        acc (fn [ls cat]
;              (if (cons? ls)
;                (progn (set-cdr! new (cons (f (car old)) nil))
;                       (recur (cdr old) (cdr new)))
(defn reduce
  ([f ls] (reduce f (cdr ls) (car ls)))
  ([f ls start]
     (let [acc (fn [ls acc]
                 (if (cons? ls)
                   (recur (cdr ls) (f (car ls) acc))
                 acc))]
       (acc ls start))))
(defn reduce*
  ([f ls] (reduce f (cdr ls) (car ls)))
  ([f ls start]
     (let [acc (fn [ls acc]
                 (if (cons? ls)
                   (recur (cdr ls) (f (car ls) acc))
                 (f ls acc)))]
       (acc ls start))))
(defn length [ls]
  (let [acc (fn [ls len]
              (if (cons? ls)
                (recur (cdr ls) (inc len))
                (if ls
                  (raise-fmt "~a is not a list" ls)
                  len)))]
    (acc ls 0)))
(defn nth [n ls]
  (if (zero? n)
    (car ls)
    (recur (dec n) (cdr ls))))
(defn take [ls n]
  (let [head (cons (car ls) nil)
        acc (fn [ls new n]
              (if (and (> 0 n) (cons? ls))
                (progn (set-cdr! new (cons (car ls) nil))
                       (recur (cdr ls) (cdr new) (dec n)))
                (set-cdr! new nil)))]
    (acc (cdr ls) head n)
    head))
(defn drop [ls n]
  (if (and (cons? ls) (> 0 n))
    (recur (cdr ls) (dec n))
    ls))
(defn assoc
  ([elt alist]
     (assoc elt alist eq))
  ([elt alist test]
     (loop [alist alist]
       (if (not (cons? alist))
         nil
         (if (not (cons? (car alist)))
           (raise-fmt "~a is not a list" alist)
           (if (test (caar alist) elt)
             (car alist)
             (recur (cdr alist))))))))

(defmacro as-list [l]
  (if (list? l) l (clojure.core/list l)))
;;man this is excessive, but I really wanted to avoid sequences as munch as possible
;;I need this to implement cl-let
(defun flatten-1 (ls)
  (letfn [(acc1 [ls sl]
            (if (not-empty ls)
              (recur (rest ls) (conj sl (first ls)))
              sl))
          (acc2 [ls sl]
            (if (not-empty ls)
              (if (list? (first ls))
                (recur (rest ls) (acc2 (first ls) sl))
                (trampoline acc1 (rest ls) (conj sl (first ls))))
              sl))]
    (reverse (acc2 (rest ls) (as-list (first ls))))))
;;More common lisp equivlents
(defmacro cl-let [bindings & body]
  `(let [~@(flatten-1 bindings)]
     ~@body))
(defmacro flet [bindings & body]
  `(let [~@(flatten-1 bindings)]
     ~@body))
(defmacro prog1 [first & rest]
  `(let [ret# ~first]
     (do ~@rest)
     ret#))
(defmacro prog2 [fst snd & rest]
  `(do ~fst
       (let [ret# ~snd]
         (do ~@rest)
         ret#)))
(defun equal [x y];it's my equal, I want 1 to equal 1.0
  (cl-if (and (cons? x) (cons? y))
    (and (equal (car x) (car y))
         (recur (cdr x) (cdr y)))
    (if (and (number? x)(number? y))
      (==  x y))
    (= x y)))
(defmacro unwind-protect [bodyform & unwindforms]
  `(try
     ~bodyform
     (catch Throwable ~(gensym))
      (finally ~@unwindforms)))
(defmacro condition-case [var bodyform & handlers]
  `(try
     ~bodyform
     ~(mapcat (fn [cell] `(catch ~(first cell) ~var ~(rest cell))) handlers)))
(defmacro dolist-cons [[var list] & body]
  `(loop [~var (car list)]
     ~@body
     (if (
  
;(defmacro cons-destructuring-bind [args expr &rest body]
;  `(let ~(vec (mapcat (
;(defmacro do [vars test & body]
  ;add vars later
  ;do ((var init step)...) (end-test result...) body...)
;  (cl-let ((var (clj-list-to-cons vars)))
;          `(loop [~(car var) ~(cadr var)]
;             (cl-if ~end-test
;                    nil;add result later
;                    ~@body
;                    (recur (~(caddr step) ~(car var)))))))
