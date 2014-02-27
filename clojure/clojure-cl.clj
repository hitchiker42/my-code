(defmacro progn [& rest] `(do ~@rest))
(defmacro prog1 [first & rest]
  `(let [ret# ~first]
     (do ~@rest)
     ret#))
(defmacro prog2 [fst snd & rest]
  `(do ~fst
       (let [ret# ~snd]
         (do ~@rest)
         ret#)))
;(defmacro do [vars test & body]
  ;do ((var init step)...) (end-test) body...)
;  `(let [acc# (fn [
