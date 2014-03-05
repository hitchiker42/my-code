(ns typescript
  (:use [clojure.string :as string :only (join)]))
(defn strcat [& args]
  (join args))
(def identifier
  #"[$_\p{L}][$_\p{L}\p{N}]*")
(def identifier-path
  (re-pattern (strcat identifier "(\\." identifier ")*")))
(def single-line-comment
  #"//.*")
(def multi-line-comment
  #"/\*[\p{M}\P{M}]*?\*/")
(def predefined-type
  #"any|number|boolean|string|void")
(def type-parameter)
(def type-parameter-list)
