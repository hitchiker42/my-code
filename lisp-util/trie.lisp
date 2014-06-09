(defpackage trie
  (:export make-trie)
  (:use :cl :util))
(in-package trie)
;;recursive data structure, a trie is composed of nodes each of which
;;contains a string, an optional value for that node and a list of
;;children nodes.

;;children is a datatype of the form
;;children ::= chliren of (char . children)
;;           | child of node
;;i.e a character followed by a list of nodes begining with that character
;;indexed by starting character, or if there is only one left, then just that
;;character

;;for example given the strings "car" "cab" "com" "cabs"
;;with values 1, 2, 3,4
;;the trie would be
;;(:key "c" :value nil :children 
;;                       ((a . ((r . (:key "ar" :value 1 :children nil))
;;                              (b . (:key "ab" :value 2 :children
;;                                         (s . :key "s" :value 4 :children nil)))
;;                        (o . (:key "om" :value 3 :children nil))))
;;or the keys could be changed to contain only the characters that weren't alrady indexed
;;by the list of children (i.e for the above "ar"->"" "ab"->"" "s"->"" "om"->"m"
(defclass trie ()
    ((key
      :type string
      :accessor key
      :initform ""
      :initarg :key)
     (value
      :accessor value
      :initform nil
      :initarg :value
      :documentation "optional value at each node")
     (children
      :type list
      :accessor childern
      :initform nil
      :initarg :children
      :documentation "list of subtries")))
(defun make-empty-trie ()
  (make-instance 'trie))
(defun make-trie (&optional initial-nodes)
  "make a trie, takes an optional list of key-value pairs to be placed
in the newly created trie. The key value pairs are of the form (key . value) or
just key, in which case value is set to t."
  (let* ((key-value-generator (lambda (x) (if (consp x) x (cons x t))))
         (key-value-sorter (lambda (x y) (string< (car x) (car y))))
         (key-value-pairs
          (sort (mapcar key-value-generator initial-nodes) key-value-sorter )))
    ;;the naieve way of doing this is to just add each key value pair to the
    ;;trie using trie-add, which is what I'm going to do for not
    (dolist (key-value)
(defun make-trie-with-fn (fn seq)
  "build a trie out of a sequence by calling fn on each element of seq to
get a key-value pair. If fn only returns one value the value of each key 
will be set to t")
(defun trie-lookup (trie string)
  "search for string in trie, if it is found return it's value otherwise return nil.
There is no way to distinguish between a key with a value of nil and a key that
was not found. But a key shouldn't have a value of nil anyway")
(defun subtrie-lookup (string children)
  "internal function used to seach for string (which should generally be a suffix
of the actual string being searched for in the list of children for a node")
(defun trie-add (trie string &optional value)
  "insert string into trie with optional value, value is set to t if not given")
(defun trie-remove (trie string)
  "remove string from trie, return t if string was removed, nil otherwise")
(defun trie-find-add (trie string &optional value)
  "look for string in trie and add it if not found. Follows the same rules as
trie-add for adding a string. Returns two values the value of the string in 
the trie (which will be the same as value (or t) if newly added) and a value
indicating if the string was present in the trie already, t if it was otherwise nil")
(defun trie-traverse (trie fun)
  "traverse trie calling fun on each key, fun should take 1 or 2 arguments
and will be given the key and the corrsponding value as arguments")
(defun print-trie (trie)
  "print the trie, this function is simply for convience and is implemented
as (trie-traverse trie #'pprint)"
  (trie-traverse trie #'pprint))
