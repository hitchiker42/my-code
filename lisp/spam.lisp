(defpackage :tucker.spam
  (:use :common-lisp :tucker.pathnames))

(defun classify (text)
  ;;parse text and classify as spam,ham or unsure
  (classification (score (extract-features text))))

(defparameter *max-ham-score* .4)
(defparameter *max-spam-score* .6)

(defun classification (score)
  ;;classify mail as spam or ham based on score
  (cond
    ((<= score *max-ham-score*) 'ham)
    ((>= score *max-spam-score*) 'spam)
    (t 'unsure)))

(defclass word-feature ()
  ((word
    :initarg :word
    :accessor word
    :initform (error "Must supply :word")
    :documentation "The word this feature represents.")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "Number of spams this feature has been seen in")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "Number of hams this feature has been seen in")))
;;or
(defstruct word-features
  (word 0 :type string)
  (spam-count 0 :type integer)
  (ham-count 0 :type integer))

(defvar *feature-database* (make-hash-table :test #'equal))
(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal)))
(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))
;;or (make-word-features (:word word)
