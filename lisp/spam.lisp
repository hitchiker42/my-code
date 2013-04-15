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
(defmethod print-object ((object word-feature) stream)
;;print feature object
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))
;;or
(defstruct word-feature2
  (word2 "" :type string)
  (spam-count2 0 :type integer)
  (ham-count2 0 :type integer))

(defvar *feature-database* (make-hash-table :test #'equal))
(defvar *total-spams* 0)
(defvar *total-hams* 0)
(defun clear-database ()
  (setf 
   *feature-database* (make-hash-table :test #'equal)
   *total-hams* 0
   *total-spams* 0))
(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))
;;or (make-word-features (:word word)
(defun extract-words (text)
;;pull out all words (where words are any 3 consecutive alphabetic characters)
;;from text using regexs and return as a set of strings
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))
(defun extract-features (text)
  ;;create feature objects from words extracted from text
  (mapcar #'intern-feature (extract-words text)))
(defun train (text type)
  ;;"teach" spam filter to recognize spam and ham
  (dolist (feature (extract-feature text))
    (increment-count feature type))
  (increment-total-count type))
(defun increment-count (feature type)
  ;;increment count for type type(ham or spam) on feature object feature
  (ecase type ;case which raises error if not matched
    (ham (incf (ham-count feature))) ;essentaly keys are implicitly quoted
    (spam (incf (spam-count feature)))))
(defun increment-total-count (type)
  ;;increase total count of global counter for type
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))
