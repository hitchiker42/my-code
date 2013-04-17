(defpackage :tucker.spam
  (:use :common-lisp :tucker.pathnames))

(defun classify (text)
  "parse text and classify as spam,ham or unsure"
  (classification (score (extract-features text))))

(defparameter *max-ham-score* .4)
(defparameter *max-spam-score* .6)

(defun classification (score)
  "classify mail as spam or ham based on score"
  (values
   (cond
     ((<= score *max-ham-score*) 'ham)
     ((>= score *max-spam-score*) 'spam)
     (t 'unsure))
   score))

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
"print word-feature object"
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
"clear database of word-features and reset total spam/ham counts to 0"
  (setf
   *feature-database* (make-hash-table :test #'equal)
   *total-hams* 0
   *total-spams* 0))
(defun intern-feature (word)
"add word to feature database as word-feature"
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))
;;or (make-word-features (:word word)
(defun extract-words (text)
"pull out all words (where words are any 3 consecutive alphabetic characters)
from text using regexs and return as a set of strings"
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))
(defun extract-features (text)
  "create feature objects from words extracted from text"
  (mapcar #'intern-feature (extract-words text)))
(defun train (text type)
  "\"teach\" spam filter to recognize spam and ham"
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))
(defun increment-count (feature type)
  "increment count for type type(ham or spam) on feature object feature"
  (ecase type ;case which raises error if not matched
    (ham (incf (ham-count feature))) ;essentaly keys are implicitly quoted
    (spam (incf (spam-count feature)))))
(defun increment-total-count (type)
  "increase total count of global counter for type"
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))
(defun spam-probability (feature)
  "calculate the probablity that feature will appear in a spam"
  (with-slots (spam-count ham-count) feature
    (let ((spam-freq (/ spam-count (max 1 *total-spams*)))
          (ham-freq (/ ham-count (max 1 *total-hams*))))
      (/ spam-freq (+ spam-freq ham-freq)))))
(defun bayesian-spam-probability (feature &optional
                                           (assumed-probability 1/2)
                                           (weight 1))
  "calculates bayesian probability that feature incicates a spam"
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))
(defun score (features)
  "calculate a net spam score for the list features"
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      ;;loop to collect bayesian spam/ham probabilities for each feature
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    ;;merge probabilites from ham-probs and spam-probs using the fisher method
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))
(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))
(defun fisher (probs number-of-probs)
  "the fisher methods of combining probabilities"
  (inverse-chi-square
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))
(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min
   (loop with m = (/ value 2)
        for i below (/ degrees-of-freedom 2)
        for prob = (exp (- m)) then (* prob (/ m i))
        summing prob)
   1.0))
;;Note corpus is a large set of messages of known type(spam or ham)
(defun add-file-to-corpus (filename type corpus)
  ;;add filename of type type to corpus
  (vector-push-extend (list filename type) corpus))
(defun add-directory-to-corpus (dir type corpus)
  ;;add all files in dir to corpus with type type
  (dolist (filename (list-directory dir))
    (add-file-to-corpus filename type corpus)))
