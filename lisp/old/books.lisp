;book database
(defvar *db* nil)
;make data base entry (book)
(defun make-book (title author rating read)
  (list :title title :author author :rating rating :read read))
;add book to database
(defun add-book (book) (push book *db*))
;prompt for input
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))
;make a book entry from user input
(defun prompt-for-book ()
  (make-book
   (prompt-read "Title")
   (prompt-read "Author")
   (parse-integer (prompt-read "Rating") :junk-allowed t)
   (y-or-n-p "Read [y/n]:")))
;add books to database using user imput
(defun add-books ()
  (loop (add-book (prompt-for-book))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))
;save database to file
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax 
      (print *db* out))))
;load database from file
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))
;print database in human readable form
(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))
;select books in database based on selector function
(defun select (selector)
  (remove-if-not selector *db*))
;return books matching given parameters
;depreciated
(defun where (&key title author rating (read nil read-p))
  #'(lambda (book)
      (and
       (if title  (equal (getf book :title ) title ) t)
       (if author (equal (getf book :author) author) t)
       (if rating (equal (getf book :rating) rating) t)
       (if read-p (equal (getf book :read  ) read  ) t))))
;update book in database using given parameters
(defun update (selector &key title author rating (read nil read-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector row)
              (if title  (equal (getf row :title ) title ))
              (if author (equal (getf row :author) author))
              (if rating (equal (getf row :rating) rating))
              (if read-p (equal (getf row :read  ) read  )))
             row) *db*)))
;remove entrys
(defun delete-rows (selector)
  (setf *db* (remove-if selector *db*)))
;make a compairson function for value in field
(defun make-comparitor (field value)
  `(equal (getf book ,field) ,value))
;make a list of comparitiors based on fields
(defun make-comparisons-list (fields)
  (loop while fields
       collecting (make-comparitor (pop fields) (pop fields))))
;replacement for where function
;given a series of :field value pairs return all matching books
(defmacro where (&rest clauses)
  `#'(lambda (book) (and ,@(make-comparisons-list clauses))))
