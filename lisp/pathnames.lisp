(defpackage :tucker.pathnames
  (:use :common-lisp))
(in-package :tucker.pathnames)
;;check if path name componant value exists and is defined
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))


(defun directory-pathname-p (p)
  ;;check if pathname is in directory form
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  ;;convert pathname name into directory form
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name nil
         :type nil
         :defaults pathname)
        pathname)))

(defun directory-wildcard (dirname)
  ;;takes dirname and returns wildcard
  ;;i.e. give /home/name return /home/name/*, kinda...
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname)
  ;;lists all files in dirname
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))
    #+(or sbcl cmu lispworks)
    (directory wildcard)
    #+openmcl
    (directory wildcard :directories t)
    #+allergo
    (directory wildcard :directories-are-files nil)
    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))
    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))
#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

(defun file-exists-p (pathname)
  ;;check if pathname exists and if it's a directory
  ;;return name in directory form
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form)))))

(defun pathname-as-file (name)
  ;;return file format pathname for name
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
        (let* ((directory (pathname-directory pathname))
               (name-and-type (pathname (first (last directory)))))
          (make-pathname
           :directory (butlast directory)
           :name (pathname-name name-and-type)
           :type (pathname-name name-and-type)
           :defaults pathname))
        pathname)))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  ;;apply fn to all files in dirname, as well as directories if directories is t
  ;;if test is given only apply fn on files than test returns true for
  (labels
   ((walk (name)
      (cond
        ((directory-pathname-p name)
         (when (and directories (funcall test name))
           (funcall fn name))
         (dolist (x  (list-directory name)) (walk x)))
        ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))
