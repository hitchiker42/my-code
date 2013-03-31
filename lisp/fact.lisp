(defun fact (n) 
  (cond ((= (car n) 0) 1)
        (t (cons (fact (1+ (car n))) n))))
