(defun euclid (m n)
  (let ((r (mod m n)))
    (if (zerop r)
        n
        (euclid n r))))
