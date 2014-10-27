(with-temp-file "random20"
  (insert (format "%d %d\n" 1000 20))
  (dotimes (i 1000)
    (insert (format "%d\n" (random (1- (expt 2 20)))))))
