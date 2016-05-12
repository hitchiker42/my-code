(define-macro (λ arg . body) `(lambda (,arg) ,@body))
(define Y (λ g (λ x (g (x x))) (λ x (g (x x)))))
(define F (λ r (λ n (if (= n 0) 1 (* n (r (1- n)))))))
(define iseven (Y (λ x (if (even? x) 0 1)))
