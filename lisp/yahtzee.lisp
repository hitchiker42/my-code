(defun roll ()
  (labels ((rand () (1+ (random 5))))
    (list (rand) (rand) (rand) (rand) (rand))))
(roll)
(defun yahtzee ()
  (labels ((roll-dice (cnt) 
             (let ((dice (roll)))
               (if 
                (find-if-not (lambda (x) (eq (car dice) x)) (cdr dice))
                (roll-dice (+ 1 cnt))
                cnt))))
    (roll-dice 0)))

