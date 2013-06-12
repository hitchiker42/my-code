(require 'cl-lib)
;;actually useful
(defun to-hex (dec)
  (format "%x" dec))
(cl-defmacro seq (&key (start 0) (end 10) (step 1))
`(let ((seq-count ,start)
       (seq-list ()))
   (while (< seq-count ,end)
     (setq seq-list (cons seq-count seq-list))
     (setq seq-count (+ seq-count ,step)))
     (princ (reverse  seq-list))))
(cl-defmacro my-dotimes (var end &rest body);; &key (start 0) (step (/ end (abs end))))
  `(lexical-let ((x 0))
     (while (<= (abs x) (abs ,end))
       (let ((',var x))
        (progn ,@body))
       (setq x (+ var 1)))))
(defmacro body-test (&rest body)
  `(progn ,@body))
;; char *decToHex(unsigned input)
;; {
;;     char *output = malloc(sizeof(unsigned) * 2 + 3);
;;     strcpy(output, "0x00000000");

;;     static char HEX[] = "0123456789ABCDEF";

;;     // represents the end of the string.
;;     int index = 9;

;;     while (input > 0 ) {
;;         output[index--] = HEX[(input & 0xF)];
;;         input >>= 4;            
;;     }

;;     return output;
;; }
;; (defun to-hex (dec) 
;; "This probably doesn't work as it would require big endian
;;  input to work correctly"
;;   (let ((i (length (format "%x" dec)))
;;         (in dec)
;;         (out 0))
;;     (while (> in 0)
;;       (setf out (+ out (* (expt 16 i) (logand in #xf))))
;;       (setf in (lsh in 4))
;;       (setf i (1- i)))
;;     out))

