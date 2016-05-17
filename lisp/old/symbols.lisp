(defmacro symbol-splice (&rest symbols)
"returns the symbol created by concatinating the names of symbols"
  `(intern (apply #'concatenate 'string (mapcar #'symbol-name ',symbols))
          *package*))
(defmacro symbol-splice-strings (&rest strings); &key (literal-case nil))
"returns the symbol with the name formed by concatinating strings"
;(let ((upcase (if literal-case #'identity #'string-upcase)))
  `(intern (concatenate 'string ,@(mapcar #'string-upcase strings))
           *package*))
(defmacro define-symbol-splice-strings 
    (definition &rest strings)
"set the symbol with the name of strings concatenated together to definition"
  `(setq
    ,(intern (apply #'concatenate 'string 
                    (mapcar #'string-upcase strings)) *package*)
     ,definition))
(defmacro define-symbol-splice-strings 
    (definition &rest strings)
"set the symbol with the name of strings concatenated together to definition"
  `(setq
    ,(intern (apply #'concatenate 'string
                    (mapcar #'symbol-namestrings)) *package*)
    ,definition))
