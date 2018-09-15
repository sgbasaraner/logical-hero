(load "logic.lisp")

(loop (let ((expression (make-random-expression)) (solution-or-not (random 2)))
    (write-line expression)
    (write-line (if (equal solution-or-not 0) (solve-expression expression) (make-wrong-answer expression)))
    (let ((answer (read-line)))
      (cond 
        ((equal answer (string "t")) 
          (write-line (if (equal solution-or-not 0) (string "Correct answer!") (string "Wrong answer!"))))
        ((equal answer (string "f")) 
          (write-line (if (equal solution-or-not 1) (string "Correct answer!") (string "Wrong answer!"))))
        (t (write-line (string "Please use t or f.")))))))