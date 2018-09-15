;;; parameters for convenience
(defparameter *and* (string "∧")
  "Logical AND operator.")

(defparameter *or* (string "∨")
  "Logical OR operator.")

(defparameter *true* (string "T")
  "Logical TRUE.")

(defparameter *false* (string "F")
  "Logical FALSE.")

;;; random generators
(defun should-negate-p () 
  (equal (random 3) 0))

(defun should-add-parantheses-p ()
  (equal (random 4) 0))

(defun make-random-item ()
  (string (char "TFp" (random 3))))

(defun make-random-operator () 
  (string (char "∧∨" (random 2))))

(defun make-random-block ()
  (if (should-negate-p) 
    (string "¬p") 
    (make-random-item)))

(defun make-random-expression ()
  (if (should-add-parantheses-p)
    (concatenate 'string "¬(" (make-random-block) " " (make-random-operator) " " (make-random-block) ")")
    (concatenate 'string (make-random-block) " " (make-random-operator) " " (make-random-block))))

;;; expression solving
(defun has-parantheses-p (expr)
  (equal (subseq expr 0 2) (string "¬(")))

(defun remove-parantheses (expr)
  (subseq expr 2 (- (length expr) 1)))

(defun get-operator (expr)
  (or (find #\∧ expr :test #'equal)
    (find #\∨ expr :test #'equal)))

(defun get-left (expr)
    (string-trim " " (subseq expr 0 2)))

(defun get-right (expr)
    (let ((len (length expr)))
      (string-trim " " (subseq expr (- len 2) len))))

(defun truep (block)
  (equal block *true*))

(defun falsep (block) 
  (equal block *false*))

(defun both-true-p (expr)
  (let ((left (get-left expr)) (right (get-right expr)))
  (and (truep left) (truep right))))

(defun both-false-p (expr) 
  (let ((left (get-left expr)) (right (get-right expr)))
  (and (falsep left)  (falsep right))))

(defun andp (operator)
  (equal (string operator) *and*))

(defun orp (operator)
  (equal (string operator) *or*))

;; returns the other part of the expression if one part is true,
;; doesn't check the other part if first part is true
(defun one-true (expr)
  (let ((left (get-left expr)) (right (get-right expr)))
  (cond ((truep left) right)
         ((truep right) left)
         (t nil))))

;; false version of one-true
(defun one-false (expr)
  (let ((left (get-left expr)) (right (get-right expr)))
  (cond ((falsep left) right)
         ((falsep right) left)
         (t nil))))

(defun negate-block (block)
  (cond 
  ((equal block *false*) *true*)
  ((equal block *true*) *false*)
  ((equal (subseq block 0 1) (string "¬")) (string "p"))
  (t (concatenate 'string "¬" block))))


(defun solve-without-parantheses (expr)
  (let ((operator (get-operator expr)))
    (cond 
      ((both-true-p expr) *true*)
      ((both-false-p expr) *false*)
      ((and (one-true expr) (orp operator)) *true*)
      ((and (one-true expr) (andp operator)) (one-true expr))
      ((and (one-false expr) (orp operator)) (one-false expr))
      ((and (one-false expr) (andp operator)) *false*)
      ((equal (get-left expr) (get-right expr)) (get-left expr))
      ((orp operator) *true*)
      (t *false*))))

(defun solve-expression (expr)
  (if (has-parantheses-p expr)
    (negate-block (solve-without-parantheses (remove-parantheses expr)))
    (solve-without-parantheses expr)))

;; generates a wrong but not completely unbelievable answer
(defun make-wrong-answer (expr)
  (let ((solution (solve-expression expr)))
    (cond 
      ((equal solution (string "¬p")) (make-random-item))
      ((equal solution (string "p")) (negate-block (make-random-item)))
      ((find #\p expr :test #'equal) (nth (random 3) 
        (remove-if (lambda (str) (equal str solution)) '("p" "T" "F" "¬p"))))
      (t (negate-block solution)))))

(let ((expr (make-random-expression))) (print expr) (print (make-wrong-answer expr)) (solve-expression expr))