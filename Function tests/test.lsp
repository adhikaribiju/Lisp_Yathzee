(defun doReRoll (dice indices)
  (let ((choice (getValidChoice)))
    (cond
      ((equal choice 'R)
       (reRollRandomly dice indices))
      ((equal choice 'M)
       (let ((new-values (getValidValues indices)))
         (reRollManually dice indices new-values))))))

(defun getValidChoice ()
  (format t "Do you want to reroll manually (M) or randomly (R)? ")
  (finish-output)
  (let ((choice (read)))
    (cond
      ((or (equal choice 'M) (equal choice 'R)) choice)
      (t (format t "Invalid input. Please enter 'M' for manually or 'R' for randomly.~%")
         (getValidChoice)))))

(defun getValidValues (indices)
  (format t "Enter the new dice values as a list of length ~a: " (length indices))
  (finish-output)
  (let ((new-values (read)))
    (cond
      ((and (listp new-values)
            (= (length new-values) (length indices)))
       new-values)
      (t (format t "Invalid list. Make sure the list has exactly ~a elements.~%" (length indices))
         (getValidValues indices)))))

(defun reRollRandomly (dice indices)
  (rerollDice dice indices (generate-random-values (length indices))))

(defun reRollManually(dice indices new-values)
  (rerollDice dice indices new-values))

(defun generate-random-values (n)
  (if (<= n 0)
      '()
      (cons (+ 1 (random 6)) (generate-random-values (- n 1)))))

(defun rerollDice (dice indices new-values)
  (if (or (null indices) (null new-values))
      dice
      (let ((index (car indices))
            (new-value (car new-values)))
        (rerollDice (replaceNth dice (- index 1) new-value)
                     (cdr indices)
                     (cdr new-values)))))

(defun replaceNth (lst n value)
  (cond
    ((null lst) '())
    ((= n 0) (cons value (cdr lst)))
    (t (cons (car lst) (replaceNth (cdr lst) (- n 1) value)))))



;(print (doReRoll '(1 1 2 3 4) '(2 4)))
