(defun removeDuplicates (lst)
  (cond
    ((null lst) nil)
    (t (cons (car lst) (removeDuplicates (removeElement (car lst) (cdr lst)))))))

(defun removeElement (element lst)
  (cond
    ((null lst) nil)
    ((= element (car lst)) (removeElement element (cdr lst)))
    (t (cons (car lst) (removeElement element (cdr lst))))))

(defun sortDice (lst)
  (cond
    ((null lst) nil)
    ((null (cdr lst)) lst)
    (t (let* ((pivot (car lst))
              (less (partitionLess pivot (cdr lst)))
              (greater (partitionGreaterOrEqual pivot (cdr lst))))
         (append (sortDice less) (list pivot) (sortDice greater))))))

(defun partitionLess (pivot lst)
  (cond
    ((null lst) nil)
    ((< (car lst) pivot) (cons (car lst) (partitionLess pivot (cdr lst))))
    (t (partitionLess pivot (cdr lst)))))

(defun partitionGreaterOrEqual (pivot lst)
  (cond
    ((null lst) nil)
    ((>= (car lst) pivot) (cons (car lst) (partitionGreaterOrEqual pivot (cdr lst))))
    (t (partitionGreaterOrEqual pivot (cdr lst)))))


(defun isTwoSequential (dice)
  (let ((sorted-dice (sortDice (removeDuplicates dice))))
    (findTwoSequential sorted-dice)))

(defun findTwoSequential (sorted-dice)
  (cond
    ((null sorted-dice) nil)
    ((null (cdr sorted-dice)) nil)
    ((= (1+ (car sorted-dice)) (cadr sorted-dice))
     (list (car sorted-dice) (cadr sorted-dice)))
    (t (findTwoSequential (cdr sorted-dice)))))

(defun isThreeSequential (dice)
  (let ((sorted-dice (sortDice (removeDuplicates dice))))
    (findThreeSequential sorted-dice)))

(defun findThreeSequential (sorted-dice)
  (cond
    ((or (null sorted-dice) (null (cdr sorted-dice)) (null (cddr sorted-dice))) nil)
    ((and (= (1+ (car sorted-dice)) (cadr sorted-dice))
          (= (1+ (cadr sorted-dice)) (caddr sorted-dice)))
     (list (car sorted-dice) (cadr sorted-dice) (caddr sorted-dice)))
    (t (findThreeSequential (cdr sorted-dice)))))

(defun isFourSequential (dice)
  (let ((sorted-dice (sortDice (removeDuplicates dice))))
    (findFourSequential sorted-dice)))

(defun findFourSequential (sorted-dice)
  (cond
    ((or (null sorted-dice) (null (cdr sorted-dice)) (null (cddr sorted-dice)) (null (cdddr sorted-dice))) nil)
    ((and (= (1+ (car sorted-dice)) (cadr sorted-dice))
          (= (1+ (cadr sorted-dice)) (caddr sorted-dice))
          (= (1+ (caddr sorted-dice)) (cadddr sorted-dice)))
     (list (car sorted-dice) (cadr sorted-dice) (caddr sorted-dice) (cadddr sorted-dice)))
    (t (findFourSequential (cdr sorted-dice)))))

(defun testIsTwoSequential ()
  (format t "~%Testing isTwoSequential:~%")
  (format t "Input: (2 2 2 2 3)~%Expected: (2 3)~%Result: ~a~%~%" (isTwoSequential '(2 2 2 2 3)))
  (format t "Input: (1 3 4 2 5)~%Expected: (1 2)~%Result: ~a~%~%" (isTwoSequential '(1 3 4 2 5)))
  (format t "Input: (6 7 8 4 8)~%Expected: (5 6)~%Result: ~a~%~%" (isTwoSequential '(6 8 8 7 8)))
  (format t "Input: (10 10 12 11 14)~%Expected: (11 12)~%Result: ~a~%~%" (isTwoSequential '(10 10 12 11 14)))
  (format t "Input: (9)~%Expected: NIL~%Result: ~a~%~%" (isTwoSequential '(9)))
  (format t "Input: ()~%Expected: NIL~%Result: ~a~%~%" (isTwoSequential '())))

(defun testIsThreeSequential ()
  (format t "~%Testing isThreeSequential:~%")
  (format t "Input: (2 2 1 6 3)~%Expected: (1 2 3)~%Result: ~a~%~%" (isThreeSequential '(2 2 1 6 3)))
  (format t "Input: (4 5 6 3 8)~%Expected: (4 5 6)~%Result: ~a~%~%" (isThreeSequential '(4 5 6 3 8)))
  (format t "Input: (7 8 9 10 12)~%Expected: (7 8 9)~%Result: ~a~%~%" (isThreeSequential '(7 8 9 10 12)))
  (format t "Input: (1 3 4 6 8)~%Expected: NIL~%Result: ~a~%~%" (isThreeSequential '(1 3 4 6 8)))
  (format t "Input: (10 11 13 12 14)~%Expected: (10 11 12)~%Result: ~a~%~%" (isThreeSequential '(10 11 13 12 14)))
  (format t "Input: (3)~%Expected: NIL~%Result: ~a~%~%" (isThreeSequential '(3)))
  (format t "Input: ()~%Expected: NIL~%Result: ~a~%~%" (isThreeSequential '())))

(defun testIsFourSequential ()
  (format t "~%Testing isFourSequential:~%")
  (format t "Input: (1 2 3 4 6)~%Expected: (1 2 3 4)~%Result: ~a~%~%" (isFourSequential '(1 2 3 4 6)))
  (format t "Input: (5 4 3 6 7)~%Expected: (4 5 6 7)~%Result: ~a~%~%" (isFourSequential '(5 4 3 6 7)))
  (format t "Input: (8 7 6 5 9)~%Expected: (5 6 7 8)~%Result: ~a~%~%" (isFourSequential '(8 7 6 5 9)))
  (format t "Input: (10 9 8 11 12)~%Expected: (8 9 10 11)~%Result: ~a~%~%" (isFourSequential '(10 9 8 11 12)))
  (format t "Input: (2 3 1 5)~%Expected: NIL~%Result: ~a~%~%" (isFourSequential '(2 3 1 5)))
  (format t "Input: (1)~%Expected: NIL~%Result: ~a~%~%" (isFourSequential '(1)))
  (format t "Input: ()~%Expected: NIL~%Result: ~a~%~%" (isFourSequential '())))
  
  
(format t "Testing removeDuplicates: ~%")
(format t "removeDuplicates '(1 2 2 3 3 4): ~a~%" (removeDuplicates '(1 2 2 2 2 4)))
(format t "removeDuplicates '(5 5 5 5 5): ~a~%" (removeDuplicates '(5 5 5 5 5)))
(format t "removeDuplicates '(1 3 2 2 3 1): ~a~%" (removeDuplicates '(1 3 2 2 3 1)))

(format t "~%Testing sortDice: ~%")
(format t "sortDice '(4 3 1 2 5): ~a~%" (sortDice '(4 3 1 2 5)))
(format t "sortDice '(10 9 8 7 6): ~a~%" (sortDice '(10 9 8 7 6)))
(format t "sortDice '(1 2 3 4 5): ~a~%" (sortDice '(1 2 3 4 5)))




;; Run the tests
(testIsTwoSequential)
(testIsThreeSequential)
(testIsFourSequential)


(defun findIndicesOfSequence (dice sequence)
  (findIndices dice sequence 1))

(defun findIndices (dice sequence current-index)
  (cond
    ((null sequence) nil)
    ((null dice) nil)
    ((= (car dice) (car sequence))
     (let ((rest-indices (findIndices (cdr dice) (cdr sequence) (1+ current-index))))
       (cond
         (rest-indices (cons current-index rest-indices))
         (t (cons current-index nil)))))
    (t (findIndices (cdr dice) sequence (1+ current-index)))))

(defun findIndex (element dice current-index)
  (cond
    ((null dice) nil)
    ((= (car dice) element) current-index)
    (t (findIndex element (cdr dice) (1+ current-index)))))



(defun testFindIndicesOfSequence ()
  (format t "~%Testing findIndicesOfSequence:~%")
  (format t "Input: dice = (1 4 4 2 3), sequence = (1 2 3)~%Expected: (1 2 5)~%Result: ~a~%~%"
          (findIndicesOfSequence '(1 4 4 2 3) '(1 2 3)))
  (format t "Input: dice = (4 5 8 6 9 7 8), sequence = (5 6 7)~%Expected: (2 3 4)~%Result: ~a~%~%"
          (findIndicesOfSequence '(4 5 8 6 9 7 8) '(4 5 6 7)))
  (format t "Input: dice = (2 3 4 5 6), sequence = (2 3 4)~%Expected: (1 2 3)~%Result: ~a~%~%"
          (findIndicesOfSequence '(2 3 4 5 6) '(2 3 4)))
  (format t "Input: dice = (6 7 6 5 4), sequence = (5 6 7)~%Expected: (4 3 2)~%Result: ~a~%~%"
          (findIndicesOfSequence '(6 7 6 5 4) '(5 6 7)))
  (format t "Input: dice = (1 3 5 7 9), sequence = (1 2 3)~%Expected: NIL~%Result: ~a~%~%"
          (findIndicesOfSequence '(1 3 5 7 9) '(1 2 3)))
  (format t "Input: dice = (10 12 14 15 16), sequence = (14 15 16)~%Expected: (3 4 5)~%Result: ~a~%~%"
          (findIndicesOfSequence '(10 12 14 15 16) '(14 15 16)))
  (format t "Input: dice = (1 1 1 1 1), sequence = (1 1)~%Expected: (1 2)~%Result: ~a~%~%"
          (findIndicesOfSequence '(1 1 1 1 1) '(1 1)))
  (format t "Input: dice = (3 1 4 1 5), sequence = (1 4 5)~%Expected: (2 3 5)~%Result: ~a~%~%"
          (findIndicesOfSequence '(3 1 4 1 5) '(1 4 5)))
  (format t "Input: dice = (2 2 2 3 4), sequence = (2 3 4)~%Expected: (1 4 5)~%Result: ~a~%~%"
          (findIndicesOfSequence '(2 2 2 3 4) '(2 3 4))))

;; Run the tests
(testFindIndicesOfSequence)