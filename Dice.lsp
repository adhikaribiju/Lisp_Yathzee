(defun initialize-random-seed ()
  (setf *random-state* (make-random-state t)))

(defun get-random-dice-roll ()
  "Generate a random dice roll between 1 and 6."
  (initialize-random-seed) ;; Initialize the random seed
  (+ 1 (random 6)))



;; ROLL DICE FUNCTION
(defun rollDice ()
  (let ((response (get-yes-no-input)))
    (cond
      ((equal response "Y") (getManualDice))  ;; If "Y", get manual input
      (t (generate-random-dice 5)))))            ;; Else, generate random dice

(defun get-yes-no-input ()
  "Ask the user for a valid Y/N input. Repeat until valid input is received."
  (format t "Do you want to manually enter the dice? (Y/N): ")
  (finish-output)
  (let ((input (read-line)))
    (cond
      ((equal input "Y") "Y")
      ((equal input "N") "N")
      (t (format t "Invalid Input~%")   ;; Invalid input, repeat the process
         (get-yes-no-input)))))         ;; Recursive call until valid Y/N is received

(defun get-manual-dice (n)
  "Recursively gather N dice values between 1 and 6."
  (cond
    ((zerop n) nil)  ;; Base case: when no more dice to gather, return empty list
    (t (let ((dice-value (get-valid-dice-input n)))
         (cons dice-value (get-manual-dice (1- n)))))))  ;; Recursion: prepend dice value to list

(defun getManualDice ()
  (format t "Enter the Dice Values in the form of a list (e.g., (1 2 3 4 5)): ")
  (finish-output)
  (let ((input (read)))
    (cond
      ((isValidList input) input)
      (t (displayInvalidMsg)))))

(defun displayInvalidMsg ()
  (format t "Invalid input! Please enter exactly 5 values between 1 and 6.~%")
  (getManualDice))

(defun isValidList (dice)
  "Checks if the input is a list of exactly 5 integers, each between 1 and 6."
  (and (listp dice)                ; Check if it's a list
       (= (length dice) 5)         ; Check if it has exactly 5 elements
       (isValidNum dice)))

(defun isValidNum (dice)
  "Checks if all elements in the dice list are integers between 1 and 6."
  (cond
    ((null dice) t)  ; Base case: if the list is empty, return true
    ((and (integerp (car dice)) (<= 1 (car dice) 6))
     (isValidNum (cdr dice)))
    (t nil)))

;; delete garna milcha
(defun get-valid-dice-input (i)
  "Ask for a valid dice value (1-6). Recursively retry until valid input is given."
  (format t "Enter value for dice ~d (1-6): " i)
  (finish-output)
  (let ((input (parse-integer (read-line))))
    (cond
      ((and (integerp input) (<= 1 input 6)) input)  ;; Valid input, return it
      (t (format t "Invalid Input. Please enter a number between 1 and 6.~%") ;; Invalid input
         (get-valid-dice-input i)))))  ;; Recursion: ask again for valid input

(defun generate-random-dice (n)
  "Recursively generate N random dice values between 1 and 6."
  (cond
    ((zerop n) nil)  ;; Base case: no more dice to generate, return empty list
    (t (cons (+ 1 (random 6))  ;; Generate a random dice value (between 1 and 6)
             (generate-random-dice (1- n))))))  ;; Recursively generate remaining dice
;; ;; ROLL DICE FUNCTION ENDS








; The doReRoll function asks the user whether they want to reroll dice randomly or manually.
; It keeps asking until valid input is given, 
; then updates and returns the dice list with new values at the given indices.
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




; returns the number of times valueTocount appears in dice, else returns nil
(defun findCount (dice valueTocount)
  (cond
    ((null dice) nil)  ;; Base case: if the list is empty, return nil
    ((equal (car dice) valueTocount)  ;; If the first element matches valueTocount
     (let ((rest-count (findCount (cdr dice) valueTocount)))
       (if rest-count
           (+ 1 rest-count)
           1)))
    (t (findCount (cdr dice) valueTocount))))  ;; Continue with the rest of the list





  ; computer dice functions
  (defun findMatchingIndices (dice match-count)
  (labels ((helper (remaining-dice seen-values)
             (cond
               ((null remaining-dice) nil)  ;; If the list is empty, return nil
               (t 
                (let* ((value (car remaining-dice))
                       (occurrences (countOccurrences value dice)))
                  (cond
                    ;; If occurrences are greater than or equal to match-count, collect only the first match-count indices
                    ((and (>= occurrences match-count) (not (member value seen-values)))
                     (collect-first-n (findIndices value dice 1) match-count))
                    ;; Otherwise, add value to seen and continue searching
                    (t (helper (removeValue value remaining-dice) (cons value seen-values)))))))))
    (helper dice nil)))

(defun collect-first-n (lst n)
  (cond
    ((or (null lst) (= n 0)) nil)  ;; Base case: if the list is empty or we've collected n elements, return nil
    (t (cons (car lst) (collect-first-n (cdr lst) (1- n))))))  ;; Otherwise, collect the first element and continue

(defun countOccurrences (value dice)
  (cond
    ((null dice) 0)  ;; Base case: if the list is empty, return 0
    ((equal (car dice) value) 
     (+ 1 (countOccurrences value (cdr dice))))
    (t (countOccurrences value (cdr dice)))))

(defun findIndices (value dice index)
  (cond
    ((null dice) '())  ;; Base case: if the list is empty, return an empty list
    ((equal (car dice) value) 
     (cons index (findIndices value (cdr dice) (+ index 1))))
    (t (findIndices value (cdr dice) (+ index 1)))))

(defun removeValue (value dice)
  (cond
    ((null dice) '())
    ((equal (car dice) value) (removeValue value (cdr dice)))
    (t (cons (car dice) (removeValue value (cdr dice))))))

(defun giveFourOfaKindIndices (dice)
  (findMatchingIndices dice 4))

(defun giveThreeOfaKindIndices (dice)
  (findMatchingIndices dice 3))

(defun giveTwoOfaKindIndices (dice)
  (findMatchingIndices dice 2))



  (defun custom-remove (lst items-to-remove)
  (cond
    ((null lst) '())  ;; If the list is empty, return an empty list
    ((member (car lst) items-to-remove) 
     (custom-remove (cdr lst) items-to-remove))  ;; If the element is in items-to-remove, skip it
    (t (cons (car lst) (custom-remove (cdr lst) items-to-remove)))))  ;; Otherwise, keep the element


; four of a kind ko lagi,
; returns the index of the element that is part of the two distinct pairs of numbers in the list
(defun checkUniqueAmongPairs (lst)
  (let ((pairs (collectPairs lst lst)))
    (cond
      ((or (null pairs) (/= (length pairs) 2))
       nil)  ;; Return nil if there are not exactly two distinct pairs
      (t
       (uniqueIndexAmongPairs lst (car pairs) (cadr pairs) 0)))))  ;; Find the unique element

(defun collectPairs (lst full-list &optional pairs)
  "Collect distinct elements that appear exactly twice."
  (cond
    ((null lst) pairs)  ;; If the list is empty, return the collected pairs
    ((and (= (count (car lst) full-list) 2)
          (not (member (car lst) pairs)))
     (collectPairs (cdr lst) full-list (cons (car lst) pairs)))
    (t (collectPairs (cdr lst) full-list pairs))))

(defun uniqueIndexAmongPairs (lst pair1 pair2 index)
  (cond
    ((null lst) nil)  ;; If the list is empty, return nil
    ((and (not (equal (car lst) pair1))
          (not (equal (car lst) pair2)))
     (+ index 1))  ;; Return the incremented index if the element is not part of either pair
    (t (uniqueIndexAmongPairs (cdr lst) pair1 pair2 (+ index 1)))))







;; the functions below checks if the dices are in certain sequence, to accomplish this there are helper functions to sort and delete some elements
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

(defun findIndicesOfSequence (dice sequence)
  (findIndicesS dice sequence 1))

(defun findIndicesS (dice sequence current-index)
  (cond
    ((null sequence) nil)
    ((null dice) nil)
    ((= (car dice) (car sequence))
     (let ((rest-indices (findIndicesS (cdr dice) (cdr sequence) (1+ current-index))))
       (cond
         (rest-indices (cons current-index rest-indices))
         (t (cons current-index nil)))))
    (t (findIndicesS (cdr dice) sequence (1+ current-index)))))

(defun findIndex (element dice current-index)
  (cond
    ((null dice) nil)
    ((= (car dice) element) current-index)
    (t (findIndex element (cdr dice) (1+ current-index)))))






; for Displaying the computer kept dice message
(defun get-dice-values-at-indices (dice indices)
  (cond
    ;; Base case: if the indices list is empty, return an empty list
    ((null indices) nil)
    ;; Recursive case: get the dice value at the current index and recurse on the rest of the indices
    (t (cons (nth (1- (car indices)) dice)
             (get-dice-values-at-indices dice (cdr indices))))))