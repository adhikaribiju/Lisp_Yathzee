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