(load "Scorecard.lsp")
(load "Dice.lsp")

(defun playHumanTurn (scorecard round)
 
  (display-scorecard scorecard)  ; Display current scorecard

  (format t "Your turn~%")

  (let ((dice (rollDice)))
    (format t "You rolled: ~{~a ~}~%" dice)
    (format t "Available Categories to Score: ~a~%" (available-categories dice))

    (let ((final-dice (tryreroll dice scorecard 0 nil)))  ; Passing nil for kept-dice
      ; Display the final dice after rerolls
      (format t "Your final dice: ~{~a ~}~%" final-dice)
      (format t "Enter the category you wish to score: ")
      (finish-output)

      ; Read the category and update the scorecard
      (let* ((category (read))
             (new-scorecard (scoreCategory scorecard final-dice category 1 round)))  ; Updating the scorecard
        (format t "You chose category ~a~%" category)
        (display-scorecard new-scorecard)  ; Display the updated scorecard
        
        new-scorecard)))) ; Return the updated scorecard



; Generate a list of numbers from start to end inclusive
(defun number-sequence (start end)
  (cond
    ((> start end) nil)
    (t (cons start (number-sequence (+ start 1) end)))))

; Create pairs of positions and dice values
(defun make-dice-pairs (positions dice)
  (cond
    ((null positions) nil)
    (t (cons (cons (car positions) (car dice))
             (make-dice-pairs (cdr positions) (cdr dice))))))

; Find a matching dice pair for the given value
(defun find-matching-dice-pair (value dice-pairs available-positions used-positions)
  (cond
    ((null dice-pairs) nil)
    (t
     (let ((pair (car dice-pairs)))
       (cond
         ((and (equal (cdr pair) value)
               (member (car pair) available-positions)
               (not (member (car pair) used-positions)))
          pair)
         (t
          (find-matching-dice-pair value (cdr dice-pairs) available-positions used-positions)))))))

; Helper function to recursively match values to dice positions
(defun match-values-to-dice-helper (values dice-pairs available-positions used-positions acc)
  (cond
    ((null values)
     (values t (reverse acc)))  ; All values matched successfully
    (t
     (let ((match (find-matching-dice-pair (car values) dice-pairs available-positions used-positions)))
       (cond
         (match
          (match-values-to-dice-helper (cdr values) dice-pairs available-positions
                                       (cons (car match) used-positions)
                                       (cons (car match) acc)))
         (t
          (values nil nil)))))))

; Try to match the user's desired dice values to available dice positions
(defun match-values-to-dice (values dice available-positions)
  (let ((dice-pairs (make-dice-pairs (number-sequence 1 (length dice)) dice)))
    (match-values-to-dice-helper values dice-pairs available-positions '() '())))

; Validate dice values and map them to indices
(defun validate-dice-values (values dice kept-dice)
  (let ((available-dice (set-difference (number-sequence 1 (length dice)) kept-dice)))
    (multiple-value-bind (success indices)
        (match-values-to-dice values dice available-dice)
      (values success indices))))

; Helper function to retrieve kept dice values
(defun kept-dice-values-helper (kept-dice dice)
  (cond
    ((null kept-dice) nil)
    (t (cons (nth (- (car kept-dice) 1) dice)
             (kept-dice-values-helper (cdr kept-dice) dice)))))

; Retrieve kept dice values
(defun kept-dice-values-fn (kept-dice dice)
  (cond
    ((null kept-dice) nil)
    (t (kept-dice-values-helper kept-dice dice))))

; Compute the new kept dice based on the dice indices not being rerolled
(defun compute-new-kept-dice (dice-indices dice-length)
  (set-difference (number-sequence 1 dice-length) dice-indices))

; Read a valid die value (1-6) from the user
(defun read-die-value (current-value)
  (format t "Enter the new value for dice (~a): " current-value)
  (finish-output)
  (let ((new-value (read)))
    (cond
      ((and (integerp new-value) (>= new-value 1) (<= new-value 6))
       new-value)
      (t
       (format t "Invalid value. Please enter a number between 1 and 6.~%")
       (read-die-value current-value)))))

; Helper function for rerolling dice manually at specified indices
(defun manually-reroll-dice-by-indices-helper (dice indices position)
  (cond
    ((null dice) nil)
    (t (cons (cond
               ((member position indices)
                (read-die-value (car dice)))
               (t
                (car dice)))
             (manually-reroll-dice-by-indices-helper (cdr dice) indices (+ position 1))))))

; Return a new list of dice where the dice at the given indices are rerolled manually
(defun manually-reroll-dice-by-indices (dice indices)
  (manually-reroll-dice-by-indices-helper dice indices 1))

; Helper function for rerolling dice randomly at specified indices
(defun randomly-reroll-dice-by-indices-helper (dice indices position)
  (cond
    ((null dice) nil)
    (t (cons (cond
               ((member position indices)
                (+ 1 (random 6)))
               (t
                (car dice)))
             (randomly-reroll-dice-by-indices-helper (cdr dice) indices (+ position 1))))))

; Return a new list of dice where only the dice at the specified indices are rerolled randomly
(defun randomly-reroll-dice-by-indices (dice indices)
  (randomly-reroll-dice-by-indices-helper dice indices 1))

; Ask the user if they want to reroll randomly or manually
(defun ask-reroll-method ()
  (format t "Do you want to reroll randomly or manually enter the dice? (Enter 'R' for random or 'M' for manual): ")
  (finish-output)
  (read-line))

; Ask the user which dice values they wish to reroll and handle the reroll process
(defun reRoll (dice kept-dice)
  ; Display the kept dice
  (let ((kept-dice-values (kept-dice-values-fn kept-dice dice)))
    (cond
      (kept-dice-values
       (format t "Kept dice: ~{~a ~}~%" kept-dice-values))))

  ; Display current dice
  (format t "Current dice: ~{~a ~}~%" dice)
  (format t "Enter the dice values you wish to reroll (e.g., '(2 4 6)): ")
  (finish-output)

  (let ((userDiceValues (read)))
    ; Validate the input and keep asking until the input is valid
    (multiple-value-bind (valid-p dice-indices)
        (validate-dice-values userDiceValues dice kept-dice)
      (cond
        (valid-p
         (let ((choice (ask-reroll-method)))
           (cond
             ((equal choice "R")
              (let ((new-dice (randomly-reroll-dice-by-indices dice dice-indices))
                    (new-kept-dice (compute-new-kept-dice dice-indices (length dice))))
                (format t "New dice: ~{~a ~}~%" new-dice)
                (values new-dice new-kept-dice)))
             ((equal choice "M")
              (let ((new-dice (manually-reroll-dice-by-indices dice dice-indices))
                    (new-kept-dice (compute-new-kept-dice dice-indices (length dice))))
                (format t "New dice: ~{~a ~}~%" new-dice)
                (values new-dice new-kept-dice)))
             (t
              (format t "Invalid choice. Please enter 'R' or 'M'.~%")
              (reRoll dice kept-dice)))))  ; Recursively call reRoll until the input is valid
        (t
         (format t "Invalid dice values or no available dice to reroll.~%")
         (reRoll dice kept-dice))))))  ; Recursively call reRoll until the input is valid

; Ask the user for a valid Y/N input. Repeat until valid input is received
(defun askInput ()
  (format t "~%Do you want to roll again? (Y/N):~%")
  (finish-output)
  (let ((input (read-line)))
    (cond
      ((equal input "Y") "Y")
      ((equal input "N") "N")
      (t
       (format t "Invalid Input~%")   ; Invalid input, repeat the process
       (askInput)))))  ; Recursive call until valid Y/N is received

; Allows the user to reroll dice up to 2 times
(defun tryreroll (dice scorecard reroll-count kept-dice)
  (cond
    ((< reroll-count 2)
     (let ((userInput (askInput)))  ; Ask if the user wants to reroll
       (finish-output)
       (cond
         ; User chooses to reroll
         ((equal userInput "Y")
          (multiple-value-bind (new-dice new-kept-dice)
              (reRoll dice kept-dice)  ; Pass kept-dice to reRoll
            (format t "You have ~a rerolls left.~%" (- 2 (+ reroll-count 1)))
            ; Recursively call tryreroll with updated reroll count, dice, and kept-dice
            (tryreroll new-dice scorecard (+ reroll-count 1) new-kept-dice)))

         ; User chooses to stand
         ((equal userInput "N")
          (format t "You chose to stand.~%")
          (format t "Available Categories to Score: ~a~%" (available-categories dice))
          dice)  ; Return the current dice

         ; Invalid input, ask again
         (t
          (format t "Invalid input. Please enter 'Y' or 'N'.~%")
          (tryreroll dice scorecard reroll-count kept-dice)))))
    (t
     ; No more rerolls allowed
     (format t "You have used all your rerolls. You must stand now.~%")
     (format t "Available Categories to Score: ~a~%" (available-categories dice))
     dice)))  ; Return the current dice
