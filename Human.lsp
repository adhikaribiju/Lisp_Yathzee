(load "Scorecard.lsp")
(load "Dice.lsp")


; *********************************************************************
; Function Name: playHumanTurn
; Purpose: This function allows the human player to take their turn in the game.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; round, an integer representing the current round number.
; Return Value: The updated scorecard after the human player has taken their turn.
; Algorithm:
; 1. Roll the dice.
; 2. Display the rolled dice.
; 3. Find the available categories to score based on the rolled dice and the current scorecard.
; 4. If there are available categories to score:
;    a. Ask the user to enter the category number they wish to score.
;    b. Update the scorecard with the chosen category and the score.
; 5. If there are no available categories to score, display a message.
; 6. Return the updated scorecard.
; Reference: none
; *********************************************************************
(defun playHumanTurn (scorecard round)
 
  (format t "~%Your turn~%")
  (display-scorecard scorecard)  ; Display current scorecard
  (terpri)

  (let ((dice (rollDice)))
    (format t "You rolled: ~{~a ~}~%" dice)
    (let ((Categories (potentialCategories scorecard dice))))
    (terpri)
    (format t "~%Available Categories to Score: ~a~%" (available-categories dice scorecard))
            (displayAvailableCategories (available-categories dice scorecard))
    (let ((final-dice (tryreroll dice scorecard 0 nil)))  ; Passing nil for kept-dice
      ; Display the final dice after rerolls
      (format t "Your final dice: ~{~a ~}~%" final-dice)
      
      (cond ((not (null (available-categories final-dice scorecard)))
            (format t "Enter the category you wish to score: ")
            (finish-output)

            ; Read the category and update the scorecard
            (let* ((category (getCategoryNoFromUser scorecard final-dice))
                  (new-scorecard (scoreCategory scorecard final-dice category 1 round)))  ; Updating the scorecard
              (terpri)
              (format t "Category No. ~a scored~%" category)
              new-scorecard)
            )
            (t 
              (format t "No category to score. ~%")
              scorecard
              ))
      ))) ; Return the updated scorecard



; *********************************************************************
; Function Name: getCategoryNoFromUser
; Purpose: This function prompts the user to enter a category number and validates the input.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; dice, a list of integers representing the dice values.
; Return Value: The category number entered by the user.
; Algorithm:
; 1. Prompt the user to enter a category number.
; 2. Read the user input.
; 3. Check if the input is a number.
; 4. Check if the number is in the available categories.
; 5. If the input is not a number or the number is not in the available categories, display an error message and prompt the user again.
; 6. Return the valid category number.
; Reference: none
; *********************************************************************
(defun getCategoryNoFromUser (scorecard dice)
  (finish-output)
  (format t "Enter a category number: ")
  (let ((userInput (read)))
    (cond
      ;; Check if the input is a number
      ((numberp userInput)
       (cond
         ;; Check if the number is in the available categories
         ((member userInput (available-categories dice scorecard))
          ;(format t "User Entered ~a~% " userInput)
          userInput)
         (t
          (format t "Invalid Category. Check Available Categories~%")
          (getCategoryNoFromUser scorecard dice))))
      
      ;; If it's not a number, prompt the user again
      (t
       (format t "Invalid input. Please enter a valid number.~%")
       (getCategoryNoFromUser scorecard dice)))))




; *********************************************************************
; Function Name: number-sequence
; Purpose: Generate a list of numbers from start to end (inclusive).
; Parameters:
; start, a number representing the starting number of the sequence.
; end, a number representing the ending number of the sequence.
; Return Value: A list of numbers from start to end (inclusive).
; Algorithm:
; 1. Check if the start number is greater than the end number.
; 2. If the start number is greater than the end number, return nil.
; 3. Otherwise:
;    a. Append the start number to the result of:
;    b. Recursively calling number-sequence with start+1 and end.
; Reference: none
; *********************************************************************
(defun number-sequence (start end)
  (cond
    ((> start end) nil)
    (t (cons start (number-sequence (+ start 1) end)))))




; *********************************************************************
; Function Name: make-dice-pairs
; Purpose: Create pairs of positions and dice values.
; Parameters:
; positions, a list of numbers representing the positions of the dice.
; dice, a list of numbers representing the values of the dice.
; Return Value: A list of pairs where each pair contains a position and a dice value.
; Algorithm:
; 1. Check if the list of positions is empty.
; 2. If the list of positions is empty, return nil.
; 3. Otherwise:
;    a. Create a pair of the first position and the first dice value.
;    b. Recursively call make-dice-pairs with the rest of the positions and dice values.
;    c. Append the pair to the result of the recursive call.
; Reference: none
; *********************************************************************
(defun make-dice-pairs (positions dice)
  (cond
    ((null positions) nil)
    (t (cons (cons (car positions) (car dice))
             (make-dice-pairs (cdr positions) (cdr dice))))))




; *********************************************************************
; Function Name: find-matching-dice-pair
; Purpose: Find a matching dice pair for the given value.
; Parameters:
; value, a number representing the value we're looking for.
; dice-pairs, a list of pairs where each pair contains a position and a dice value.
; available-positions, a list of available positions.
; used-positions, a list of used positions.
; Return Value: A pair containing the position and value that match the criteria, or nil if no pair is found.
; Algorithm:
; 1. Check if the list of dice pairs is empty.
; 2. If the list of dice pairs is empty, return nil.
; 3. Otherwise:
;    a. Get the first pair from the list.
;    b. Check if the value matches the dice value, the position is available, and the position has not been used.
;    c. If all conditions are met, return the pair.
;    d. Otherwise, recursively call find-matching-dice-pair with the rest of the dice pairs.
; Reference: none
; *********************************************************************
(defun find-matching-dice-pair (value dice-pairs available-positions used-positions)
  (cond
    ((null dice-pairs) nil)
    (t
     (let ((pair (car dice-pairs)))
       (cond
         ((and (equal (cdr pair) value) ; Check if the value matches the dice value
               (member (car pair) available-positions)  ; Check if the position is available
               (not (member (car pair) used-positions)))  ;Check if the position hasn't been used
          pair)
         (t
          (find-matching-dice-pair value (cdr dice-pairs) available-positions used-positions)))))))


; *********************************************************************
; Function Name: match-values-to-dice
; Purpose: Match the user's desired dice values to available dice positions.
; Parameters:
; values, a list of numbers representing the desired dice values.
; dice, a list of numbers representing the values of the dice.
; available-positions, a list of available positions.
; Return Value: A list containing a boolean indicating success and a list of indices if successful, or nil if no match is found.
; Algorithm:
; 1. Create pairs of positions and dice values.
; 2. Call the helper function match-values-to-dice-helper with the values, dice pairs, available positions, used positions, and an empty accumulator.
; Reference: none
; *********************************************************************
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


; *********************************************************************
; Function Name: match-values-to-dice
; Purpose: Match the user's desired dice values to available dice positions.
; Parameters:
; values, a list of numbers representing the desired dice values.
; dice, a list of numbers representing the values of the dice.
; available-positions, a list of available positions.
; Return Value: A list containing a boolean indicating success and a list of indices if successful, or nil if no match is found.
; Algorithm:
; 1. Create pairs of positions and dice values.
; 2. Call the helper function match-values-to-dice-helper with the values, dice pairs, available positions, and an empty accumulator.
; Reference: none
; *********************************************************************
(defun match-values-to-dice (values dice available-positions)
  (let ((dice-pairs (make-dice-pairs (number-sequence 1 (length dice)) dice)))
    (match-values-to-dice-helper values dice-pairs available-positions '() '())))



(defun difference (list1 list2)
  "Returns a list of elements that are in list1 but not in list2."
  (if (null list1)
      nil
      (let ((first-element (first list1))
            (remaining-elements (rest list1)))
        (if (member first-element list2)
            (difference remaining-elements list2)
            (cons first-element (difference remaining-elements list2))))))


; *********************************************************************
; Function Name: validate-dice-values
; Purpose: Validate the user's desired dice values and map them to indices.
; Parameters:
; values, a list of numbers representing the desired dice values.
; dice, a list of numbers representing the values of the dice.
; kept-dice, a list of indices representing the dice that are kept and not rerolled.
; Return Value: A list containing a boolean indicating success and a list of indices if successful, or nil if no match is found.
; Algorithm:
; 1. Calculate the available dice by taking the set difference between the positions of all dice and the kept dice.
; 2. Call the helper function match-values-to-dice with the values, dice, and available dice.
; Reference: none
; *********************************************************************
(defun validate-dice-values (values dice kept-dice)
  (let ((available-dice (difference (number-sequence 1 (length dice)) kept-dice)))
    (multiple-value-bind (success indices)
        (match-values-to-dice values dice available-dice)
      (values success indices))))


; *********************************************************************
; Function Name: kept-dice-values-helper
; Purpose: Helper function to retrieve the values of the kept dice.
; Parameters:
; kept-dice, a list of indices representing the dice that are kept and not rerolled.
; dice, a list of numbers representing the values of the dice.
; Return Value: A list of numbers representing the values of the kept dice.
; Algorithm:
; 1. Check if the list of kept dice is empty.
; 2. If the list of kept dice is empty, return nil.
; 3. Otherwise:
;    a. Get the value of the dice at the position specified by the first element of the kept dice list.
;    b. Recursively call kept-dice-values-helper with the rest of the kept dice list.
;    c. Cons the value to the result of the recursive call.
; Reference: none
; *********************************************************************
(defun kept-dice-values-helper (kept-dice dice)
  (cond
    ((null kept-dice) nil)
    (t (cons (nth (- (car kept-dice) 1) dice)
             (kept-dice-values-helper (cdr kept-dice) dice)))))

; *********************************************************************
; Function Name: kept-dice-values-fn
; Purpose: Retrieve the values of the kept dice based on their indices.
; Parameters:
; kept-dice, a list of indices representing the dice that are kept and not rerolled.
; dice, a list of numbers representing the values of the dice.
; Return Value: A list of numbers representing the values of the kept dice.
; Algorithm:
; 1. Check if the list of kept dice is empty.
; 2. If the list of kept dice is empty, return nil.
; 3. Otherwise, call the helper function kept-dice-values-helper with the kept dice and dice values.
; Example:
; (kept-dice-values-fn '(2 4) '(5 2 4 6)) -> (2 6)
; The result is (2 6), which are the values of the dice at positions 2 and 4 in the dice list.
; Reference: none
; *********************************************************************
(defun kept-dice-values-fn (kept-dice dice)
  (cond
    ((null kept-dice) nil)
    (t (kept-dice-values-helper kept-dice dice))))



; *********************************************************************
; Function Name: compute-new-kept-dice
; Purpose: Compute the new kept dice based on the dice indices not being rerolled.
; Parameters:
; dice-indices, a list of numbers representing the indices of the dice to be rerolled.
; dice-length, a number representing the total number of dice.
; Return Value: A list of numbers representing the indices of the dice that will be kept and not rerolled.
; Algorithm:
; 1. Generate a sequence of numbers from 1 to dice-length.
; 2. Compute the set difference between the generated sequence and the dice-indices.
; 3. Return the resulting list of indices.
; Reference: none
; *********************************************************************
(defun compute-new-kept-dice (dice-indices dice-length)
  (difference (number-sequence 1 dice-length) dice-indices))


; *********************************************************************
; Function Name: read-die-value
; Purpose: Read a valid die value (1-6) from the user.
; Parameters:
; current-value, an integer representing the current value of the die.
; Return Value: A valid die value (1-6) entered by the user.
; Algorithm:
; 1. Prompt the user to enter a new value for the die.
; 2. Read the user input.
; 3. Check if the input is an integer between 1 and 6.
; 4. If the input is valid, return the new value.
; 5. If the input is invalid, display an error message and prompt the user again.
; Reference: none
; *********************************************************************
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




; *********************************************************************
; Function Name: manually-reroll-dice-by-indices-helper
; Purpose: Helper function to reroll dice manually at specified indices.
; Parameters:
; dice, a list of numbers representing the values of the dice.
; indices, a list of numbers representing the indices of the dice to be rerolled.
; position, a number representing the current position in the dice list.
; Return Value: A new list of dice where the dice at the given indices are rerolled manually.
; Algorithm:
; 1. Check if the list of dice is empty.
; 2. If the list of dice is empty, return nil.
; 3. Otherwise:
;    a. Check if the current position is in the list of indices.
;    b. If it is, prompt the user to enter a new value for the die at the current position.
;    c. If it is not, keep the current value of the die.
;    d. Recursively call manually-reroll-dice-by-indices-helper with the rest of the dice and the next position.
; Reference: none
; *********************************************************************
(defun manually-reroll-dice-by-indices-helper (dice indices position)
  (cond
    ((null dice) nil)
    (t (cons (cond
               ((member position indices)
                (read-die-value (car dice)))
               (t
                (car dice)))
             (manually-reroll-dice-by-indices-helper (cdr dice) indices (+ position 1))))))



; *********************************************************************
; Function Name: manually-reroll-dice-by-indices
; Purpose: Reroll dice manually at specified indices.
; Parameters:
; dice, a list of numbers representing the values of the dice.
; indices, a list of numbers representing the indices of the dice to be rerolled.
; Return Value: A new list of dice where the dice at the given indices are rerolled manually.
; Algorithm:
; 1. Call the helper function manually-reroll-dice-by-indices-helper with the dice, indices, and starting position 1.
; Example:
; (manually-reroll-dice-by-indices '(3 6 1 5) '(2 4)) -> (3 4 1 2)
; The result is (3 4 1 2), where the dice at positions 2 and 4 are rerolled manually.
; Reference: none
; *********************************************************************
(defun manually-reroll-dice-by-indices (dice indices)
  (manually-reroll-dice-by-indices-helper dice indices 1))



; *********************************************************************
; Function Name: randomly-reroll-dice-by-indices-helper
; Purpose: Helper function to reroll dice randomly at specified indices.
; Parameters:
; dice, a list of numbers representing the values of the dice.
; indices, a list of numbers representing the indices of the dice to be rerolled.
; position, a number representing the current position in the dice list.
; Return Value: A new list of dice where the dice at the given indices are rerolled randomly.
; Algorithm:
; 1. Check if the list of dice is empty.
; 2. If the list of dice is empty, return nil.
; 3. Otherwise:
;    a. Check if the current position is in the list of indices.
;    b. If it is, generate a random value between 1 and 6 for the die at the current position.
;    c. If it is not, keep the current value of the die.
;    d. Recursively call randomly-reroll-dice-by-indices-helper with the rest of the dice and the next position.
; Reference: none
; *********************************************************************
(defun randomly-reroll-dice-by-indices-helper (dice indices position)
  (cond
    ((null dice) nil)
    (t (cons (cond
               ((member position indices)
                (+ 1 (random 6)))
               (t
                (car dice)))
             (randomly-reroll-dice-by-indices-helper (cdr dice) indices (+ position 1))))))


; *********************************************************************
; Function Name: randomly-reroll-dice-by-indices
; Purpose: Reroll dice randomly at specified indices.
; Parameters:
; dice, a list of numbers representing the values of the dice.
; indices, a list of numbers representing the indices of the dice to be rerolled.
; Return Value: A new list of dice where the dice at the given indices are rerolled randomly.
; Algorithm:
; 1. Call the helper function randomly-reroll-dice-by-indices-helper with the dice, indices, and starting position 1.
; Example:
; (randomly-reroll-dice-by-indices '(3 6 1 5) '(2 4)) -> (3 4 1 2)
; The result is (3 4 1 2), where the dice at positions 2 and 4 are rerolled randomly.
; Reference: none
; *********************************************************************
(defun randomly-reroll-dice-by-indices (dice indices)
  (randomly-reroll-dice-by-indices-helper dice indices 1))



; *********************************************************************
; Function Name: ask-reroll-method
; Purpose: Ask the user if they want to reroll randomly or manually.
; Parameters: None.
; Return Value: The user's choice of reroll method ('R' for random or 'M' for manual).
; Algorithm:
; 1. Prompt the user to enter 'R' for random reroll or 'M' for manual reroll.
; 2. Read the user input.
; 3. Return the user's choice.
; Reference: none
; *********************************************************************
(defun ask-reroll-method ()
  (format t "Do you want to reroll randomly or manually enter the dice? (Enter 'R' for random or 'M' for manual): ")
  (finish-output)
  (read-line))


; *********************************************************************
; Function Name: reRoll
; Purpose: Reroll the dice based on the user's input.
; Parameters:
; dice, a list of numbers representing the values of the dice.
; kept-dice, a list of indices representing the dice that are kept and not rerolled.
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; reroll-count, an integer representing the number of rerolls allowed.
; Return Value: A list of numbers representing the new values of the dice after rerolling.
; Algorithm:
; 1. Display the kept dice if any.
; 2. Display the current dice.
; 3. Ask the user to enter the dice values they wish to reroll.
; 4. Validate the input and keep asking until the input is valid.
; 5. Ask the user if they want to reroll randomly or manually.
; 6. If the user chooses random reroll:
;    a. Reroll the dice at the specified indices randomly.
;    b. Compute the new kept dice based on the indices.
;    c. Display the new dice.
;    d. Return the new dice and the new kept dice.
; 7. If the user chooses manual reroll:
;    a. Reroll the dice at the specified indices manually.
;    b. Compute the new kept dice based on the indices.
;    c. Display the new dice.
;    d. Return the new dice and the new kept dice.
; 8. If the user enters an invalid choice, display an error message and ask again.
; Reference: none
; *********************************************************************
(defun reRoll (dice kept-dice scorecard reroll-count)
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
              (reRoll dice kept-dice scorecard reroll-count)))))  ; Recursively call reRoll until the input is valid
        (t
         (format t "Invalid dice values or no available dice to reroll.~%")
         (reRoll dice kept-dice scorecard reroll-count))))))  ; Recursively call reRoll until the input is valid

; *********************************************************************
; Function Name: askInput
; Purpose: Ask the user if they want to roll again.
; Parameters: None.
; Return Value: The user's choice ('Y' for yes or 'N' for no).
; Algorithm:
; 1. Prompt the user to enter 'Y' for yes or 'N' for no.
; 2. Read the user input.
; 3. Check if the input is 'Y' or 'N'.
; 4. If the input is 'Y' or 'N', return the input.
; 5. If the input is invalid, display an error message and ask again.
; Reference: none
; *********************************************************************
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

; *********************************************************************
; Function Name: tryreroll
; Purpose: Attempt to reroll the dice based on the user's input.
; Parameters:
; dice, a list of numbers representing the values of the dice.
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; reroll-count, an integer representing the number of rerolls allowed.
; kept-dice, a list of indices representing the dice that are kept and not rerolled.
; Return Value: The updated dice after rerolling.
; Algorithm:
; 1. Check if the reroll count is 1.
; 2. If the reroll count is 1:
;    a. Display the available categories to score.
;    b. Ask the user if they want to use help.
;    c. If the user chooses to use help, find the best pattern to score.
;    d. Display the available categories to score.
; 3. Ask the user if they want to reroll.
; 4. If the user chooses to reroll:
;    a. Reroll the dice.
;    b. Update the reroll count.
;    c. Recursively call tryreroll with the updated dice, scorecard, reroll count, and kept dice.
; 5. If the user chooses to stand:
;    a. Display a message indicating the choice to stand.
;    b. Display the available categories to score.
; 6. If the input is invalid, display an error message and ask again.
; Reference: none
; *********************************************************************
(defun tryreroll (dice scorecard reroll-count kept-dice)
  ;(print reroll-count)
  (cond ((= reroll-count 1) (let ((Categories (potentialCategories scorecard dice))) Categories)
          (format t "~%Available Categories to Score: ~a~%" (available-categories dice scorecard))
            (displayAvailableCategories (available-categories dice scorecard))  
        ))
  (terpri)
  (let ((helpAsked (humanHelp scorecard dice kept-dice reroll-count))))
  (cond
    ((< reroll-count 2)
     (let ((userInput (askInput)))  ; Ask if the user wants to reroll
       (finish-output)
       (cond
         ; User chooses to reroll
         ((equal userInput "Y")
          (multiple-value-bind (new-dice new-kept-dice)
              (reRoll dice kept-dice scorecard reroll-count)  ; Pass kept-dice to reRoll
            (format t "You have ~a rerolls left.~%" (- 2 (+ reroll-count 1)))
            ; Recursively call tryreroll with updated reroll count, dice, and kept-dice
            (tryreroll new-dice scorecard (+ reroll-count 1) new-kept-dice)))

         ; User chooses to stand
         ((equal userInput "N")
          (format t "You chose to stand.~%")
          (terpri)
          (format t "Available Categories to Score: ~a~%" (available-categories dice scorecard))
          (displayAvailableCategories (available-categories dice scorecard))
          (terpri)
          dice)  ; Return the current dice

         ; Invalid input, ask again
         (t
          (format t "Invalid input. Please enter 'Y' or 'N'.~%")
          (tryreroll dice scorecard reroll-count kept-dice)))))
    (t
     ; No more rerolls allowed
     (format t "~%You have used all your rerolls. You must stand now.~%")
     dice)))  ; Return the current dice


; *********************************************************************
; Function Name: humanHelp
; Purpose: Ask the user if they want to use help and find the best pattern to score.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; dice, a list of numbers representing the values of the dice.
; kept-dice, a list of indices representing the dice that are kept and not rerolled.
; reroll-count, an integer representing the number of rerolls allowed.
; Return Value: The best pattern to score based on the dice values.
; Algorithm:
; 1. Keep asking the user if they want to use help until a valid input is received.
; 2. If the user chooses to use help:
;    a. Find the best pattern to score based on the dice values and the scorecard.
; Reference: none
; *********************************************************************
(defun humanHelp (scorecard dice kept-dice reroll-count)
; keep on asking unitl you get a Y/N input

  (let* ((userInput (askIfHelpNeeded)))
    (cond ((equal userInput "Y") 
           (findHumanPattern scorecard dice kept-dice reroll-count))
          (t nil))
  )
)



; *********************************************************************
; Function Name: askIfHelpNeeded
; Purpose: Ask the user if they want to use help.
; Parameters: None.
; Return Value: The user's choice ('Y' for yes or 'N' for no).
; Algorithm:
; 1. Prompt the user to enter 'Y' for yes or 'N' for no.
; 2. Read the user input.
; 3. Check if the input is 'Y' or 'N'.
; 4. If the input is 'Y' or 'N', return the input.
; 5. If the input is invalid, display an error message and ask again.
; Reference: none
; *********************************************************************
(defun askIfHelpNeeded ()
  (format t "Do you want to use help? (Y/N):~%")
  (finish-output)
  (let ((input (read-line)))
    (cond
      ((equal input "Y") "Y")
      ((equal input "N") "N")
      (t
       (format t "Invalid Input~%")   ; Invalid input, repeat the process
       (askInput)))))  ; Recursive call until valid Y/N is received



;*********************************************************************
;Source Code for all the Human Help Feature
;*********************************************************************

;*********************************************************************
; Function Name: displayKeepAndRollMsg
; Purpose: Display the message to the user indicating which dice to keep and reroll.
; Parameters:
; dice, a list of numbers representing the values of the dice.
; indicesToKeep, a list of numbers representing the indices of the dice to keep.
; indicesToReroll, a list of numbers representing the indices of the dice to reroll.
; Return Value: T.
; Algorithm:
; 1. Get the values of the dice at the indices to keep and reroll.
; 2. Display the message to the user indicating which dice to keep and reroll.
; Reference: none
;*********************************************************************
(defun displayKeepAndRollMsg(dice indicesToKeep indicesToReroll)
    (format t "You may keep these dices: ~a and reroll these ~a~%" (get-dice-values-at-indices dice indicesToKeep) (get-dice-values-at-indices dice indicesToReroll))
  t
)

;*********************************************************************
; Function Name: findHumanPattern
; Purpose: Find the best pattern to score based on the dice values.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; dice, a list of numbers representing the values of the dice.
; keptDicesInd, a list of indices representing the dice that are kept and not rerolled.
; reroll-count, a number representing the number of rerolls allowed.
; Return Value: The best pattern to score based on the dice values.
; Algorithm:
; 1. Check if the reroll count is 2.
; 2. If the reroll count is 2:
;    a. Find the highest score category for the dice.
;    b. If no category is available, display a message indicating that there are no categories available to score.
;    c. If a category is available, display a message indicating the category that scores the most and that there are no additional rolls left.
; 3. If the reroll count is less than 2:
;    a. Check if the lower section is filled.
;    b. If the lower section is not filled, try to fill the lower section.
;    c. If the lower section is filled:
;       i. Check if the upper section is filled.
;       ii. If the upper section is not filled, try to fill the upper section.
;       iii. If the upper section is filled, return nil.
; Reference: none
;*********************************************************************
(defun findHumanPattern (scorecard dice keptDicesInd reroll-count)
    (cond
    ((>= reroll-count 2) ; if no more reroll left
    
        (let* ((category (highestScoreCategory dice scorecard)))
            (cond ((null category) 
                    (format t "Sorry, there are no categories available to score."))
            (t (format t "Category No.~a scores you the most. You don't have additional roll so it is the best outcome." category))
            )
        )) 
    (t 
        ; if we are here, it means one or two rerolls left
        (cond
            ((= (isLowerSectionFilled (first scorecard)) 0)
                (tryLowerSection scorecard dice keptDicesInd (+ reroll-count 1))) ; try lower section, since it is not filled
            (t 
                (cond 
                ((= (isUpperSectionFilled (first scorecard)) 0)
                    (tryUpperSectionFill scorecard dice keptDicesInd (+ reroll-count 1)))  ; Lower section is filled
                (t 
                  nil)))))))


;*********************************************************************
; Function Name: tryUpperSectionFill
; Purpose: Attempt to fill the upper section based on the dice values.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; dice, a list of numbers representing the values of the dice.
; keptDicesInd, a list of indices representing the dice that are kept and not rerolled.
; numOfRolls, a number representing the number of rolls allowed.
; Return Value: T.
; Algorithm:
; 1. Check the highest score in the dice.
; 2. If the highest score is 7, score it.
; 3. If the highest score is not 7, reroll everything.
; Reference: none
;*********************************************************************
(defun tryUpperSectionFill (scorecard dice keptDicesInd numOfRolls)
    ; check the highest score in the dice
    ; if 7 score it, else reroll everything
    (let* ((higestCategoryNo (highestScoreCategory dice scorecard) )
            (scoreOfHigestCategory (getCategoryScore dice higestCategoryNo) ))
        (cond
                ((> scoreOfHigestCategory 7)
                    ; score it
                    (format t "You can score on Category No. ~a since it scores the most." higestCategoryNo))
                (t 
                    (format t "You may reroll all the dices for a better score.")))))


;*********************************************************************
; Function Name: highestScoreCategory
; Purpose: Find the category with the highest score based on the dice values.
; Parameters:
; dice, a list of numbers representing the values of the dice.
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; Return Value: The category number with the highest score.
; Algorithm:
; 1. Get the available categories.
; 2. Find the highest score category based on the dice values and the scorecard using the helper function findHighestCategory.
; Reference: none
;*********************************************************************
(defun highestScoreCategory (dice scorecard)
  (let ((categories (available-categories dice scorecard)))
    (findHighestCategory categories dice (car categories))))


;*********************************************************************
; Function Name: findHighestCategory
; Purpose: Find the category with the highest score based on the dice values.
; Parameters:
; categories, a list of numbers representing the available categories.
; dice, a list of numbers representing the values of the dice.
; current-best, a number representing the current best category.
; Return Value: The category number with the highest score.
; Algorithm:
; 1. Check if the categories list is empty.
; 2. If the categories list is empty, return the current best category.
; 3. If the score of the current category is higher than the current best, recurse with the current category.
; 4. Otherwise, recurse with the current best category unchanged.
; Reference: none
;*********************************************************************
(defun findHighestCategory (categories dice current-best)
  (cond
    ;; Base case: if categories is empty, return the current best category
    ((null categories) current-best)
    ;; If the score of the current category is higher than the current best, recurse with the current category
    ((>= (getCategoryScore dice (car categories))
         (getCategoryScore dice current-best))
     (findHighestCategory (cdr categories) dice (car categories)))
    ;; Otherwise, recurse with the current best category unchanged
    (t (findHighestCategory (cdr categories) dice current-best))))


;*********************************************************************
; Function Name: getCategoryScore
; Purpose: Get the score for a specific category based on the dice values.
; Parameters:
; dice, a list of numbers representing the values of the dice.
; category, a number representing the category number.
; Return Value: The score for the category based on the dice values.
; Algorithm:
; 1. Check if the category is in the upper section.
; 2. If the category is in the upper section, calculate the score based on the category number.
; 3. If the category is in the lower section, calculate the score based on the category number.
; Reference: none
;*********************************************************************
(defun isUpperSectionFilled (scorecard)
  (labels ((check-upper (lst count)
             (cond
               ((= count 0) 
                 1) ; All upper categories checked
               ((null lst) 
                0) ; Less than 6 elements, not filled
               ((null (second (car lst))) 
                0) ; Current upper score is nil, not filled
               (t 
                (check-upper (cdr lst) (- count 1))))) ; Recurse on the rest
           )
    (check-upper scorecard 6))) ; Initialize with count 6



;*********************************************************************
; Function Name: isLowerSectionFilled
; Purpose: Check if the lower section is filled in the scorecard.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; Return Value: 1 if the lower section is filled, 0 otherwise.
; Algorithm:
; 1. Skip the upper section categories.
; 2. Check if all lower section categories are filled.
; Reference: none
;*********************************************************************
(defun isLowerSectionFilled (scorecard)
  (labels ((skip-upper (lst count)
             (cond
               ((= count 0) 
                lst) ; Skipped first 6 elements
               ((null lst) 
                nil) ; Less than 6 elements, cannot skip
               (t 
                (skip-upper (cdr lst) (- count 1))))) ; Skip one and recurse
           (check-lower (lst count)
             (cond
               ((= count 0) 
                1) ; All lower categories checked
               ((null lst) 
                0) ; Less than 6 elements, not filled
               ((null (second (car lst))) 
                0) ; Current lower score is nil, not filled
               (t 
                (check-lower (cdr lst) (- count 1))))) ; Recurse on the rest
           )
    (let ((lower-section (skip-upper scorecard 6))) ; Skip first 6 for lower section
      (if lower-section
          (check-lower lower-section 6) ; Check the next 6 elements
          0)))) ; If lower-section is nil, return 0







;*********************************************************************
; Function Name: tryLowerSection
; Purpose: Attempt to fill the lower section based on the dice values.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; dice, a list of numbers representing the values of the dice.
; keptDicesInd, a list of indices representing the dice that are kept and not rerolled.
; numOfRolls, a number representing the number of rolls allowed.
; Return Value: T.
; Algorithm:
; 1. Check if Yahtzee is available to score.
;    - If available, verify if the dice form a Yahtzee.
;      - If so, display a message confirming it.
;      - If not, proceed to check other scoring categories (Four of a Kind, Three of a Kind, etc.).
; 2. For each category (Four of a Kind, Three of a Kind, Two of a Kind):
;    - If the category is available, verify the dice pattern.
;      - If matched, check if kept dice indices are valid and display a message to keep/re-roll.
;    - If not matched, move to the next category.
; 3. If no matching category is found:
;    - Check for sequential combinations (Five Straight, Four Straight, or Three Sequential).
;      - Display messages based on available scoring options.
;      - Suggest re-roll strategies based on dice patterns.
; 4. If no higher scoring combinations are available:
;    - Default recommendation is to pursue Yahtzee and display related messages.
;    - Display dice to keep/re-roll accordingly.
; Reference: none
;*********************************************************************

(defun tryLowerSection (scorecard dice keptDicesInd numOfRolls)

    (cond
    ((isCategoryAvailable 12 scorecard numOfRolls)   ;; if Yahtzee is available to score 
                (cond 
                    ((yahtzee-p dice) 
                         (format t "Congrats! Yahtzee is available to score! You may score it!~%")
                    )
                    (t 
                         (cond
                            ((four-of-a-kind-p dice)
                            (let* ((indicesToKeep (giveFourOfaKindIndices dice)))
                            (cond 
                                ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd)))
                                (displayKeepAndRollMsg dice keptDicesInd indicesToReroll)
                                ))
                                (t 
                                    (format t "Try to pursue Yahtzee.~%")
                                    (let* ((indicesToKeep (giveFourOfaKindIndices dice))
                                    (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                    (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                    (newDice '()))
                                   
                                    (cons '(2) (list scorecard newDice indicesToKeep)))))))

                            (t
                            (cond
                                ((three-of-a-kind-p dice)
                                (let* ((indicesToKeep (giveThreeOfaKindIndices dice)))
                                (cond 
                                    ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                    (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd))
                                            (val1 (displayKeepAndRollMsg dice keptDicesInd indicesToReroll))
                                            (newDice '()))
                                    (list scorecard newDice)))
                                    (t 
                                        (format t "Try to pursue Yahtzee.~%")
                                        (let* ((indicesToKeep (giveThreeOfaKindIndices dice))
                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                        (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                        (newDice '()))
                                    (cons '(2) (list scorecard newDice indicesToKeep))  )))))

                                (t
                                (cond
                                    ((two-of-a-kind-p dice)
                                    (let* ((indicesToKeep (giveTwoOfaKindIndices dice)))
                                    (cond 
                                        ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                        (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd))
                                                (val1 (displayKeepAndRollMsg dice keptDicesInd indicesToReroll))
                                                (newDice '()))
                                        (list scorecard newDice)))
                                        (t 
                                            (format t "Try to pursue Yahtzee.~%")
                                            (let* ((indicesToKeep (giveTwoOfaKindIndices dice))
                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                            (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                            (newDice '()))
                                           
                                            (cons '(2) (list scorecard newDice indicesToKeep)))))) )

                                    (t

                                        (cond
                                        ((isCategoryAvailable 11 scorecard numOfRolls)   ;; if Five Straight is available to score 
                                            ;(print dice)
                                                    (cond 
                                                        ((five-straight-p dice) ; condition
                                                        (format t "Five Straight is available to score! You may score that for 40 points.~%")
                                                        )
                                                        (t 
                                                            (cond
                                                                ((four-straight-p dice)
                                                                (format t "Four Straight is available. However, try getting a Five Straight. ~%" )
                                                                (let* ( (sequentialValues (isFourSequential dice))
                                                                        (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                                        (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                                                        (newDice '()))
                                                                    (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                                                (t
                                                                (cond
                                                                    ((isThreeSequential dice)
                                                                    (format t "Try pursuing Five Straight since you have three sequential dices.~%")
                                                                    (let* ((sequentialValues (isThreeSequential dice))
                                                                            (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                                            (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                                                            (newDice '()))
                                                                           
                                                                        (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                                                    (t
                                                                    (list scorecard dice))))))))
                                        (t 

                                        (cond

                                        ((isCategoryAvailable 10 scorecard numOfRolls)   ;; if Four Straight is available to score 
                                                    (cond 
                                                        ((four-straight-p dice) ; condition
                                                         (format t "Four Straight is available to score! Five straight is already filled up, so you might want to score Four Straight now..~%")
                                                        )
                                                        (t 
                                                            (format t "Seeing if Four Straight possible~%")
                                                                    (cond
                                                                    ((isThreeSequential dice)
                                                                     (format t "Try pursuing Four Straight since you have three sequential dices.~%")
                                                                    (let* ((sequentialValues (isThreeSequential dice))
                                                                            (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                                            (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                                                            (newDice '()))
                                                                           
                                                                        (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                                                    (t
                                                                    (cond
                                                                        ((isTwoSequential dice)
                                                                          (format t "Try pursuing Four Straight since you have two sequential dices.~%")
                                                                        (let* ((sequentialValues (isTwoSequential dice))
                                                                                (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                                                                (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                                                (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                                                                (newDice '()))
                                                                            (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                                                        (t
                                                                        (list scorecard dice))))))))
                                            (t 
                                                (cond
                                                ((isCategoryAvailable 9 scorecard numOfRolls)   ;; if Full House is available to score 
                                                    (print dice)
                                                            (cond 
                                                                ((full-house-p dice) ; condition
                                                                 (format t "You may score Full House since it scores the most")
                                                                )
                                                                (t 
                                                                (list scorecard dice)
                                                                )
                                                            )
                                                )
                                                (t 
                                                    (list scorecard dice)
                                                ))
                                    ))))
                                    (list scorecard dice) )))))))))
    (t 
    (cond
    ((isCategoryAvailable 11 scorecard numOfRolls)   ;; if Five Straight is available to score 
                (cond 
                    ((five-straight-p dice) ; condition
                    (format t "Five Straight is available to score! You may score that for 40 points.~%")
                    )
                    (t 
                         (cond
                            ((isFourSequential dice)
                             (format t "Four Straight is available. However, try getting a Five Straight. ~%" )
                            (let* ( (sequentialValues (isFourSequential dice))
                                    (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                    (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                    (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                    (newDice '()))
                                    (cons '(2) (list scorecard newDice indicesToKeep))  ))

                            (t
                            (cond
                                ((isThreeSequential dice)
                                 (format t "Try pursuing Five Straight since you have three sequential dices.~%")
                                (let* ((sequentialValues (isThreeSequential dice))
                                        (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                        (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                        (newDice '()))
                                       
                                        (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                (t
                                (cond
                                    ((isTwoSequential dice)
                                     (format t "Try pursuing Five Straight since you have two sequential dices.~%")
                                    (let* ((sequentialValues (isTwoSequential dice))
                                            (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                            (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                            (newDice '()))
                                           
                                            (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                    (t
                                    (cond
                                  ((isCategoryAvailable 8 scorecard numOfRolls)  
                                      (cond
                                              ((four-of-a-kind-p dice)
                                              (format t "Four of a Kind is available to score! You may score it!~%")
                                                )
                                              (t
                                                  (cond
                                                  ((three-of-a-kind-p dice)
                                                      (let* ((indicesToKeep (giveThreeOfaKindIndices dice)))
                                                      (cond 
                                                          ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                                          (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd))
                                                                  (val1 (displayKeepAndRollMsg dice keptDicesInd indicesToReroll))
                                                                  (newDice '()))
                                                          (list scorecard newDice)))
                                                          (t 
                                                              (format t "Try to pursue Four of a Kind.~%")
                                                              (let* ((indicesToKeep (giveThreeOfaKindIndices dice))
                                                              (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                              (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                                              (newDice '()))
                                                            
                                                          (cons '(2) (list scorecard newDice indicesToKeep))  )))))

                                                  (t
                                                  (list scorecard dice)))) )))))))))
                    )
                )
     )
    (t 
    (cond

    ((isCategoryAvailable 10 scorecard numOfRolls)   ;; if Four Straight is available to score 
                (cond 
                    ((four-straight-p dice) ; condition
                      (format t "Four Straight is available. You may score it since it gives you maximum points. ~%" )
                    )
                    (t 
                                (cond
                                ((isThreeSequential dice)
                                (format t "Try pursuing Four Straight since you have three sequential dices.~%")
                                (let* ((sequentialValues (isThreeSequential dice))
                                        (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                        (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                        (newDice '()))
                                       
                                        
                                    (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                (t
                                (cond
                                    ((isTwoSequential dice)
                                    (format t "Try pursuing Four Straight since you have two sequential dices.~%")
                                    (let* ((sequentialValues (isTwoSequential dice))
                                            (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                            (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                            (newDice '()))
                                           
                                        (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                    (t
                                 (cond
                                ((isCategoryAvailable 8 scorecard numOfRolls)  
                                    (cond
                                            ((four-of-a-kind-p dice)
                                            (format t "Four of a Kind is available to score! You may score it!~%")
                                              )
                                            (t
                                                (cond
                                                ((three-of-a-kind-p dice)
                                                    (let* ((indicesToKeep (giveThreeOfaKindIndices dice)))
                                                    (cond 
                                                        ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                                        (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd))
                                                                (val1 (displayKeepAndRollMsg dice keptDicesInd indicesToReroll))
                                                                (newDice '()))
                                                        (list scorecard newDice)))
                                                        (t 
                                                            (format t "Try to pursue Four of a Kind.~%")
                                                            (let* ((indicesToKeep (giveThreeOfaKindIndices dice))
                                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                            (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                                            (newDice '()))
                                                          
                                                        (cons '(2) (list scorecard newDice indicesToKeep))  )))))

                                                (t
                                                (list scorecard dice)))) )))))))

                    )
                )
        )
        (t 

    (cond

    ((isCategoryAvailable 9 scorecard numOfRolls)   ;; if Full House is available to score 
                (cond 
                    ((full-house-p dice) ; condition
                    (format t "You should score Full House since it scores the most!~%")
                    )
                    (t 
                          (cond
                                ((three-of-a-kind-p dice)
                                (let* ((indicesToKeep (giveThreeOfaKindIndices dice)))
                                (cond 
                                    ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                    (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd))
                                            (val1 (displayKeepAndRollMsg dice keptDicesInd indicesToReroll))
                                            (newDice '()))
                                    (list scorecard newDice)))
                                    (t 
                                        (format t "Try to pursue Full House since threre are three of a kind values.~%")
                                        (let* ((indicesToKeep (giveThreeOfaKindIndices dice))
                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                        (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                        (newDice '()))
                                       
                                    (cons '(2) (list scorecard newDice indicesToKeep))  )))))

                                (t
                                  (cond
                                        ((four-of-a-kind-p dice)
                                        (let* ((indicesToKeep (giveFourOfaKindIndices dice)))
                                        (cond 
                                            ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                            (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd))
                                                    (val1 (displayKeepAndRollMsg dice keptDicesInd indicesToReroll))
                                                    (newDice '()))
                                            (list scorecard newDice)))
                                            (t 
                                                (format t "Try to pursue Full House.~%")
                                                (let* ((indicesToKeep (giveFourOfaKindIndices dice))
                                                (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                                (newDice '()))
                                               
                                                (cons '(2) (list scorecard newDice indicesToKeep)))))))

                                        (t
                                            (cond
                                            ((two-of-a-kind-p dice)
                                                  (format t "Try to pursue Full House.~%")
                                                (cond
                                                    ((checkUniqueAmongPairs dice) 
                                                        (let* ((indicesToReroll (list (checkUniqueAmongPairs dice)))
                                                                (indicesToKeep (custom-remove '(1 2 3 4 5) indicesToReroll))
                                                                (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                                                (newDice '()))
                                                            (cons '(2) (list scorecard newDice indicesToKeep)))                                      
                                                    )
                                                    (t 
                                                        (let* ((indicesToKeep (giveTwoOfaKindIndices dice))
                                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                            (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                                            (newDice '()))
                                                           
                                                        (cons '(2) (list scorecard newDice indicesToKeep))  )
                                                    )
                                                )
                                              )

                                            (t
                                           (list scorecard dice)))))))
                    )
                )
     )
    (t 

            (cond
             ((isCategoryAvailable 8 scorecard numOfRolls)  
                (cond
                        ((four-of-a-kind-p dice)
                         (format t "Four of a Kind is available to score! You may score it!~%")
                          )
                        (t
                            (cond
                            ((three-of-a-kind-p dice)
                                (let* ((indicesToKeep (giveThreeOfaKindIndices dice)))
                                (cond 
                                    ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                    (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd))
                                            (val1 (displayKeepAndRollMsg dice keptDicesInd indicesToReroll))
                                            (newDice '()))
                                    (list scorecard newDice)))
                                    (t 
                                        (format t "Try to pursue Four of a Kind.~%")
                                        (let* ((indicesToKeep (giveThreeOfaKindIndices dice))
                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                        (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                        (newDice '()))
                                       
                                    (cons '(2) (list scorecard newDice indicesToKeep))  )))))

                            (t
                            (list scorecard dice)))) ))
                        (t
                        (cond
                            ((isCategoryAvailable 7 scorecard numOfRolls)  
                                (cond
                                        ((three-of-a-kind-p dice)
                                        (format t "Three of a Kind is available to score! You may score it!~%"))
                                        (t
                                            (let* ((indicesToKeep '()) ; rerolling all dices
                                                    (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                    (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                                    (newDice '()))
                                                   
                                                (cons '(2) (list scorecard newDice indicesToKeep))  )
                                            )) )
                                            (t
                                                 (format t "You may try rerolling all dices for better score!~%")
                                                (list scorecard dice)
                                            ))
                 ))
        (list scorecard dice))))))))))



        