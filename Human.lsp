(load "Scorecard.lsp")
(load "Dice.lsp")

(defun playHumanTurn (scorecard round)
 
  (format t "~%Your turn~%")
  (display-scorecard scorecard)  ; Display current scorecard
  (terpri)

  

  (let ((dice (rollDice)))
    (format t "You rolled: ~{~a ~}~%" dice)
    (let ((Categories (potentialCategories scorecard dice))))
    (terpri)
    (format t "Available Categories to Score: ~a~%" (available-categories dice scorecard))
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
              ;(display-scorecard new-scorecard)  ; Display the updated scorecard

              new-scorecard)
            )
            (t 
              (format t "No category to score. ~%")
              scorecard
              ))
      ))) ; Return the updated scorecard


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


; Generate a list of numbers from start to end (inclusive)
; (number-sequence 1 5) -> (1 2 3 4 5)
(defun number-sequence (start end)
  (cond
    ((> start end) nil)
    (t (cons start (number-sequence (+ start 1) end)))))




; Create pairs of positions and dice values
; (make-dice-pairs '(1 2 3) '(5 4 6)) ->  ((1 5) (2 4) (3 6))
(defun make-dice-pairs (positions dice)
  (cond
    ((null positions) nil)
    (t (cons (cons (car positions) (car dice))
             (make-dice-pairs (cdr positions) (cdr dice))))))




; Find a matching dice pair for the given value
;(find-matching-dice-pair 
 ;5                                ; The value we're looking for
 ;'((1 . 3) (2 . 5) (3 . 4) (4 . 5))  ; The list of dice pairs (position . value)
 ;'(1 2 3 4)                      ; Available positions
 ;'(3))                           ; Used positions
 ; -> (2 . 5)
; If the function could not find a pair that matched all these criteria, it would eventually return nil.
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

; (match-values-to-dice '(5 2) '(4 4 4 4) '(1 2 3 4)) -> (t (1 2))
; otherwise,  (match-values-to-dice '(5 2) '(4 4 4 4) '(1 2 3 4)) -> (nil nil)
; Try to match the user's desired dice values to available dice positions
(defun match-values-to-dice (values dice available-positions)
  (let ((dice-pairs (make-dice-pairs (number-sequence 1 (length dice)) dice)))
    (match-values-to-dice-helper values dice-pairs available-positions '() '())))

; Validate dice values and map them to indices
; (validate-dice-values '(5 2) '(5 2 4 5) '(3)) -> (t (1 2))
; if no match found, returns nil nil
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
; (kept-dice-values-fn '(2 4) '(5 2 4 6)) -> (2 6)
;  (2 6), which are the values of the dice at positions 2 and 4 in the dice list.
(defun kept-dice-values-fn (kept-dice dice)
  (cond
    ((null kept-dice) nil)
    (t (kept-dice-values-helper kept-dice dice))))



; (compute-new-kept-dice '(2 4) 5) -> (1 3 5) 
; 1 3 5) the dice at these positions are the ones that will be kept and not rerolled.
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
; (manually-reroll-dice-by-indices '(3 6 1 5) '(2 4)) -> (3 4 1 2)
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
  ;(print reroll-count)
  (cond ((= reroll-count 1) (let ((Categories (potentialCategories scorecard dice))) Categories)))
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
          (format t "Available Categories to Score: ~a~%" (available-categories dice scorecard))
          dice)  ; Return the current dice

         ; Invalid input, ask again
         (t
          (format t "Invalid input. Please enter 'Y' or 'N'.~%")
          (tryreroll dice scorecard reroll-count kept-dice)))))
    (t
     ; No more rerolls allowed
     (format t "~%You have used all your rerolls. You must stand now.~%")
     (format t "Available Categories to Score: ~a~%" (available-categories dice scorecard))
     dice)))  ; Return the current dice



(defun humanHelp (scorecard dice kept-dice reroll-count)
  ;(format t "scorecard ~a~%" scorecard)
  ;(format t "dice ~a~%" dice)
  ;(format t "reroll-count ~a~%" reroll-count)
  ;(format t "kept-dice ~a~%" kept-dice) 

; keep on asking unitl you get a Y/N input

  (let* ((userInput (askIfHelpNeeded)))
    (cond ((equal userInput "Y") 
           (findHumanPattern scorecard dice kept-dice reroll-count))
          (t nil))
  )
)




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




;; HUMAN HELP starts here




(defun displayKeepAndRollMsg(dice indicesToKeep indicesToReroll)
    (format t "You may keep these dices: ~a and reroll these ~a~%" (get-dice-values-at-indices dice indicesToKeep) (get-dice-values-at-indices dice indicesToReroll))
  t
)


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

; given a list of available categories, () i want the to return the category with the highest score
;(getCategoryScore dice category-no) returns the score of given category
;(available-categories dice) reutns the available categories
;; available-categories needs to take into account the scores that are already scored

(defun highestScoreCategory (dice scorecard)
  (let ((categories (available-categories dice scorecard)))
    (findHighestCategory categories dice (car categories))))

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


; Function to check if the Upper Section (first 6 categories) is filled
; returns 1 if filled, else 0
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


; returns 1 if filled, else 0
; Function to check if the Lower Section (last 6 categories) is filled
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
                                                                    (list scorecard dice)))))
                                                        )
                                                    )
                                        )
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
                                                                        (list scorecard dice)))))

                                                        )
                                                    )
                                            )
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
                                    (list scorecard dice) ))))))

                        ; check if dice has four of a kind, if yes reroll the one dice which is not unique
                        ; check if dice has three of a kind, if yes reroll the two dices which are not unique
                        ; check if dice has two numbers that are same, if yes, reroll the three dices that are not unique
                        ; 
                    )
                )
     )
    (t 
    ;(print scorecard)
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
                                    (list scorecard dice)))))))
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
                                    (format t "Try pursuing Four Straight since you have three sequential dices.~%")
                                    (let* ((sequentialValues (isTwoSequential dice))
                                            (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                            (val1 (displayKeepAndRollMsg dice indicesToKeep indicesToReroll))
                                            (newDice '()))
                                           
                                        (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                    (t
                                     (list scorecard dice)))))

                    )
                )
        )
        (t 

    (cond

    ((isCategoryAvailable 9 scorecard numOfRolls)   ;; if Full House is available to score 
        ;(format t "Full House Available cha!~%")
        ;(print dice)
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
        (list scorecard dice)
     ))
))))


     
     ))
)