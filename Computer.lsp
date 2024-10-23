(load "Scorecard.lsp")
(load "Dice.lsp")


;*********************************************************************
; Function Name: playComputerTurn
; Purpose: To simulate the computer's turn in the Yahtzee game.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; round, an integer representing the current round number.
; Return Value: The updated scorecard after the computer has taken its turn.
; Algorithm:
; 1. Display a message indicating that it is the computer's turn.
; 2. Display the current scorecard.
; 3. Roll the dice.
; 4. Find the category with the highest score based on the rolled dice and the current scorecard.
; 5. If there is a best category found, score it.
; 6. If there is no category, reroll all the dice.
; 7. Repeat steps 3-6 until the computer has taken its turn.
; 8. Return the updated scorecard.
; Reference: none
;*********************************************************************
(defun playComputerTurn(scorecard round)

    (format t "~%~%Computer's turn~%")

  (display-scorecard scorecard)  ; Display current scorecard
  (terpri)
      ; Read the category and update the scorecard
      (let* 
        ((dice (rollDice))
        (category (highestScoreCategory dice scorecard))
        (decision (computerDecide scorecard dice 1 0 round '()))
        )
        (cond
            ((= (car (last decision)) 1)  (first (first decision)))
            ((= (car (last decision)) 0) 
                (cond
                    ((available-categories (second (first decision)) scorecard) ; if there is any scorable category on the dice
                    (let* ( (newdice (second (first decision)))
                            (category (highestScoreCategory newdice scorecard))
                            (new-scorecard (scoreCategory scorecard newdice category 2 round)))
                            (format t "Final Dice: ~a~%" newdice)
                            (format t "Computer scored on Category No: ~a~%" category)
                        new-scorecard))  ;; Return new-scorecard

                    (t
                    (format t "Nothing to score~%")
                    (first (first decision))))))))



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
; Function Name: computerDecide
; Purpose: To make a decision for the computer on whether to keep the dice or reroll based on the current dice values and scorecard.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; dice, a list of numbers representing the values of the dice.
; numOfRolls, a number representing the number of rolls taken so far.
; isDecided, a number representing whether the computer has made a decision (1 for true, 0 for false).
; round_no, a number representing the current round number.
; keptDicesInd, a list of numbers representing the indices of the dice that the computer has decided to keep.
; Return Value: A list containing the updated scorecard and the decision status.
; Algorithm:
; 1. Check if the computer has made a decision.
; 2. If the computer has made a decision, return the scorecard.
; 3. If the number of rolls is greater than or equal to 3, return 0 and the scorecard.
; 4. Otherwise, find the pattern based on the dice values and scorecard using the helper function findPattern.
; 5. If the pattern is 0, continue with the recursion.
; 6. If the pattern is 1, make a decision to keep the dice and recurse with the updated scorecard.
; 7. Otherwise  make a decision to reroll and recurse with the updated dice values.
; Reference: none
;*********************************************************************
(defun computerDecide (scorecard dice numOfRolls isDecided round_no keptDicesInd)
  (cond
    ;; Base case: 
    ((= isDecided 1) (list scorecard isDecided)) ;; if decision made return the scorecard
    ((>= numOfRolls 3) (list scorecard 0)) ;if numOfRolls is greater than or equal to 3, return 0 and the scorecard
    

    ;; If findPattern returns 0, continue with the recursion
    (t 
        (let*
            ((NewScorecard (findPattern scorecard dice round_no keptDicesInd numOfRolls)))
            (cond
                ((and (numberp (first (first NewScorecard)))
                    (= (first (first NewScorecard)) 1))
                    (computerDecide (rest NewScorecard) (car (last (rest NewScorecard))) (+ numOfRolls 1) 1 round_no keptDicesInd)
                )
                (t 
                    (cond
                        ((and (numberp (first (first NewScorecard)))
                            (= (first (first NewScorecard)) 2))
                        (computerDecide (list (second NewScorecard) (third NewScorecard)) (first (rest (rest NewScorecard))) (+ numOfRolls 1) isDecided round_no (car (last (rest NewScorecard))))
                        )
                        (t 
                            (cond
                                ((second NewScorecard) (computerDecide NewScorecard (car (last NewScorecard)) (+ numOfRolls 1) isDecided round_no keptDicesInd))
                                (t (computerDecide scorecard dice (+ numOfRolls 1) isDecided round_no keptDicesInd)))))))))))



;*********************************************************************
; Function Name: isUpperSectionFilled
; Purpose: To check if the Upper Section of the scorecard is filled.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; Return Value: 1 if the Upper Section is filled, 0 otherwise.
; Algorithm:
; 1. Check if the Upper Section is filled by calling the helper function check-upper.
; 2. Initialize the count to 6.
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
; Purpose: To check if the Lower Section of the scorecard is filled.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; Return Value: 1 if the Lower Section is filled, 0 otherwise.
; Algorithm:
; 1. Check if the Lower Section is filled by calling the helper function check-lower.
; 2. Skip the first 6 elements to check the Lower Section.
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
; Function Name: findPattern
; Purpose: To find the pattern in the dice values and scorecard to make a decision for the computer.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; dice, a list of numbers representing the values of the dice.
; round_no, a number representing the current round number.
; keptDicesInd, a list of numbers representing the indices of the dice that the computer has decided to keep.
; numOfRolls, a number representing the number of rolls taken so far.
; Return Value: A list containing the updated scorecard and the dice values.
; Algorithm:
; 1.Find the potential categories based on the dice values.
; 2. Check if the number of rolls is 1. 
; 3. If the number of rolls is greater than 1, check if the Lower Section is filled.
; 4. If the Lower Section is not filled, call the helper function tryLowerSectionFill.
; 5. If the Lower Section is filled, check if the Upper Section is filled.
; 6. If the Upper Section is not filled, call the helper function tryUpperSectionFill.
; 7. If the Upper Section is filled, return the scorecard and dice values.
; Reference: none
;*********************************************************************
(defun findPattern (scorecard dice round_no keptDicesInd numOfRolls)
    (format t "~%~%Roll No. ~a~%" numOfRolls)
    (format t "Computer rolled: ~{~a ~}~%" dice)
    (cond ((= numOfRolls 1) (let ((Categories (potentialCategories scorecard dice))) Categories)))
    (terpri)
    (cond ((> numOfRolls 1)
        (cond
            ((= (isLowerSectionFilled (first scorecard)) 0)
                (isOfAKind (first scorecard) dice round_no keptDicesInd numOfRolls)
            ) 
            (t 
                (cond 
                ((= (isUpperSectionFilled (first scorecard)) 0)
                    (tryUpperSectionFill (first scorecard) dice round_no keptDicesInd numOfRolls))  ; Return scorecard after printing
                (t 
                    (list (first scorecard) dice)))))               
            )
            
            (t 
                (cond
                  ((= (isLowerSectionFilled scorecard) 0)
                        (isOfAKind scorecard dice round_no keptDicesInd numOfRolls)) 
                    (t 
                    (cond 
                    ((= (isUpperSectionFilled scorecard) 0)
                        (tryUpperSectionFill scorecard dice round_no keptDicesInd numOfRolls))  ; Return scorecard after printing
                    (t 
                        (list scorecard dice)))); Return scorecard after printing
                    ))))  


;*********************************************************************
; Function Name: tryLowerSectionFill
; Purpose: To try to fill the Lower Section of the scorecard based on the dice values.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; dice, a list of numbers representing the values of the dice.
; round_no, a number representing the current round number.
; keptDicesInd, a list of numbers representing the indices of the dice that the computer has decided to keep.
; Return Value: A list containing the updated scorecard and the dice values.
; Algorithm:
; 1. Find the available categories.
; 2. First, try isOfAKind.
; 3. If isOfAKind does not work, try isSequential.
; Reference: none
;*********************************************************************
(defun tryLowerSectionFill (scorecard dice round_no keptDicesInd)
    ; find the available categories, first try isOfAKind, if it works good, if not try isSquential
    (let* ((returnVal (isOfAKind scorecard dice round_no keptDicesInd))) 
        (returnVal)))


;*********************************************************************
; Function Name: tryUpperSectionFill
; Purpose: To try to fill the Upper Section of the scorecard based on the dice values.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; dice, a list of numbers representing the values of the dice.
; round_no, a number representing the current round number.
; keptDicesInd, a list of numbers representing the indices of the dice that the computer has decided to keep.
; numOfRolls, a number representing the number of rolls taken so far.
; Return Value: A list containing the updated scorecard and the dice values.
; Algorithm:
; 1. Check the highest score in the dice.
; 2. If the score is greater than 7, score it.
; 3. If the score is less than 7, reroll everything.
; Reference: none
;*********************************************************************
(defun tryUpperSectionFill (scorecard dice round_no keptDicesInd numOfRolls)
    ; check the highest score in the dice
    ; if 7 score it, else reroll everything
    (let* ((higestCategoryNo (highestScoreCategory dice scorecard) )
            (scoreOfHigestCategory (getCategoryScore dice higestCategoryNo) ))

        (cond
                ((> scoreOfHigestCategory 7)
                    ; score it
                    (let* ((new-scorecard (scoreCategory scorecard dice higestCategoryNo 2 round_no)) (returnVal (list new-scorecard dice)))
                        (format t "Category No. ~a Scored!~%" higestCategoryNo)
                        (cons '(1) returnVal)
                        )
                    )
                (t 
                    (let* ((indicesToKeep '()) ; rerolling all dices
                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                        (val1 (displayKeepMsg dice indicesToKeep))
                        (newDice (doReRoll dice indicesToReroll)))
                        (format t "New dice ~{~a~^ ~}~%" newDice)
                    (list scorecard newDice)  )))))


(defun isOfAKind (scorecard dice round_no keptDicesInd numOfRolls)

;if dice combination is Yathzee and Yahtzee available, Score it!
; if full house and it is available, score it
;Check if three of a kind, four of a kind available, if yes -> Needs rerolling
    (cond
    ((isCategoryAvailable 12 scorecard numOfRolls)   ;; if Yahtzee is available to score 
                (cond 
                    ((yahtzee-p dice) ; condition
                    (let* ((new-scorecard (scoreCategory scorecard dice 12 2 round_no)) (returnVal (list new-scorecard dice)))
                    (format t "Yahtzee Scored!~%")
                    (cons '(1) returnVal)
                    )
                    )
                    (t 
                         (cond
                            ((four-of-a-kind-p dice)
                            (let* ((indicesToKeep (giveFourOfaKindIndices dice)))
                            (cond 
                                ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd))
                                        (val1 (displayKeepMsg dice keptDicesInd))
                                        (newDice (doReRoll dice indicesToReroll)))
                                (list scorecard newDice)))
                                (t 
                                    (let* ((indicesToKeep (giveFourOfaKindIndices dice))
                                    (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                    (val1 (displayKeepMsg dice indicesToKeep))
                                    (newDice (doReRoll dice indicesToReroll)))
                                    (format t "New dice ~{~a~^ ~}~%" newDice)
                                    (cons '(2) (list scorecard newDice indicesToKeep)))))))

                            (t
                            (cond
                                ((three-of-a-kind-p dice)
                                (let* ((indicesToKeep (giveThreeOfaKindIndices dice)))
                                (cond 
                                    ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                    (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd))
                                            (val1 (displayKeepMsg dice keptDicesInd))
                                            (newDice (doReRoll dice indicesToReroll)))
                                    (list scorecard newDice)))
                                    (t 
                                        (let* ((indicesToKeep (giveThreeOfaKindIndices dice))
                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                        (val1 (displayKeepMsg dice indicesToKeep))
                                        (newDice (doReRoll dice indicesToReroll)))
                                        (format t "New dice ~{~a~^ ~}~%" newDice)
                                    (cons '(2) (list scorecard newDice indicesToKeep))  )))))

                                (t
                                (cond
                                    ((two-of-a-kind-p dice)
                                    (let* ((indicesToKeep (giveTwoOfaKindIndices dice)))
                                    (cond 
                                        ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                        (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd))
                                                (val1 (displayKeepMsg dice keptDicesInd))
                                                (newDice (doReRoll dice indicesToReroll)))
                                        (list scorecard newDice)))
                                        (t 
                                            (let* ((indicesToKeep (giveTwoOfaKindIndices dice))
                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                            (val1 (displayKeepMsg dice indicesToKeep))
                                            (newDice (doReRoll dice indicesToReroll)))
                                            (format t "New dice ~{~a~^ ~}~%" newDice)
                                            (cons '(2) (list scorecard newDice indicesToKeep)))))) )

                                    (t
                                        (cond
                                        ((isCategoryAvailable 11 scorecard numOfRolls)   ;; if Five Straight is available to score 
                                                    (cond 
                                                        ((five-straight-p dice) ; condition
                                                        (let* ((new-scorecard (scoreCategory scorecard dice 11 2 round_no)) (returnVal (list new-scorecard dice)))
                                                                (format t "Five Straight Scored!~%")
                                                                (cons '(1) returnVal)
                                                                )
                                                        )
                                                        (t 
                                                            (cond
                                                                ((four-straight-p dice)
                                                                (let* ( (sequentialValues (isFourSequential dice))
                                                                        (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                                        (val1 (displayKeepMsg dice indicesToKeep))
                                                                        (newDice (doReRoll dice indicesToReroll)))
                                                                        (format t "New dice ~{~a~^ ~}~%" newDice )
                                                                    (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                                                (t
                                                                (cond
                                                                    ((isThreeSequential dice)
                                                                    (let* ((sequentialValues (isThreeSequential dice))
                                                                            (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                                            (val1 (displayKeepMsg dice indicesToKeep))
                                                                            (newDice (doReRoll dice indicesToReroll)))
                                                                            (format t "New dice ~{~a~^ ~}~%" newDice)
                                                                        (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                                                    (t
                                                                    (list scorecard dice)))))
                                                        )
                                                    )
                                        )
                                        (t 
                                        (cond

                                        ((isCategoryAvailable 10 scorecard numOfRolls)   ;; if Four Straight is available to score 
                                            (format t "Four Straight Available cha!~%")
                                            ;(print dice)
                                                    (cond 
                                                        ((four-straight-p dice) ; condition
                                                        (let* ((new-scorecard (scoreCategory scorecard dice 10 2 round_no)) (returnVal (list new-scorecard dice)))
                                                                (format t "Four Straight Scored!~%")
                                                                (cons '(1) returnVal)
                                                                )
                                                        )
                                                        (t 
                                                            (format t "Seeing if Four Straight possible~%")
                                                                    (cond
                                                                    ((isThreeSequential dice)
                                                                    (let* ((sequentialValues (isThreeSequential dice))
                                                                            (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                                            (val1 (displayKeepMsg dice indicesToKeep))
                                                                            (newDice (doReRoll dice indicesToReroll)))
                                                                            (format t "New dice ~{~a~^ ~}~%" newDice)
                                                                        (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                                                    (t
                                                                    (cond
                                                                        ((isTwoSequential dice)
                                                                        (let* ((sequentialValues (isTwoSequential dice))
                                                                                (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                                                                (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                                                (val1 (displayKeepMsg dice indicesToKeep))
                                                                                (newDice (doReRoll dice indicesToReroll)))
                                                                                (format t "New dice ~{~a~^ ~}~%" newDice)
                                                                            (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                                                        (t
                                                                        (list scorecard dice)))))

                                                        )
                                                    )
                                            )
                                            (t 
                                                (cond
                                                ((isCategoryAvailable 9 scorecard numOfRolls)   ;; if Full House is available to score 
                                                            (cond 
                                                                ((full-house-p dice) ; condition
                                                                (let* ((new-scorecard (scoreCategory scorecard dice 9 2 round_no)) (returnVal (list new-scorecard dice)))
                                                                        (format t "Full House Scored!~%")
                                                                        (cons '(1) returnVal)
                                                                        )
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
                        
                    ))
     )
    (t 
    (cond
    ((isCategoryAvailable 11 scorecard numOfRolls)   ;; if Five Straight is available to score 
                (cond 
                    ((five-straight-p dice) ; condition
                    (let* ((new-scorecard (scoreCategory scorecard dice 11 2 round_no)) (returnVal (list new-scorecard dice)))
                            (format t "Five Straight Scored!~%")
                            (cons '(1) returnVal)
                            )
                    )
                    (t 
                         (cond
                            ((isFourSequential dice)
                            (let* ( (sequentialValues (isFourSequential dice))
                                    (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                    (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                    (val1 (displayKeepMsg dice indicesToKeep))
                                    (newDice (doReRoll dice indicesToReroll)))
                                    (format t "New dice ~{~a~^ ~}~%" newDice )
                                    (cons '(2) (list scorecard newDice indicesToKeep))  ))

                            (t
                            (cond
                                ((isThreeSequential dice)
                                (let* ((sequentialValues (isThreeSequential dice))
                                        (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                        (val1 (displayKeepMsg dice indicesToKeep))
                                        (newDice (doReRoll dice indicesToReroll)))
                                        (format t "New dice ~{~a~^ ~}~%" newDice)
                                        (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                (t
                                (cond
                                    ((isTwoSequential dice)
                                    (let* ((sequentialValues (isTwoSequential dice))
                                            (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                            (val1 (displayKeepMsg dice indicesToKeep))
                                            (newDice (doReRoll dice indicesToReroll)))
                                            (format t "New dice ~{~a~^ ~}~%" newDice)
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
                    (let* ((new-scorecard (scoreCategory scorecard dice 10 2 round_no)) (returnVal (list new-scorecard dice)))
                            (format t "Four Straight Scored!~%")
                            (cons '(1) returnVal)
                            )
                    )
                    (t 
                        (format t "Seeing if Four Straight possible~%")
                                (cond
                                ((isThreeSequential dice)
                                (let* ((sequentialValues (isThreeSequential dice))
                                        (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                        (val1 (displayKeepMsg dice indicesToKeep))
                                        (newDice (doReRoll dice indicesToReroll)))
                                        (format t "New dice ~{~a~^ ~}~%" newDice)
                                        
                                    (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                (t
                                (cond
                                    ((isTwoSequential dice)
                                    (let* ((sequentialValues (isTwoSequential dice))
                                            (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                            (val1 (displayKeepMsg dice indicesToKeep))
                                            (newDice (doReRoll dice indicesToReroll)))
                                            (format t "New dice ~{~a~^ ~}~%" newDice)
                                        (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                    (t
                                     (list scorecard dice)))))

                    )
                )
        )
        (t 

    (cond
      ; if full house is not scored yet, 
            ; check if 2 pairs of unique dice available for fourofakind
            ; check if dice has three of a kind, if yes reroll the two dices which are not unique
            ; check if dice has two of a kind, if yes reroll the three dices that are not unique
        ; if full house is already scored,
            ; check if four of a kind available, if yes, score it
            ; check if three of a kind available, try getting four of a kind

    ((isCategoryAvailable 9 scorecard numOfRolls)   ;; if Full House is available to score 
                (cond 
                    ((full-house-p dice) ; condition
                    (let* ((new-scorecard (scoreCategory scorecard dice 9 2 round_no)) (returnVal (list new-scorecard dice)))
                            (format t "Full House Scored!~%")
                            (cons '(1) returnVal)
                            )
                    )
                    (t 
                          (cond
                                ((three-of-a-kind-p dice)
                                (let* ((indicesToKeep (giveThreeOfaKindIndices dice)))
                                (cond 
                                    ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                    (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd))
                                            (val1 (displayKeepMsg dice keptDicesInd))
                                            (newDice (doReRoll dice indicesToReroll)))
                                    (list scorecard newDice)))
                                    (t 
                                        (let* ((indicesToKeep (giveThreeOfaKindIndices dice))
                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                        (val1 (displayKeepMsg dice indicesToKeep))
                                        (newDice (doReRoll dice indicesToReroll)))
                                        (format t "New dice ~{~a~^ ~}~%" newDice)
                                    (cons '(2) (list scorecard newDice indicesToKeep))  )))))

                                (t
                                  (cond
                                        ((four-of-a-kind-p dice)
                                        (let* ((indicesToKeep (giveFourOfaKindIndices dice)))
                                        (cond 
                                            ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                            (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd))
                                                    (val1 (displayKeepMsg dice keptDicesInd))
                                                    (newDice (doReRoll dice indicesToReroll)))
                                            (list scorecard newDice)))
                                            (t 
                                                (let* ((indicesToKeep (giveFourOfaKindIndices dice))
                                                (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                (val1 (displayKeepMsg dice indicesToKeep))
                                                (newDice (doReRoll dice indicesToReroll)))
                                                (format t "New dice ~{~a~^ ~}~%" newDice)
                                                (cons '(2) (list scorecard newDice indicesToKeep)))))))

                                        (t
                                            (cond
                                            ((two-of-a-kind-p dice)
                                                (cond
                                                    ((checkUniqueAmongPairs dice) 
                                                        (let* ((indicesToReroll (list (checkUniqueAmongPairs dice)))
                                                                (indicesToKeep (custom-remove '(1 2 3 4 5) indicesToReroll))
                                                                (val1 (displayKeepMsg dice indicesToKeep))
                                                                (newDice (doReRoll dice indicesToReroll)))
                                                                (format t "New dice ~{~a~^ ~}~%" newDice)
                                                            (cons '(2) (list scorecard newDice indicesToKeep)))                                      
                                                    )
                                                    (t 
                                                        (let* ((indicesToKeep (giveTwoOfaKindIndices dice))
                                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                            (val1 (displayKeepMsg dice indicesToKeep))
                                                            (newDice (doReRoll dice indicesToReroll)))
                                                            (format t "New dice ~{~a~^ ~}~%" newDice)
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
                        (let* ((new-scorecard (scoreCategory scorecard dice 8 2 round_no)) (returnVal (list new-scorecard dice)))
                                                    (format t "Four of a Kind Scored!~%")
                                                    (cons '(1) returnVal)
                                      ))
                        (t
                            (cond
                            ((three-of-a-kind-p dice)
                                (let* ((indicesToKeep (giveThreeOfaKindIndices dice)))
                                (cond 
                                    ((and (not (null keptDicesInd)) (keptIndicesChecker keptDicesInd indicesToKeep))
                                    (let* ((indicesToReroll (custom-remove '(1 2 3 4 5) keptDicesInd))
                                            (val1 (displayKeepMsg dice keptDicesInd))
                                            (newDice (doReRoll dice indicesToReroll)))
                                    (list scorecard newDice)))
                                    (t 
                                        (let* ((indicesToKeep (giveThreeOfaKindIndices dice))
                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                        (val1 (displayKeepMsg dice indicesToKeep))
                                        (newDice (doReRoll dice indicesToReroll)))
                                        (format t "New dice ~{~a~^ ~}~%" newDice)
                                    (cons '(2) (list scorecard newDice indicesToKeep))  )))))

                            (t
                            (list scorecard dice)))) ))
                        (t
                        (cond
                            ((isCategoryAvailable 7 scorecard numOfRolls)  
                                (cond
                                        ((three-of-a-kind-p dice)
                                        (let* ((new-scorecard (scoreCategory scorecard dice 7 2 round_no)) (returnVal (list new-scorecard dice)))
                                                                    (format t "Three of a Kind Scored!~%")
                                                                    (cons '(1) returnVal)
                                                    ))
                                        (t
                                            (let* ((indicesToKeep '()) ; rerolling all dices
                                                    (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                    (val1 (displayKeepMsg dice indicesToKeep))
                                                    (newDice (doReRoll dice indicesToReroll)))
                                                    (format t "New dice ~{~a~^ ~}~%" newDice)
                                                (cons '(2) (list scorecard newDice indicesToKeep))  )
                                            )) )
                                            (t
                                                (list scorecard dice)
                                            ))
                 ))
        (list scorecard dice))))))))))

;*********************************************************************
; Function Name: displayKeepMsg
; Purpose: To display the dice values that the computer has decided to keep.
; Parameters:
; dice, a list of numbers representing the values of the dice.
; indicesToKeep, a list of numbers representing the indices of the dice that the computer has decided to keep.
; Return Value: None.
; Algorithm:
; 1. Get the dice values at the indices to keep.
; 2. Display the dice values that the computer has decided to keep.
; Reference: none
;*********************************************************************
(defun displayKeepMsg(dice indicesToKeep)
    (format t "Computer Decided to keep these dices: ~a~%" (get-dice-values-at-indices dice indicesToKeep))
)
