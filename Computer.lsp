(load "Scorecard.lsp")
(load "Dice.lsp")

(defun playComputerTurn(scorecard round)

    (format t "Computer's turn~%")

    ; the computer will roll the dice first
    ;; and score the highest available category if any



  (display-scorecard scorecard)  ; Display current scorecard

      ; Read the category and update the scorecard
      (let* 
        ((dice (rollDice))
        (category (highestScoreCategory dice scorecard))
        (decision (computerDecide scorecard dice 1 0 round '()))
        ;(new-scorecard (scoreCategory scorecard dice category 2 round))
        )
        ;(print decision)
        (cond
            ((= (car (last decision)) 1) (format t "~%DECIDED~%"))
            ((= (car (last decision)) 0) (format t "~%Not DECIDED~%"))
        )


        ;((category (highestScoreCategory (available-categories dice))))
        ;(format t "Computer rolled: ~{~a ~}~%" dice)
        ;(format t "Highest Categories to Score: ~a~%" category)
        
         ; Updating the scorecard
        ;(format t "Computer chose category ~a~%" category)
        ;(display-scorecard scorecard) 


        ; scorecard -> (first (first (first decision)))
        ; latest dice (2nd wala ho hai) -> (second (first decision))

        (cond
            ((second (second (first decision)))
            (print "Crash Site")
            (let* ( (newdice (second (first decision)))
                    (category (highestScoreCategory newdice scorecard))
                    (new-scorecard (scoreCategory scorecard newdice category 2 round)))
                    (print newdice)
                new-scorecard))  ;; Return new-scorecard

            (t
            (print "MUJI")
            (first (first (first decision)))))
        ) ; Display the updated scorecard
    ;; will implement reroll functions later on.
)



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
    ((> (getCategoryScore dice (car categories))
         (getCategoryScore dice current-best))
     (findHighestCategory (cdr categories) dice (car categories)))
    ;; Otherwise, recurse with the current best category unchanged
    (t (findHighestCategory (cdr categories) dice current-best))))





(defun computerDecide (scorecard dice numOfRolls isDecided round_no keptDicesInd)
  (cond
    ;; Base case: 
    ((= isDecided 1) (list scorecard isDecided)) ;; if decision made return the scorecard
    ((>= numOfRolls 3) (list scorecard 0)) ;if numOfRolls is greater than or equal to 3, return 0 and the scorecard
    

    ;; If findPattern returns 0, continue with the recursion
    (t 
        (let*
            ((NewScorecard (findPattern scorecard dice round_no keptDicesInd numOfRolls)))
            ;(print "YETA")            
            ;(print NewScorecard)
            ;(print "UTA")  

            (cond
                ((and (numberp (first (first NewScorecard)))
                    (= (first (first NewScorecard)) 1))
                    ;(print "Yeta HORA?")
                    (computerDecide (rest NewScorecard) (car (last (rest NewScorecard))) (+ numOfRolls 1) 1 round_no keptDicesInd)
                )
                (t 
                    (cond
                        ((and (numberp (first (first NewScorecard)))
                            (= (first (first NewScorecard)) 2))
                            (print "DICES KEPT")
                            ; (print (subseq NewScorecard 1 2)) ;scorecard
                            ;(print (car (last (rest NewScorecard)))) ; dicesIndicesToKeep
                            ; (print (first (rest (rest NewScorecard)))) ; dice
                        (computerDecide (subseq NewScorecard 1 2) (first (rest (rest NewScorecard))) (+ numOfRolls 1) isDecided round_no (car (last (rest NewScorecard))))
                        )
                        (t 
                            (cond
                                ;((null NewScorecard) (computerDecide scorecard dice (+ numOfRolls 1) isDecided round_no keptDicesInd))
                                ;(t (computerDecide NewScorecard dice 1 1 round_no keptDicesInd)) ;; scorecard is updated and decision made
                                ((second NewScorecard) (computerDecide NewScorecard (car (last NewScorecard)) (+ numOfRolls 1) isDecided round_no keptDicesInd))
                                (t (computerDecide scorecard dice (+ numOfRolls 1) isDecided round_no keptDicesInd))
                            )
                    ))
            ))
            
            )

    ;; If findPattern returns 1 (pattern found), return 0
    ;; 
    ;; Default case (should not be reached, but added for safety): return 0
)))



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




(defun findPattern (scorecard dice round_no keptDicesInd numOfRolls)
  (format t "PUGEA~%")
  ;(print scorecard)
    (format t "Num of Rolls ~a~% " numOfRolls)
    (format t "Computer rolled: ~{~a ~}~%" dice)
    (cond ((> numOfRolls 1)
        (cond
            ((= (isLowerSectionFilled (first scorecard)) 0)
            ;(print scorecard)
            (format t "Lower section not filled~%")
                (isOfAKind (first scorecard) dice round_no keptDicesInd numOfRolls)
            )  ; Return scorecard after printing
            (t 
                (cond 
                ((= (isUpperSectionFilled (first scorecard)) 0)
                    (format t "Lower section filled~%")
                    (tryUpperSectionFill (first scorecard) dice round_no keptDicesInd numOfRolls))  ; Return scorecard after printing
                (t 
                    (format t "Upper section filled~%")
                    ;(checkSameKind scorecard dice)
                ;(checkSequential scorecard dice)
                    (list (first scorecard) dice)))))               
            )
            
            (t 
                (cond
                    ((= (isLowerSectionFilled scorecard) 0)
                    (print scorecard)
                    (format t "Lower section not filled~%")
                        (isOfAKind scorecard dice round_no keptDicesInd numOfRolls)
                    )  ; Return scorecard after printing
                    (t 
                    (cond 
                    ((= (isUpperSectionFilled scorecard) 0)
                        (format t "Lower section filled~%")
                        (tryUpperSectionFill scorecard dice round_no keptDicesInd numOfRolls))  ; Return scorecard after printing
                    (t 
                        (format t "Upper section filled~%")
                        ;(checkSameKind scorecard dice)
                    ;(checkSequential scorecard dice)
                        (list scorecard dice)))); Return scorecard after printing
                    )
  )
    )
)  


; only returns new scorecard
(defun tryLowerSectionFill (scorecard dice round_no keptDicesInd)
    ; find the available categories, first try isOfAKind, if it works good, if not try isSquential

    (let* ((returnVal (isOfAKind scorecard dice round_no keptDicesInd))) 
        (returnVal) ; cond chai if retrunVal returns turn exit the func by returning approriate value
    
    )
)

(defun tryUpperSectionFill (scorecard dice round_no keptDicesInd numOfRolls)
    ; check the highest score in the dice
    ; if 7 score it, else reroll everything
    (let* ((higestCategoryNo (highestScoreCategory dice scorecard) )
            (scoreOfHigestCategory (getCategoryScore dice higestCategoryNo) ))

        (cond
                ((> scoreOfHigestCategory 7)
                    ; score it
                    (let* ((new-scorecard (scoreCategory scorecard dice higestCategoryNo 2 round_no)) (returnVal (list new-scorecard dice)))
                        ;(display-scorecard new-scorecard)
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
                    (list scorecard newDice)  )
                    )
                
            ))
)


(defun isOfAKind (scorecard dice round_no keptDicesInd numOfRolls)

;if dice combination is Yathzee and Yahtzee available, Score it!
; if full house and it is available, score it

;Check if three of a kind, four of a kind available, if yes -> Needs rerolling
(format t "Yahtzee CHECK~%")
    ;(print scorecard)
    (cond
    ((isCategoryAvailable 12 scorecard numOfRolls)   ;; if Yahtzee is available to score 
        (format t "Yahtzee Available cha!~%")
        (print dice)
                (cond 
                    ((yahtzee-p dice) ; condition
                    (let* ((new-scorecard (scoreCategory scorecard dice 12 2 round_no)) (returnVal (list new-scorecard dice)))
                    ;(display-scorecard new-scorecard)
                    (format t "Yahtzee Scored!~%")
                    (cons '(1) returnVal)
                    )
                    )
                    (t 
                        (format t "Computer needs to try yatzee~%")
                         (cond
                            ((four-of-a-kind-p dice)
                            (print "LA MUji")
                            (let* ((indicesToKeep (giveFourOfaKindIndices dice))
                                    (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                    (val1 (displayKeepMsg dice indicesToKeep))
                                    (newDice (doReRoll dice indicesToReroll)))
                                    (format t "New dice ~{~a~^ ~}~%" newDice )
                                (list scorecard newDice)  ))

                            (t
                            (cond
                                ((three-of-a-kind-p dice)
                                (let* ((indicesToKeep (giveThreeOfaKindIndices dice)))
                                (cond 
                                    ((and (not (null keptDicesInd)) (keptIndicesChecker dice indicesToKeep))
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
                                    (list scorecard newDice)  )))))

                                (t
                                (cond
                                    ((two-of-a-kind-p dice)
                                    ;(print "You are right")
                                    (let* ((indicesToKeep (giveTwoOfaKindIndices dice))
                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                            (val1 (displayKeepMsg dice indicesToKeep))
                                            (newDice (doReRoll dice indicesToReroll)))
                                            (print indicesToKeep)
                                            (print indicesToReroll)
                                            (format t "New dice ~{~a~^ ~}~%" newDice)
                                        (cons '(2) (list scorecard newDice indicesToKeep))  ))

                                    (t
                                        (format t "Sequential CHECK~%")
                                        ;(print scorecard)
                                        (cond
                                        ((isCategoryAvailable 11 scorecard numOfRolls)   ;; if Five Straight is available to score 
                                            (format t "Five Straight Available cha!~%")
                                            (print dice)
                                                    (cond 
                                                        ((five-straight-p dice) ; condition
                                                        (let* ((new-scorecard (scoreCategory scorecard dice 11 2 round_no)) (returnVal (list new-scorecard dice)))
                                                                ;(display-scorecard new-scorecard)
                                                                (format t "Five Straight Scored!~%")
                                                                (cons '(1) returnVal)
                                                                )
                                                        )
                                                        (t 
                                                            (format t "Computer needs to try getting sequential%")
                                                            (cond
                                                                ((four-straight-p dice)
                                                                (let* ( (sequentialValues (isFourSequential dice))
                                                                        (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                                        (val1 (displayKeepMsg dice indicesToKeep))
                                                                        (newDice (doReRoll dice indicesToReroll)))
                                                                        (format t "New dice ~{~a~^ ~}~%" newDice )
                                                                    (list scorecard newDice)  ))

                                                                (t
                                                                (cond
                                                                    ((isThreeSequential dice)
                                                                    ;(print "aye")
                                                                    (print (isThreeSequential dice))
                                                                    (let* ((sequentialValues (isThreeSequential dice))
                                                                            (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                                            (val1 (displayKeepMsg dice indicesToKeep))
                                                                            (newDice (doReRoll dice indicesToReroll)))
                                                                            (format t "New dice ~{~a~^ ~}~%" newDice)
                                                                        (list scorecard newDice)  ))

                                                                    (t
                                                                    (list scorecard dice)))))
                                                        )
                                                    )
                                        )
                                        (t 
                                        (format t "No Five Straight Available!~%")
                                        (format t "Check Four Straight")

                                        (cond

                                        ((isCategoryAvailable 10 scorecard numOfRolls)   ;; if Four Straight is available to score 
                                            (format t "Four Straight Available cha!~%")
                                            (print dice)
                                                    (cond 
                                                        ((four-straight-p dice) ; condition
                                                        (let* ((new-scorecard (scoreCategory scorecard dice 10 2 round_no)) (returnVal (list new-scorecard dice)))
                                                                ;(display-scorecard new-scorecard)
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
                                                                        (list scorecard newDice)  ))

                                                                    (t
                                                                    (cond
                                                                        ((isTwoSequential dice)
                                                                        (let* ((sequentialValues (isThreeSequential dice))
                                                                                (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                                                                (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                                                (val1 (displayKeepMsg dice indicesToKeep))
                                                                                (newDice (doReRoll dice indicesToReroll)))
                                                                                (format t "New dice ~{~a~^ ~}~%" newDice)
                                                                            (list scorecard newDice)  ))

                                                                        (t
                                                                        (format t "NOTHING For Sequence Generation BRO~%") (list scorecard dice)))))

                                                        )
                                                    )
                                            )
                                            (t 
                                            (format t "No Seuence~%")
                                            (format t "Check Full House")
                                                (cond
                                                ((isCategoryAvailable 9 scorecard numOfRolls)   ;; if Full House is available to score 
                                                    (format t "Full House Available cha!~%")
                                                    (print dice)
                                                            (cond 
                                                                ((full-house-p dice) ; condition
                                                                (let* ((new-scorecard (scoreCategory scorecard dice 9 2 round_no)) (returnVal (list new-scorecard dice)))
                                                                        ;(display-scorecard new-scorecard)
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
                                                (format t "No Yahtzee no Seq and no Full House!~%")
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
     (format t "No Yahtzee!~%")
    
    (format t "Sequential CHECK~%")
    ;(print scorecard)
    (cond
    ((isCategoryAvailable 11 scorecard numOfRolls)   ;; if Five Straight is available to score 
        (format t "Five Straight Available cha!~%")
        (print dice)
                (cond 
                    ((five-straight-p dice) ; condition
                    (let* ((new-scorecard (scoreCategory scorecard dice 11 2 round_no)) (returnVal (list new-scorecard dice)))
                            ;(display-scorecard new-scorecard)
                            (format t "Five Straight Scored!~%")
                            (cons '(1) returnVal)
                            )
                    )
                    (t 
                        (format t "Computer needs to try getting sequential%")
                         (cond
                            ((isFourSequential dice)
                            (let* ( (sequentialValues (isFourSequential dice))
                                    (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                    (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                    (val1 (displayKeepMsg dice indicesToKeep))
                                    (newDice (doReRoll dice indicesToReroll)))
                                    (format t "New dice ~{~a~^ ~}~%" newDice )
                                (list scorecard newDice)  ))

                            (t
                            (cond
                                ((isThreeSequential dice)
                                (print "aye")
                                (print (isThreeSequential dice))
                                (let* ((sequentialValues (isThreeSequential dice))
                                        (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                        (val1 (displayKeepMsg dice indicesToKeep))
                                        (newDice (doReRoll dice indicesToReroll)))
                                        (format t "New dice ~{~a~^ ~}~%" newDice)
                                    (list scorecard newDice)  ))

                                (t
                                (cond
                                    ((isTwoSequential dice)
                                    (let* ((sequentialValues (isThreeSequential dice))
                                            (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                            (val1 (displayKeepMsg dice indicesToKeep))
                                            (newDice (doReRoll dice indicesToReroll)))
                                            (format t "New dice ~{~a~^ ~}~%" newDice)
                                        (list scorecard newDice)  ))

                                    (t
                                    (format t "NOTHING For Sequence Generation BRO~%") (list scorecard dice)))))))
                    )
                )
     )
    (t 
     (format t "No Five Straight Available!~%")
     (format t "Check Four Straight")

    (cond

    ((isCategoryAvailable 10 scorecard numOfRolls)   ;; if Four Straight is available to score 
        (format t "Four Straight Available cha!~%")
        (print dice)
                (cond 
                    ((four-straight-p dice) ; condition
                    (let* ((new-scorecard (scoreCategory scorecard dice 10 2 round_no)) (returnVal (list new-scorecard dice)))
                            ;(display-scorecard new-scorecard)
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
                                    (list scorecard newDice)  ))

                                (t
                                (cond
                                    ((isTwoSequential dice)
                                    (let* ((sequentialValues (isThreeSequential dice))
                                            (indicesToKeep (findIndicesOfSequence dice sequentialValues))
                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                            (val1 (displayKeepMsg dice indicesToKeep))
                                            (newDice (doReRoll dice indicesToReroll)))
                                            (format t "New dice ~{~a~^ ~}~%" newDice)
                                        (list scorecard newDice)  ))

                                    (t
                                    (format t "NOTHING For Sequence Generation BRO~%") (list scorecard dice)))))

                    )
                )
        )
        (t 
        (format t "No Seuence~%")
            (format t "Check Full House")

        ; if full house is not scored yet, 
            ; check if 2 pairs of unique dice available for fourofakind
            ; check if dice has three of a kind, if yes reroll the two dices which are not unique
            ; check if dice has two of a kind, if yes reroll the three dices that are not unique
        ; if full house is already scored,
            ; check if four of a kind available, if yes, score it
            ; check if three of a kind available, try getting four of a kind

    (cond
      ; if full house is not scored yet, 
            ; check if 2 pairs of unique dice available for fourofakind
            ; check if dice has three of a kind, if yes reroll the two dices which are not unique
            ; check if dice has two of a kind, if yes reroll the three dices that are not unique
        ; if full house is already scored,
            ; check if four of a kind available, if yes, score it
            ; check if three of a kind available, try getting four of a kind

    ((isCategoryAvailable 9 scorecard numOfRolls)   ;; if Full House is available to score 
        (format t "Full House Available cha!~%")
        (print dice)
                (cond 
                    ((four-straight-p dice) ; condition
                    (let* ((new-scorecard (scoreCategory scorecard dice 9 2 round_no)) (returnVal (list new-scorecard dice)))
                            ;(display-scorecard new-scorecard)
                            (format t "Full House Scored!~%")
                            (cons '(1) returnVal)
                            )
                    )
                    (t 
                        (format t "Seeing if full house possible~%")
                         (cond
                            ((two-of-a-kind-p dice)
                                (cond
                                    ((checkUniqueAmongPairs dice) 
                                        (let* ((indicesToReroll (list (checkUniqueAmongPairs dice)))
                                                (indicesToKeep (custom-remove '(1 2 3 4 5) indicesToReroll))
                                                (val1 (displayKeepMsg dice indicesToKeep))
                                                (newDice (doReRoll dice indicesToReroll)))
                                                (format t "New dice ~{~a~^ ~}~%" newDice)
                                            (list scorecard newDice))                                      
                                    )
                                    (t 
                                        (let* ((indicesToKeep (giveTwoOfaKindIndices dice))
                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                            (val1 (displayKeepMsg dice indicesToKeep))
                                            (newDice (doReRoll dice indicesToReroll)))
                                            (format t "New dice ~{~a~^ ~}~%" newDice)
                                        (list scorecard newDice)  )
                                    )
                                )
                             )

                            (t
                                (cond
                                    ((four-of-a-kind-p dice)
                                    (let* ((indicesToKeep (giveFourOfaKindIndices dice))
                                            (rest indicesToKeep) ; euta indices hataidim
                                            (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                            (val1 (displayKeepMsg dice indicesToKeep))
                                            (newDice (doReRoll dice indicesToReroll)))
                                            (format t "New dice ~{~a~^ ~}~%" newDice)
                                        (list scorecard newDice)  ))

                                    (t
                                        (cond
                                        ((three-of-a-kind-p dice)
                                        (let* ((indicesToKeep (giveThreeOfaKindIndices dice))
                                                (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                (val1 (displayKeepMsg dice indicesToKeep))
                                                (newDice (doReRoll dice indicesToReroll)))
                                                (format t "New dice ~{~a~^ ~}~%" newDice)
                                            (list scorecard newDice)  ))

                                        (t
                                        (format t "Full house possible vayena~%")(list scorecard dice)))))))

                        ; check if dice has four of a kind, if yes reroll the one dice which is not unique
                        ; check if dice has three of a kind, if yes reroll the two dices which are not unique
                        ; check if dice has two numbers that are same, if yes, reroll the three dices that are not unique
                        ; 
                    )
                )
     )
    (t 
     (format t "No Yahtzee no Seq and no Full House!~%")
        ;if 4 of a kind available, check if available on dice, if not, check if 3 of a kind availble and roll the remaining dice
        ;if not, check if 3 of a kind available, check if 3 of a kind available on dice, if yes score it,
        ; if not, reroll all dice.

            (cond
             ((isCategoryAvailable 8 scorecard numOfRolls)  
                (cond
                        ((four-of-a-kind-p dice)
                        (let* ((new-scorecard (scoreCategory scorecard dice 8 2 round_no)) (returnVal (list new-scorecard dice)))
                                                    ;(display-scorecard new-scorecard)
                                                    (format t "Four of a Kind Scored!~%")
                                                    (cons '(1) returnVal)
                                      ))
                        (t
                            (cond
                            ((three-of-a-kind-p dice)
                                (let* ((indicesToKeep (giveThreeOfaKindIndices dice))
                                        (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                        (val1 (displayKeepMsg dice indicesToKeep))
                                        (newDice (doReRoll dice indicesToReroll)))
                                        (format t "New dice ~{~a~^ ~}~%" newDice)
                                    (list scorecard newDice)  ))

                            (t
                            (list scorecard dice)))) ))
                (t
                        (cond
                            ((isCategoryAvailable 7 scorecard numOfRolls)  
                                (cond
                                        ((three-of-a-kind-p dice)
                                        (let* ((new-scorecard (scoreCategory scorecard dice 7 2 round_no)) (returnVal (list new-scorecard dice)))
                                                                    ;(display-scorecard new-scorecard)
                                                                    (format t "Three of a Kind Scored!~%")
                                                                    (cons '(1) returnVal)
                                                    ))
                                        (t
                                            (let* ((indicesToKeep '()) ; rerolling all dices
                                                    (indicesToReroll (custom-remove '(1 2 3 4 5) indicesToKeep))
                                                    (val1 (displayKeepMsg dice indicesToKeep))
                                                    (newDice (doReRoll dice indicesToReroll)))
                                                    (format t "New dice ~{~a~^ ~}~%" newDice)
                                                (list scorecard newDice)  )
                                            )) )
                                            (t
                                                (list scorecard dice)
                                            ))
                 ))
        (list scorecard dice)
     ))
))))


     
     ))
)

; if Yahtzee is not AVAILABLE, 
; if FullHouse if not AVAILABLE


(defun displayKeepMsg(dice indicesToKeep)

    (format t "Computer Decided to keep these dices: ~a~%" (get-dice-values-at-indices dice indicesToKeep))

)
