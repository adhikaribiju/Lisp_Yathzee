(load "Dice.lsp")

; *********************************************************************
; Function Name: get-player
; Purpose: This function returns the player name based on the player ID.
; Parameters:
; player-id, a number representing the player ID.
; Return Value: The player name as a string.
; Algorithm:
; 1. Check the player ID.
; 2. Return the corresponding player name.
; Reference: none
; *********************************************************************
(defun get-player (player-id)
  (cond ((equal player-id 1) "You")
        ((equal player-id 2) "Computer")
        (t "X")))


; *********************************************************************
; Function Name: display-row
; Purpose: Display a row of the scorecard.
; Parameters:
; index, a number representing the row number.
; row, a list representing the row of the scorecard.
; Return Value: None.
; Algorithm:
; 1. Extract the category, score, player ID, and round from the row.
; 2. Display the row with proper formatting.
; Reference: none
; *********************************************************************
(defun display-row (index row)
  (let ((category (first row))
        (score (second row))
        (player-id (third row))
        (round (fourth row)))
     (format t "~t~12a~t~35a~t~10a~t~10a~t~10a~%"  
            (+ index 1)            ; Category No.
            category                ; Category name
            (or score "0")        ; Score
            (get-player player-id)  ; Player
            (or round "0"))))     ; Round



; *********************************************************************
; Function Name: display-scorecard-helper
; Purpose: Display the scorecard recursively.
; Parameters:
; scorecard, a list of lists representing the scorecard.
; index, a number representing the row number.
; Return Value: None.
; Algorithm:
; 1. Check if the scorecard is empty.
; 2. If not, display the row and move to the next row.
; 3. Recursively call the function with the rest of the scorecard.
; Reference: none
; *********************************************************************
(defun display-scorecard-helper (scorecard index)
  (cond ((null scorecard) nil)
        (t (display-row index (first scorecard))
           (display-scorecard-helper (rest scorecard) (+ index 1)))))


; *********************************************************************
; Function Name: display-scorecard
; Purpose: Display the scorecard.
; Parameters:
; scorecard, a list of lists representing the scorecard.
; Return Value: None.
; Algorithm:
; 1. Display the header of the scorecard.
; 2. Display the separator line.
; 3. Display the scorecard using a helper function.
; Reference: none
; *********************************************************************
(defun display-scorecard (scorecard)
  (format t "~%Scorecard~%")
  (format t "------------------------------------------------------------------------------~%")
  (format t "~t~10a~t~35a~t~10a~t~10a~t~10a~%" "Category No." "Category" "Score" "Player" "Round")
  (format t "------------------------------------------------------------------------------~%")
  (display-scorecard-helper scorecard 0))


; *********************************************************************
; Function Name: calculate-scores-helper
; Purpose: Calculate the total scores for the human and computer players.
; Parameters:
; scorecard, a list of lists representing the scorecard.
; human-score, a number representing the human player's total score.
; computer-score, a number representing the computer player's total score.
; Return Value: 1 if the human player has a lower score, 2 if the computer player has a lower score, 0 if the scores are equal.
; Algorithm:
; 1. Check if the scorecard is empty.
; 2. If the scorecard is empty, compare the final scores.
; 3. If the scorecard is not empty, process the first entry and move to the next entry.
; Reference: none
; *********************************************************************
(defun calculate-scores-helper (scorecard human-score computer-score)
  (cond
    ;; Base case: If the scorecard is empty, compare the final scores
    ((null scorecard)
     (cond
       ((< human-score computer-score) 1)  ;; Return 1 if human has a lower score
       ((< computer-score human-score) 2)  ;; Return 0 if computer has a lower score
       (t 1)))  ;; Return 1 if they are equal, or human by default

    ;; Recursive case: Process the first element and move on to the rest
    (t
     (let* ((entry (first scorecard))         ;; Get the first entry in the scorecard
            (score (second entry))            ;; The score (second element)
            (player (third entry)))           ;; The player_id (third element)
       (cond
         ;; If score is non-nil and the player is human (1), add to human-score
         ((and score (= player 1)) 
          (calculate-scores-helper (rest scorecard) (+ human-score score) computer-score))
         
         ;; If score is non-nil and the player is computer (2), add to computer-score
         ((and score (= player 2)) 
          (calculate-scores-helper (rest scorecard) human-score (+ computer-score score)))
         
         ;; If score is nil or player ID is invalid, move to the next entry
         (t (calculate-scores-helper (rest scorecard) human-score computer-score)))))))


; *********************************************************************
; Function Name: calculate-total-scores
; Purpose: Calculate the total scores for the human and computer players.
; Parameters:
; scorecard, a list of lists representing the scorecard.
; Return Value: A list containing the total scores for the human and computer players.
; Algorithm:
; 1. Initialize the scores at 0.
; 2. Call the helper function to calculate the scores.
; Reference: none
; *********************************************************************
(defun calculate-total-scores (scorecard)
  (calculate-scores-helper scorecard 0 0))  ;; Initialize scores at 0


; *********************************************************************
; Function Name: total-scores
; Purpose: Calculate the total scores for the human and computer players.
; Parameters:
; scorecard, a list of lists representing the scorecard.
; Return Value: A list containing the total scores for the human and computer players.
; Algorithm:
; 1. Call the helper function to calculate the scores.
; Reference: none
; *********************************************************************
(defun total-scores(scorecard)
    (total-scores-helper scorecard 0 0))


; *********************************************************************
; Function Name: total-scores-helper
; Purpose: Calculate the total scores for the human and computer players.
; Parameters:
; scorecard, a list of lists representing the scorecard.
; human-score, a number representing the human player's total score.
; computer-score, a number representing the computer player's total score.
; Return Value: A list containing the total scores for the human and computer players.
; Algorithm:
; 1. Check if the scorecard is empty.
; 2. If the scorecard is empty, return the total scores.
; 3. If the scorecard is not empty, process the first entry and move to the next entry.
; Reference: none
; *********************************************************************
(defun total-scores-helper (scorecard human-score computer-score)
    (cond
        ((null scorecard) (list human-score computer-score))
        (t
         (let* ((entry (first scorecard))
                (score (second entry))
                (player (third entry)))
         (cond
             ((and score (= player 1))
            (total-scores-helper (rest scorecard) (+ human-score score) computer-score))
             ((and score (= player 2))
            (total-scores-helper (rest scorecard) human-score (+ computer-score score)))
             (t (total-scores-helper (rest scorecard) human-score computer-score)))))))


; *********************************************************************
; Function Name: isScorecardFilled
; Purpose: Check if the scorecard is completely filled.
; Parameters:
; scorecard, a list of lists representing the scorecard.
; Return Value: 1 if the scorecard is filled, 0 otherwise.
; Algorithm:
; 1. Check if the scorecard is empty.
; 2. If the scorecard is empty, it is filled.
; 3. If the scorecard is not empty, check the first entry.
; 4. If the first entry is not filled, the scorecard is not filled.
; 5. If the first entry is filled, check the rest of the scorecard.
; Reference: none
; *********************************************************************
(defun isScorecardFilled (scorecard)
  (cond ((null scorecard) t)  ;; Base case: If the scorecard is empty, it's filled.
        ((null (first (rest (first scorecard)))) nil)  ;; Check if the second element of the first row is nil.
        (t (isScorecardFilled (rest scorecard)))))  ;; Recursively check the rest of the scorecard.


; *********************************************************************
; Function Name: scoreCategory
; Purpose: Score a given category in the scorecard.
; Parameters:
; scorecard, a list of lists representing the scorecard.
; dice, a list of numbers representing the dice roll.
; category-number, a number representing the category to score.
; player_id, a number representing the player ID.
; round, a number representing the current round.
; Return Value: The updated scorecard after scoring the category.
; Algorithm:
; 1. Get the score for the category based on the dice roll.
; 2. Update the scorecard with the new score.
; Reference: none
; *********************************************************************
(defun scoreCategory (scorecard dice category-number player_id round)
    (let* ((score (getCategoryScore dice category-number)))
        (updateCategoryScore scorecard dice category-number score player_id round)
    )
)

;; score a given category in the given scorecard
(defun updateCategoryScore (scorecard dice category-number score player_id round)
  (cond
    ;; Base case: If the scorecard is empty, return an empty list (this should never happen with valid input)
    ((null scorecard) nil)

    ;; If the category-number is 0 (meaning we've reached the target category)
    ((= category-number 1)
     ;; Update the score (second element) to 10 in the first entry
     (let* ((entry (car scorecard)))
       (cons (list (car entry) score player_id round) ;; Update the second element to 10
             (cdr scorecard))))  ;; Return the rest of the scorecard unchanged

    ;; Otherwise, keep searching recursively through the list
    (t (cons (car scorecard)  ;; Keep the current entry unchanged
             (updateCategoryScore (cdr scorecard) dice (- category-number 1) score player_id round))))) ;; Recursively update the rest



;; Avaliable categories to score given a dice roll

(defun available-categories (dice scorecard)
  "Return a list of available categories (1 to 12) based on the dice roll and the scorecard."
  (filter-categories
   (append
    (cond ((> (count-dice dice 1) 0) '(1)) (t '()))  ;; Aces
    (cond ((> (count-dice dice 2) 0) '(2)) (t '()))  ;; Twos
    (cond ((> (count-dice dice 3) 0) '(3)) (t '()))  ;; Threes
    (cond ((> (count-dice dice 4) 0) '(4)) (t '()))  ;; Fours
    (cond ((> (count-dice dice 5) 0) '(5)) (t '()))  ;; Fives
    (cond ((> (count-dice dice 6) 0) '(6)) (t '()))  ;; Sixes
    (cond ((three-of-a-kind-p dice) '(7)) (t '()))   ;; Three of a Kind
    (cond ((four-of-a-kind-p dice) '(8)) (t '()))    ;; Four of a Kind
    (cond ((full-house-p dice) '(9)) (t '()))        ;; Full House
    (cond ((four-straight-p dice) '(10)) (t '()))    ;; Four Straight
    (cond ((five-straight-p dice) '(11)) (t '()))    ;; Five Straight
    (cond ((yahtzee-p dice) '(12)) (t '())))         ;; Yahtzee
   scorecard))

(defun filter-categories (categories scorecard)
  "Recursively filters out categories that are already filled in the scorecard."
  (cond
   ((null categories) '())  ;; Base case: no more categories to check
   ((category-filled-p (first categories) scorecard)
    (filter-categories (rest categories) scorecard))  ;; Skip if filled
   (t
    (cons (first categories) (filter-categories (rest categories) scorecard)))))  ;; Include if not filled

(defun category-filled-p (category scorecard)
  "Check if a given category is filled in the scorecard."
  (let ((score (second (nth (- category 1) scorecard))))  ;; Access the second element of the scorecard entry
    (not (null score))))  ;; Return T if filled (non-NIL value), NIL if not


(defun count-dice (dice number)
  "Count how many dice match a specific number."
  (cond
    ((null dice) 0)  ;; Base case: no dice left
    ((= (car dice) number) (+ 1 (count-dice (cdr dice) number)))  ;; Count this die and recurse
    (t (count-dice (cdr dice) number))))  ;; Move to the next die


(defun two-of-a-kind-p (dice)
  "Check if there are at least three dice with the same value."
  (two-of-a-kind-helper dice dice))

(defun two-of-a-kind-helper (dice original-dice)
  (cond
    ((null dice) nil)  ;; No match found
    ((>= (count-dice original-dice (car dice)) 2) t)  ;; Found three or more of a kind
    (t (two-of-a-kind-helper (cdr dice) original-dice))))  ;; Recurse


(defun three-of-a-kind-p (dice)
  "Check if there are at least three dice with the same value."
  (three-of-a-kind-helper dice dice))

(defun three-of-a-kind-helper (dice original-dice)
  (cond
    ((null dice) nil)  ;; No match found
    ((>= (count-dice original-dice (car dice)) 3) t)  ;; Found three or more of a kind
    (t (three-of-a-kind-helper (cdr dice) original-dice))))  ;; Recurse

(defun four-of-a-kind-p (dice)
  "Check if there are at least four dice with the same value."
  (four-of-a-kind-helper dice dice))

(defun four-of-a-kind-helper (dice original-dice)
  (cond
    ((null dice) nil)  ;; No match found
    ((>= (count-dice original-dice (car dice)) 4) t)  ;; Found four or more of a kind
    (t (four-of-a-kind-helper (cdr dice) original-dice))))  ;; Recurse

(defun full-house-p (dice)
  "Check if the dice form a full house (three of one number and two of another)."
  (full-house-helper dice nil nil))

(defun full-house-helper (dice unique-1 unique-2)
  (cond
    ((null dice)
     ;; Check both combinations for a full house
     (or (and unique-1 unique-2 (= (length unique-1) 3) (= (length unique-2) 2))
         (and unique-1 unique-2 (= (length unique-1) 2) (= (length unique-2) 3))))
    ((null unique-1)  ;; First unique set
     (full-house-helper (cdr dice) (list (car dice)) unique-2))
    ((and (null unique-2) (not (member (car dice) unique-1)))  ;; Second unique set
     (full-house-helper (cdr dice) unique-1 (list (car dice))))
    ((member (car dice) unique-1)  ;; Add to first unique set
     (full-house-helper (cdr dice) (cons (car dice) unique-1) unique-2))
    ((member (car dice) unique-2)  ;; Add to second unique set
     (full-house-helper (cdr dice) unique-1 (cons (car dice) unique-2)))
    (t nil)))  ;; If not a valid full house

(defun four-straight-p (dice)
  "Check if the dice form a sequence of four consecutive numbers."
  (let ((visited (sort-visited-dice dice)))
    (four-straight-helper visited)))

(defun insert-in-order (element sorted-list)
  (cond
    ((null sorted-list) (list element))  ;; If the sorted list is empty, return a list with the element
    ((<= element (car sorted-list)) (cons element sorted-list))  ;; Insert element in the correct position
    (t (cons (car sorted-list) (insert-in-order element (cdr sorted-list))))))  ;; Continue searching for the right spot

(defun sort-visited-dice (dice)
  (cond
    ((null dice) '())  ;; Base case: if the list is empty, return an empty list
    (t (insert-in-order (car dice) (sort-visited-dice (cdr dice))))))  ;; Insert the first element into the sorted rest


(defun four-straight-helper (visited)
  (cond
    ((< (length visited) 4) nil)  ;; Not enough dice to form a straight
    ((and (= (second visited) (+ (car visited) 1))
          (= (third visited) (+ (second visited) 1))
          (= (fourth visited) (+ (third visited) 1))) t)  ;; Found four straight
    (t (four-straight-helper (cdr visited)))))  ;; Recurse to the next set of dice

(defun five-straight-p (dice)
  "Check if the dice form a sequence of five consecutive numbers."
  (let ((visited (sort-visited-dice dice)))
    (cond
      ((< (length visited) 5) nil)  ;; Not enough dice
      ((and (= (second visited) (+ (car visited) 1))
            (= (third visited) (+ (second visited) 1))
            (= (fourth visited) (+ (third visited) 1))
            (= (fifth visited) (+ (fourth visited) 1))) t)
      (t nil))))

(defun yahtzee-p (dice)
  "Check if all five dice are the same."
  (yahtzee-helper dice (car dice)))

(defun yahtzee-helper (dice first)
  "Helper function for Yahtzee to check if all dice match the first die."
  (cond
    ((null dice) t)  ;; Base case: all dice checked
    ((= (car dice) first) (yahtzee-helper (cdr dice) first))  ;; Continue checking
    (t nil)))  ;; Found a mismatch





;; get score of the given category

(defun getCategoryScore (dice category-no)
  "Return the score for a given category based on the dice roll and category number."
  (cond
    ((null category-no) (print dice))
    ;; Categories 1 to 6: Sum of specific dice (Aces, Twos, Threes, etc.)
    ((= category-no 1) (sum-dice dice 1))  ;; Aces
    ((= category-no 2) (sum-dice dice 2))  ;; Twos
    ((= category-no 3) (sum-dice dice 3))  ;; Threes
    ((= category-no 4) (sum-dice dice 4))  ;; Fours
    ((= category-no 5) (sum-dice dice 5))  ;; Fives
    ((= category-no 6) (sum-dice dice 6))  ;; Sixes
    
    ;; Category 7: Three of a Kind - Sum of all dice
    ((= category-no 7) (sum-all-dice dice))

    ;; Category 8: Four of a Kind - Sum of all dice
    ((= category-no 8) (sum-all-dice dice))

    ;; Category 9: Full House - Fixed score of 25
    ((= category-no 9) 25)

    ;; Category 10: Four Straight - Fixed score of 30
    ((= category-no 10) 30)

    ;; Category 11: Five Straight - Fixed score of 40
    ((= category-no 11) 40)

    ;; Category 12: Yahtzee - Fixed score of 50
    ((= category-no 12) 50)

    ;; Default case, if category is invalid
    (t 0)))


(defun sum-dice (dice number)
  "Return the sum of dice that match the given number."
  (cond
    ((null dice) 0)  ;; Base case: no dice left
    ((= (car dice) number) (+ (car dice) (sum-dice (cdr dice) number)))  ;; Add matching die
    (t (sum-dice (cdr dice) number))))  ;; Skip non-matching die

(defun sum-all-dice (dice)
  "Return the sum of all dice."
  (cond
    ((null dice) 0)  ;; Base case: no dice left
    (t (+ (car dice) (sum-all-dice (cdr dice))))))  ;; Add each die value


; given the categoryNo, check if category is available to score
(defun isCategoryAvailable (category-index scorecard numOfRolls)

  (cond ((>= numOfRolls 1) 
    (let ((category (nth (- category-index 1) scorecard)))
      (cond
        ((null (second category)) t)  ;; If the score is nil, return true (t)
        (t nil)))
  )
  (t (isCategoryAvailable category-index (first scorecard) 1))))        
              ;; Otherwise, return false (nil)

;





(defun potentialCategories (scorecard dice)
  (let* ((availCategories (available-categories dice scorecard)))
    (format t "~%~%Potential Categories~%~%")
    
    ;; Check and display number categories if available using separate cond statements
    (cond ((member 1 availCategories) (format t "Aces~%")))
    (cond ((member 2 availCategories) (format t "Twos~%")))
    (cond ((member 3 availCategories) (format t "Threes~%")))
    (cond ((member 4 availCategories) (format t "Fours~%")))
    (cond ((member 5 availCategories) (format t "Fives~%")))
    (cond ((member 6 availCategories) (format t "Sixes~%")))
    
    ;; Additional category checks based on dice conditions
    (cond
     ((two-of-a-kind-p dice)
      (cond
       ((isCategoryAvailable 7 scorecard 1) (format t "Three of a Kind~%")))
      (cond
       ((isCategoryAvailable 8 scorecard 1) (format t "Four of a Kind~%")))
      (cond
       ((isCategoryAvailable 9 scorecard 1) (format t "Full House~%")))
      (cond
       ((isCategoryAvailable 12 scorecard 1) (format t "Yahtzee~%")))))
    
    (cond
     ((isTwoSequential dice)
      (cond
       ((isCategoryAvailable 10 scorecard 1) (format t "Four Straight~%")))
      (cond
       ((isCategoryAvailable 11 scorecard 1) (format t "Five Straight~%")))))))


(defun displayAvailableCategories (numbers)
  (cond ((member 1 numbers) (format t "Aces~%")))
  (cond ((member 2 numbers) (format t "Twos~%")))
  (cond ((member 3 numbers) (format t "Threes~%")))
  (cond ((member 4 numbers) (format t "Fours~%")))
  (cond ((member 5 numbers) (format t "Fives~%")))
  (cond ((member 6 numbers) (format t "Sixes~%")))
  (cond ((member 7 numbers) (format t "Three of a Kind~%")))
  (cond ((member 8 numbers) (format t "Four of a Kind~%")))
  (cond ((member 9 numbers) (format t "Full House~%")))
  (cond ((member 10 numbers) (format t "Four Straight~%")))
  (cond ((member 11 numbers) (format t "Five Straight~%")))
  (cond ((member 12 numbers) (format t "Yahtzee~%"))))




(defun potentialCategoriesComp (scorecard dice)
  (let* ((availCategories (available-categories dice scorecard)))
    (format t "~%~%Potential Categories~%~%")
    
    ;; Check and display number categories if available using separate cond statements
    (cond ((member 1 availCategories) (format t "Aces~%")))
    (cond ((member 2 availCategories) (format t "Twos~%")))
    (cond ((member 3 availCategories) (format t "Threes~%")))
    (cond ((member 4 availCategories) (format t "Fours~%")))
    (cond ((member 5 availCategories) (format t "Fives~%")))
    (cond ((member 6 availCategories) (format t "Sixes~%")))
    
    ;; Additional category checks based on dice conditions
    (cond
     ((two-of-a-kind-p dice)
      (cond
       ((isCategoryAvailable 7 scorecard 1) (format t "Three of a Kind~%")))
      (cond
       ((isCategoryAvailable 8 scorecard 1) (format t "Four of a Kind~%")))
      (cond
       ((isCategoryAvailable 9 scorecard 1) (format t "Full House~%")))
      (cond
       ((isCategoryAvailable 12 scorecard 1) (format t "Yahtzee~%")))))
    
    (cond
     ((isTwoSequential dice)
      (cond
       ((isCategoryAvailable 10 scorecard 1) (format t "Four Straight~%")))
      (cond
       ((isCategoryAvailable 11 scorecard 1) (format t "Five Straight~%")))))))

