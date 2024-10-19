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
        (new-scorecard (scoreCategory scorecard dice category 2 round))
        )
        ;((category (highestScoreCategory (available-categories dice))))
        (format t "Computer rolled: ~{~a ~}~%" dice)
        (format t "Highest Categories to Score: ~a~%" category)
        
         ; Updating the scorecard
        ;(format t "Computer chose category ~a~%" category)
        ;(display-scorecard scorecard) 
        new-scorecard
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
  (if (null categories)
      current-best
      (let ((current (car categories))
            (rest (cdr categories)))
        (if (> (getCategoryScore dice current)
               (getCategoryScore dice current-best))
            (findHighestCategory rest dice current)
            (findHighestCategory rest dice current-best)))))