(load "Round.lsp")

(defun welcome-msg()
"Presents the menu options."
  (format t "~%Welcome to Yahtzee!~%")
  (format t "~%Please select one of the following~%")
  (format t "1. Start a new game~%")
  (format t "2. Load a saved game~%")
  (format t "3. Exit~%~%")
)


(defun playTournament (player_id)
  (let* ((scorecard
          '(("Aces" nil nil nil)
            ("Twos" nil nil nil)
            ("Threes" nil nil nil)
            ("Fours" nil nil nil)
            ("Fives" nil nil nil)
            ("Sixes" nil nil nil)
            ("Three of a Kind" nil nil nil)
            ("Four of a Kind" nil nil nil)
            ("Full House" nil nil nil)
            ("Four Straight" nil nil nil)
            ("Five Straight" nil nil nil)
            ("Yahtzee" nil nil nil)))
         (round_num 1)  ;; Initialize the first round number
         (updated-scorecard  ;; Initialize updated-scorecard with the result of playRound
          (cond
            ;; Human starts the round
            ((= player_id 1)
             (playRound scorecard 1 round_num))  ;; Play human turn and capture returned scorecard
            ;; Computer starts the round
            ((= player_id 2)
             (playRound scorecard 2 round_num)))))  ;; Play computer turn and capture returned scorecard

    ;;(let* (updated_round_num (update-round-num round_num))  ;; Update round_num
      ;; Start the next consecutive round using the updated scorecard
      (playConsecutiveRound updated-scorecard round_num)
      ;; Display the winner using the updated scorecard
      ;;(display-winner updated-scorecard)
      ;;)
    ))



(defun playConsecutiveRound (scorecard round_num)
  "Continue the game with consecutive rounds until the scorecard is filled."
  (cond
    ;; If the scorecard is filled, end the game
    ((isScorecardFilled scorecard)
     (display-winner scorecard))  ;; Print the game-over message
    ;; Otherwise, keep playing the next round
    (t
      ;(format t "Lowest Score is from: ~a" (calculate-total-scores scorecard) )
      (let* ((round_num (or round_num 1))  ;; Fallback to 1 if round_num is NIL
        (player_id (calculate-total-scores scorecard))  ;; Get the player with the lowest score
            (updated_round_num (update-round-num round_num))  ;; Increment the round number
            (updated_scorecard (cond
                                  ;; If the player_id is 1, play the human's turn
                                  ((= player_id 1) (playRound scorecard 1 updated_round_num))

                                  ;; Otherwise, play the computer's turn
                                  (t (playRound scorecard 2 updated_round_num)))))
       ;; Recursively call playConsecutiveRound with the updated scorecard and round number
       (playConsecutiveRound updated_scorecard updated_round_num)
       ))))


(defun update-round-num (round_num)
  "Increment the round number by 1 and return the updated value."
  (+ round_num 1))
  

(defun displayLoadedGameScores (scorecard)
  (let* ((total-scores (total-scores scorecard))
             (human-score (first total-scores))
             (computer-score (second total-scores)))
        (format t "~%--------------------------------~%")
        (format t "Your Score: ~a~%" human-score)
        (format t "Computer's Score: ~a~%" computer-score)
        (format t "--------------------------------~%~%")))

(defun display-winner (scorecard)
    "Display the winner of the game based on the final scorecard."
    (format t "~%~%Game Over~%")
    (display-scorecard scorecard)

    (let* ((total-scores (total-scores scorecard))
             (human-score (first total-scores))
             (computer-score (second total-scores)))

        (format t "~%~%Your Score: ~a~%" human-score)
        (format t "Computer's Score: ~a~%" computer-score)
        
        (format t "~%--------------------------------~%")     
        (cond
        ((> human-score computer-score)
         (format t "Congratulations! You won the game!~%"))
        ((< human-score computer-score)
         (format t "Computer Wins!~%"))
        (t
        (format t "It's a draw! Both players have the same score.~%"))))
        (format t "--------------------------------~%")
        )
  
(defun askFileName()

  (format t "Enter the name of the file~%")
  (finish-output)
  (let (filename (read-line)))

)

;; Reading from input file:
(defun load-scorecard ()
  (let* ((file-name (read-file-name))  ; Get the file name from the user
         (data (read-file file-name))) ; Attempt to read and parse the file
    (cond
     (data
      (let* ((round-no (first data))
             (values (second data))
             (scorecard
              '(("Aces" nil nil nil)
                ("Twos" nil nil nil)
                ("Threes" nil nil nil)
                ("Fours" nil nil nil)
                ("Fives" nil nil nil)
                ("Sixes" nil nil nil)
                ("Three of a Kind" nil nil nil)
                ("Four of a Kind" nil nil nil)
                ("Full House" nil nil nil)
                ("Four Straight" nil nil nil)
                ("Five Straight" nil nil nil)
                ("Yahtzee" nil nil nil))))
        ;; Recursively update the scorecard and return the final result
        (list round-no (update-scorecard scorecard values 0))))
     (t
      (format t "Error: Failed to read file or file does not exist.~%")))))

(defun read-file-name ()
  (format t "Enter the name of the file: ")
  (finish-output)
  (let ((input (read)))
    (cond
     ;; If input is a symbol, convert it to a string
     ((symbolp input) (string-downcase (symbol-name input)))
     ;; Otherwise, assume it's already a string
     (t input))))

(defun read-file (file-name)
  (handler-case
      (with-open-file (stream file-name :direction :input)
        (read stream)
        )
    (error (e)
      (format t "Error reading file: ~A~%" e)
      nil)))


;; loading the game
(defun UpdateAtIndex (scorecard index new-values)
  (cond
    ((null scorecard) nil)  ; Base case: if the scorecard is empty, return nil
    ((= index 0)
     (let* ((name (car (car scorecard)))
            (score (first new-values))
            (player-id (second new-values))
            (round (third new-values)))
       ;; Print the player-id for debugging
       ;;(format t "~%Processing player-id: ~A~%" player-id) ;; debugging
       ;; Replace "Human" with 1 and "Computer" with 2
       (let ((processed-player-id
              (cond
                ((equal player-id 'Human) 1)
                ((equal player-id 'Computer) 2)
                (t player-id))))
         (cons (cons name (list score processed-player-id round)) 
               (cdr scorecard)))))
    (t
     (cons (car scorecard) 
           (UpdateAtIndex  (cdr scorecard) (- index 1) new-values)))))


(defun update-scorecard (scorecard values index)
  (cond
   ((>= index (length scorecard)) scorecard)  ; Base case: if index exceeds the length, return scorecard
   (t
    (let ((entry (nth index scorecard)))
      ;;(format t "~%Entry at index ~A: ~A~%" index entry)
      (if (and entry (third (nth index values)))  ; Check if there’s a value at this index in `values`
          (update-scorecard (UpdateAtIndex scorecard index (nth index values)) values (1+ index))
          (update-scorecard scorecard values (1+ index)))))))

