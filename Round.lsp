(load "Scorecard.lsp")
(load "Human.lsp")
(load "Computer.lsp")


; *********************************************************************
; Function Name: playRound
; Purpose: This function allows a player to play a round of the Yahtzee game.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; player_id, a number representing the player (1 for human, 2 for computer).
; round, an number representing the current round number.
; Return Value: The updated scorecard after the round has been played.
; Algorithm:
; 1. If the player is the human:
;    a. Allow the human to take their turn.
;    b. Check if the scorecard is filled after the human's turn.
;    c. If the scorecard is filled, ask the user if they wish to save and exit.
;    d. If the scorecard is not filled, proceed to the computer's turn.
; 2. If the player is the computer:
;   a. Allow the computer to take its turn.
;   b. Check if the scorecard is filled after the computer's turn.
;   c. If the scorecard is filled, ask the user if they wish to save and exit.
;   d. If the scorecard is not filled, proceed to the human's turn.
; 3. Return the updated scorecard.
; Reference: none 
; *********************************************************************
(defun playRound (scorecard player_id round)

  (format t "~%Round ~a~%" round)
  (cond
  ;; Human starts first, then computer
  ((= player_id 1)
   (let* ((human-updated-scorecard (playHumanTurn scorecard round)))  ;; Human plays, returns updated scorecard
     (cond 
       ;; Check if the scorecard is filled after the human's turn
       ((isScorecardFilled human-updated-scorecard) 
        (cond ((/= round 1) (ask-save-and-exit human-updated-scorecard round)))
        human-updated-scorecard)  ;; Return the final updated scorecard if filled
       
       (t  ;; Otherwise, proceed to the computer's turn
        (let ((computer-updated-scorecard (playComputerTurn human-updated-scorecard round)))
               (cond ((/= round 1) (ask-save-and-exit computer-updated-scorecard round)))
          computer-updated-scorecard)))))  ;; Return the final updated scorecard

  ;; Computer starts first, then human
  (t
   (let* ((computer-updated-scorecard (playComputerTurn scorecard round)))  ;; Computer plays, returns updated scorecard
     (cond 
       ;; Check if the scorecard is filled after the computer's turn
       ((isScorecardFilled computer-updated-scorecard) 
        (cond ((/= round 1) (ask-save-and-exit computer-updated-scorecard round)))
        computer-updated-scorecard)  ;; Return the final updated scorecard if filled
       
       (t  ;; Otherwise, proceed to the human's turn
        (let ((human-updated-scorecard (playHumanTurn computer-updated-scorecard round)))
               (cond ((/= round 1) (ask-save-and-exit human-updated-scorecard round)))
          human-updated-scorecard))))))  ;; Return the final updated scorecard
)  ;; Return the final updated scorecard


; *********************************************************************
; Function Name: playTurn
; Purpose: This function displays the player's turn.
; Parameters:
; player_id, a number representing the player (1 for human, 2 for computer).
; Return Value: None.
; Algorithm:
; 1. Display the player's turn.
; *********************************************************************
(defun playTurn(player_id)
    (format t "~%~%-----------~%")   
    (format t "~a's turn~%" (get-player player_id))
    (format t "~%-----------~%")   
)

; *********************************************
; Serialization Functions begin here
; *********************************************

; *********************************************************************
; Function Name: ask-save-and-exit
; Purpose: Asks the user if they wish to save the game and exit.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; round, an number representing the current round number.
; Return Value: None.
; Algorithm:
; 1. Ask the user if they wish to save the game and exit.
; 2. If the user chooses to save and exit:
;    a. Serialize the game.
;    b. Exit the program.
; 3. If the user chooses not to save and exit, do nothing.
; 4. If the input is invalid, ask again recursively.
; Reference: none
; *********************************************************************
(defun ask-save-and-exit (scorecard round)
  "Asks the user if they wish to save the game and exit."
(terpri)  
(format t "Do you wish to save the game and exit? (Y/N): ")
  (finish-output)
  (let ((input (read-line)))
    (cond
      ; User chooses to save and exit
      ((or (string= input "Y") (string= input "y"))
       (serialize-game scorecard (+ round 1))
       (finish-output)
       (exit))
      ; User chooses not to save and exit
      ((or (string= input "N") (string= input "n"))
       (finish-output))
      ; Invalid input, ask again recursively
      (t
       (format t "Invalid input. Please enter 'Y' or 'N'.~%")
       (ask-save-and-exit scorecard round)))))



; *********************************************************************
; Function Name: serialize-game
; Purpose: Serialize the game data and save it to a file.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; round, an number representing the current round number.
; Return Value: None.
; Algorithm:
; 1. Prompt the user for the filename.
; 2. Serialize the game data into the desired format.
; 3. Write the data to the file.
; 4. Inform the user that the file was saved successfully.
; Reference: none
; *********************************************************************
(defun serialize-game (scorecard round)
  ; Prompt the user for the filename
  (format t "Enter the name of the file:~%")
  (finish-output)
  (let* ((filename (read-line))
         (data (list
                ; Round number
                round
                ; Scorecard
                (serialize-scorecard scorecard))))
    ; Write the data to the file
    (with-open-file (stream filename
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (print data stream))
    ; Inform the user that the file was saved successfully
    (format t "File saved successfully.~%")
    (finish-output)))

; *********************************************************************
; Function Name: serialize-scorecard
; Purpose: Serialize the scorecard data.
; Parameters:
; scorecard, a list of lists representing the scorecard. Each inner list contains the category number and the score for that category.
; Return Value: A serialized version of the scorecard.
; Algorithm:
; 1. Call the helper function serialize-scorecard-helper with an empty accumulator.
; 2. Reverse the accumulated list to maintain the original order.
; Reference: none
; *********************************************************************
(defun serialize-scorecard (scorecard)
  (serialize-scorecard-helper scorecard '()))

(defun serialize-scorecard-helper (scorecard acc)
  (cond
    ((null scorecard)
     ; Reverse the accumulated list to maintain the original order
     (reverse acc))
    (t
     (let ((entry (car scorecard)))
       (serialize-scorecard-helper (cdr scorecard)
                                   (cons (serialize-scorecard-entry entry) acc))))))


; *********************************************************************
; Function Name: serialize-scorecard-entry
; Purpose: Serialize a single entry in the scorecard.
; Parameters:
; entry, a list representing a single entry in the scorecard. The entry contains the category number, player ID, and turn number.
; Return Value: A serialized version of the entry.
; Algorithm:
; 1. If the entry has a score, player ID, and turn number, return a list with the player ID and turn number.
; 2. If the entry is empty (no score), return a list with a score of 0.
; Reference: none
; *********************************************************************
(defun serialize-scorecard-entry (entry)
  (cond
    ;; If the entry has a score, player ID, and turn number
    ((and (nth 1 entry) (nth 2 entry) (nth 3 entry))
     (list (nth 1 entry)
           (cond 
             ((equal (nth 2 entry) 1) 'Human)
             ((equal (nth 2 entry) 2) 'Computer))
           (nth 3 entry)))
    ;; If the entry is empty (no score)
    (t
     '(0))))