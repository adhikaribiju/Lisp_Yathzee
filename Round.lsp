(load "Scorecard.lsp")
(load "Human.lsp")
(load "Computer.lsp")

;    ((isScorecardFilled scorecard)
     ;(display-winner scorecard))  ;; Print the game-over message



(defun playRound (scorecard player_id round)

  (cond
       ((/= round 1) (ask-save-and-exit scorecard round))
  )
  (format t "~%Round ~a~%" round)
  (cond
  ;; Human starts first, then computer
  ((= player_id 1)
   (let* ((human-updated-scorecard (playHumanTurn scorecard round)))  ;; Human plays, returns updated scorecard
     (cond 
       ;; Check if the scorecard is filled after the human's turn
       ((isScorecardFilled human-updated-scorecard) 
        human-updated-scorecard)  ;; Return the final updated scorecard if filled
       
       (t  ;; Otherwise, proceed to the computer's turn
        (let ((computer-updated-scorecard (playComputerTurn human-updated-scorecard round)))
          computer-updated-scorecard)))))  ;; Return the final updated scorecard

  ;; Computer starts first, then human
  (t
   (let* ((computer-updated-scorecard (playComputerTurn scorecard round)))  ;; Computer plays, returns updated scorecard
     (cond 
       ;; Check if the scorecard is filled after the computer's turn
       ((isScorecardFilled computer-updated-scorecard) 
        computer-updated-scorecard)  ;; Return the final updated scorecard if filled
       
       (t  ;; Otherwise, proceed to the human's turn
        (let ((human-updated-scorecard (playHumanTurn computer-updated-scorecard round)))
          human-updated-scorecard))))))  ;; Return the final updated scorecard
)  ;; Return the final updated scorecard


; Need a function that checks if the scorecard is full
; Need a function that returns the player with the smaller score
;
; playRound function
; For the first round, play the player_id turn first and play the other player's turn next
; For any other round, play the player with the smaller score first, and the other player next


(defun playTurn(player_id)
    (format t "~%~%-----------~%")   
    (format t "~a's turn~%" (get-player player_id))
    (format t "~%-----------~%")   
)

;; Serialization functions
(defun ask-save-and-exit (scorecard round)
  "Asks the user if they wish to save the game and exit."
  (format t "Do you wish to save the game and exit? (Y/N): ")
  (finish-output)
  (let ((input (read-line)))
    (cond
      ; User chooses to save and exit
      ((or (string= input "Y") (string= input "y"))
       (serialize-game scorecard round)
       (finish-output)
       (exit))
      ; User chooses not to save and exit
      ((or (string= input "N") (string= input "n"))
       (finish-output))
      ; Invalid input, ask again recursively
      (t
       (format t "Invalid input. Please enter 'Y' or 'N'.~%")
       (ask-save-and-exit scorecard round)))))




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

; Function to serialize the scorecard into the desired format
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


; Function to serialize a single scorecard entry
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