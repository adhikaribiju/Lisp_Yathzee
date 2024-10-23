(load "Tournament.lsp")
(load "Round.lsp")
(load "Dice.lsp")
(load "Scorecard.lsp")
(load "Human.lsp")
(load "Computer.lsp")

; *********************************************************************
; Function Name: start-yahtzee
; Purpose: This function displays the welcome message and prompts the user to start a new game, load a saved game, or exit the game.
; Parameters: None
; Return Value: None
; Algorithm:
; 1. Display the welcome message.
; 2. Prompt the user to enter their choice.
; 3. Based on the user's choice:
;    a. If the user chooses to start a new game, call the start-game function.
;    b. If the user chooses to load a saved game, call the load-game function.
;    c. If the user chooses to exit the game, call the exit-game function.
;    d. If the user enters an invalid choice, display an error message and prompt the user again.
; Reference: none
; *********************************************************************
(defun start-yahtzee ()
  (welcome-msg)
  (princ "Enter your choice: ")
  (finish-output)
  (let* ((user-input (read-line)))
    (cond
      ((string= user-input "1")
       (start-game))
      ((string= user-input "2")
       (load-game))
      ((string= user-input "3")
       (exit-game))
      (t
       (format t "~%Invalid choice. Please try again.~%")
       (start-yahtzee))))
)

; *********************************************************************
; Function Name: start-game
; Purpose: This function starts a new game and randomly generates dice rolls for the human and computer players.
; Parameters: None
; Return Value: None
; Algorithm:
; 1. Display a message indicating that the dice are being rolled to determine who starts the round.
; 2. Prompt the user to choose between manually rolling the dice or randomly rolling the dice.
; 3. Based on the user's choice:
;    a. If the user chooses to randomly roll the dice, generate random dice rolls for the human and computer players and call the compare-dice-rolls function.
;    b. If the user chooses to manually roll the dice, prompt the user to enter a dice roll manually and validate the input. Then, call the compare-dice-rolls function.
;    c. If the user enters an invalid choice, display an error message and prompt the user again.
; Reference: none
; *********************************************************************
(defun start-game ()
  "Start a new game and randomly generate dice rolls for human and computer."
  (format t "~%Rolling the dice to determine who starts the round....~%")
  (finish-output)
  (format t "Do you want to manually roll (M) or randomly roll (R) the dice? ")
  (finish-output)
  (let* ((user-input (read-line)))
    (cond
      ((string-equal user-input "R")
       (let ((user-roll (get-random-dice-roll))
             (computer-roll (get-random-dice-roll)))
         (compare-dice-rolls user-roll computer-roll)))
      ((string-equal user-input "M")
       (let ((user-roll (get-manual-dice-roll "Human"))
             (computer-roll (get-manual-dice-roll "Computer")))
         (compare-dice-rolls user-roll computer-roll)))
      (t
       (format t "~%Invalid choice. Please enter 'M' for manual or 'R' for random.~%")
       (start-game)))))


; *********************************************************************
; Function Name: get-random-dice-roll
; Purpose: Generate a random dice roll between 1 and 6.
; Parameters: player - The name (string) of the player for whom the dice roll is being generated.
; Return Value: The random dice roll.
; Algorithm:
; 1. Generate a random number between 1 and 6.
; 2. Return the random number as the dice roll.
; Reference: none
; *********************************************************************
(defun get-manual-dice-roll (player)
  "Prompts the user to enter a dice roll manually and validates the input."
  (format t "~%Enter a number between 1 and 6 for ~a " player)
  (finish-output)
  (let ((user-input (parse-integer (read-line))))
    (cond ((or (not user-input) (> user-input 6) (< user-input 1))
           (format t "~%Invalid Entry. Enter a number from 1-6.~%")
           (get-manual-dice-roll player))
          (t user-input))))

; *********************************************************************
; Function Name:compare-dice-rolls
; Purpose: Compare the dice rolls of the human and computer players and determine the winner.
; Parameters: user-roll - The dice roll of the human player.
;             computer-roll - The dice roll of the computer player.
; Return Value: None
; Algorithm:
; 1. Display the dice rolls of the human and computer players.
; 2. Compare the dice rolls and determine the winner.
; 3. If the human player wins, display a message indicating that the human player won and call the playTournament function with the human player starting the round.
; 4. If the computer player wins, display a message indicating that the computer player won and call the playTournament function with the computer player starting the round.
; 5. If it is a draw, display a message indicating that it is a draw and call the start-game function to roll the dice again.
; Reference: none
; *********************************************************************

(defun compare-dice-rolls (user-roll computer-roll)
  "Compare the dice rolls and determine the winner."
  (format t "~%Your roll: ~a~%"  user-roll)
  (format t "Computer's roll: ~a~%" computer-roll)
    (cond
      ((> user-roll computer-roll)
       (format t "~%You won!~%")
        (sleep 1)
       ;(display-scorecard scorecard)
       (playTournament 1)) ;; Human starts the round
      ((< user-roll computer-roll)
       (format t "~%Computer won!~%")
        (sleep 1)
       ;(display-scorecard scorecard)
       (playTournament 2) ;; computer starts the round
       )
      (t
       (format t "~%It's a draw! Rolling again...~%")
      (sleep 1)
       (start-game)))
)

; *********************************************************************
; Function Name: load-game
; Purpose: Load a previously saved game.
; Parameters: None
; Return Value: A list containing the round number and scorecard.
; Algorithm:
; 1. Call the load-scorecard function to get the round number and scorecard.
; 2. If the load-scorecard function returns a non-nil value:
;    a. Get the round number and scorecard from the result.
;    b. Display the scorecard.
;    c. Display the round number.
;    d. Call the playConsecutiveRound function with the scorecard and round number.
;    e. Return the round number and scorecard as a list.
; Reference: none
; *********************************************************************
(defun load-game ()
  "Load a previously saved game. (Placeholder function)"
  (let ((result (load-scorecard))) ; Call load-scorecard to get the round number and scorecard
    (cond
     (result
      (let* ((round-no (first result))
            (scorecard (second result))
            (roundnum (- round-no 1)))
        (display-scorecard scorecard) ; Display the scorecard
        (terpri)
        (displayLoadedGameScores scorecard) ;; Display the scoreboard
        ;(format t "Round number: ~A~%" round-no) ; Display the round number
        (playConsecutiveRound scorecard roundnum)
        (list round-no scorecard))) ; Return the round number and scorecard as a list
)))

        

; *********************************************************************
; Function Name: exit-game
; Purpose: Exit the game gracefully.
; Parameters: None
; Return Value: None
; Algorithm:
; 1. Display a message indicating that the game is exiting.
; 2. Call the quit function to exit the game.
; Reference: none
; *********************************************************************
(defun exit-game ()
  "Exit the game gracefully."
  (format t "~%Exiting the game. Goodbye!~%")
  (quit))  ;; exit function


; *********************************************************************
; START OF THE PROGRAM
; *********************************************************************
(start-yahtzee) ;; Start the game


