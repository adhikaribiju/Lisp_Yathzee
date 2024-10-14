(load "Tournament.lsp")
(load "Round.lsp")
(load "Dice.lsp")
(load "Scorecard.lsp")
(load "Human.lsp")
(load "Computer.lsp")




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




(defun start-game ()
  "Start a new game and randomly generate dice rolls for human and computer."
    (format t "~%Rolling the dice...~%")
    (let ((user-roll (get-random-dice-roll))
            (computer-roll (get-random-dice-roll)))
        (compare-dice-rolls user-roll computer-roll))
)


(defun get-random-dice-roll ()
  "Generate a random dice roll between 1 and 6."
  (+ 1 (random 6)))

(defun compare-dice-rolls (user-roll computer-roll)
  "Compare the dice rolls and determine the winner."
  (format t "~%Your roll: ~a~%"  user-roll)
  (format t "Computer's roll: ~a~%" computer-roll)
  (cond
    ((> user-roll computer-roll)
     (format t "~%You won!~%"))
    ((< user-roll computer-roll)
     (format t "~%Computer computer won!~%"))
    (t
     (format t "~%It's a draw! Rolling again...~%")
     (start-game)))
)

(defun load-game ()
  "Load a previously saved game. (Placeholder function)"
  (format t "~%Load game feature is not implemented yet.~%")
  (start-yahtzee))

(defun exit-game ()
  "Exit the game gracefully."
  (format t "~%Exiting the game. Goodbye!~%")
  (quit))  ;; Use the appropriate exit function for your Lisp environment.

;; Start the game
(start-yahtzee)
