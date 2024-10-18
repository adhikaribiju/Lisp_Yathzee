(load "Tournament.lsp")
(load "Round.lsp")
(load "Dice.lsp")
(load "Scorecard.lsp")
(load "Human.lsp")
(load "Computer.lsp")

;; Use ; to comment your code. The comment runs from the ; to the end of the line
;; Properly indent your code to make it readable. E.g.:
;; (defun function ( first second ) 
;;    (cond ( ( first-condition 
;;              first-action )
;;            ( second-condition
;;              second-action )
;;            ( t
;;              default-action ) )) )
;; Use Top-down Design. It will save you countless hours and great amount of aggravation later.
;; Pass values between functions as parameters. This is the only mechanism available to you for this purpose.
;; Use recursion instead of iteration (loops). Use cond statement where selection is involved.
;; Do not use:
;; - setq/setf...
;; - prog.
;; - if
;; - loop/do
;; - rplaca/rplacd/...
;; - arrays...
;; - push, pop
;; - property lists
;; - any "destructive" construct - that which changes the value of the argument passed to it
;; - and in general, any other C-like LISP construct, especially those we have not discussed in class. To avoid unnecessary penalization, you may want to run by me all constructs you plan to use that have not been discussed in class. Bottom Line: Write functional code, not imperative code.


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

(defun compare-dice-rolls (user-roll computer-roll)
  "Compare the dice rolls and determine the winner."
  (format t "~%Your roll: ~a~%"  user-roll)
  (format t "Computer's roll: ~a~%" computer-roll)
    (cond
      ((> user-roll computer-roll)
       (format t "~%You won!~%")
       ;(display-scorecard scorecard)
       (playTournament 1)) ;; Human starts the round
      ((< user-roll computer-roll)
       (format t "~%Computer won!~%")
       ;(display-scorecard scorecard)
       (playTournament 2) ;; computer starts the round
       )
      (t
       (format t "~%It's a draw! Rolling again...~%")
       (start-game)))
)


(defun load-game ()
  "Load a previously saved game. (Placeholder function)"
  (let ((result (load-scorecard))) ; Call load-scorecard to get the round number and scorecard
    (cond
     (result
      (let* ((round-no (first result))
            (scorecard (second result))
            (roundnum (- round-no 1)))
        (display-scorecard scorecard) ; Display the scorecard
        (displayLoadedGameScores scorecard) ;; Display the scoreboard
        ;(format t "Round number: ~A~%" round-no) ; Display the round number
        (playConsecutiveRound scorecard roundnum)
        (list round-no scorecard))) ; Return the round number and scorecard as a list
)))

        


(defun exit-game ()
  "Exit the game gracefully."
  (format t "~%Exiting the game. Goodbye!~%")
  (quit))  ;; exit function

;; Start the game
(start-yahtzee)


