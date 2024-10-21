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
                    ((full-house-p dice) ; condition
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


  