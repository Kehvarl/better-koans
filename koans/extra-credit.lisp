;;; Copyright 2013 Google Inc.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; EXTRA CREDIT:
;;;
;;; Create a program that will play the Greed game.
;;; The full rules for the game are in the file extra-credit.txt.
;;;
;;; You already have a DICE-SET class and a score function you can use.
;;; Write a PLAYER class and a GAME class to complete the project.
;;;
;;; This is a free form assignment, so approach it however you desire.

(defclass dice-set ()
  ;; Fill in the blank with a proper slot definition.
  ((dice-value :initform '() :accessor dice-values)))

(defmethod roll (count (object dice-set))
  (check-type count (integer 1 *))
  (setf (slot-value object 'dice-value) '())
  (loop for die from 1 to count
    do (setf (slot-value object 'dice-value) (cons (+ 1 (random 5)) (slot-value object 'dice-value))))
  (slot-value object 'dice-value)
  )

(defun dice-to-hash (dice)
  (let ((vals (make-hash-table)))
    (dolist (d dice vals)
            (unless (gethash d vals)
              (setf (gethash d vals) 0))
            (incf (gethash d vals))
            )))

(defun score (&rest dice)
  (let ((dice-hash (dice-to-hash (car dice)))
        (score 0))
    (loop for value being the hash-values of dice-hash
      using (hash-key key)
      do (cond
           ((= 1 key)
            (if (> value 2) (incf score 1000))
            (if (> value 3) (incf score (* (- value 3) 100)))
            (if (< value 3) (incf score (* value 100))))

           ((= 5 key)
            (if (> value 2) (incf score 500))
            (if (> value 3) (incf score (* (- value 3) 50)))
            (if (< value 3) (incf score (* value 50))))

           (t
            (if (> value 2) (incf score (* key 100))))
           ))
    score))

(defun greed-player-turn (player)
  (let ((dice (make-instance 'dice-set)))
    (let ((round (score (roll 5 dice))))
      (format t "~&Player ~a.  Rolled ~a.   Score ~a" player (dice-values dice) round)
      round)))


(defun greed-play-game (players)
  (let ((player-scores (make-hash-table))
        (player-turn 0)
        (game-over nil))
    (loop for player from 0 to players do (setf (gethash player player-scores) 0))
    (loop while (null game-over)
      do
      (let ((score (greed-player-turn player-turn)))
        (if (or (>= (gethash player-turn player-scores) 300)
                (>= score 300))
          (incf (gethash player-turn player-scores) score))
        (format t " Score: ~a" (gethash player-turn player-scores))
      (if (>= (gethash player-turn player-scores) 3000)
        (setf game-over t))
      (setf player-turn (mod (1+ player-turn) players))
      ))))


(define-test play-greed
  (assert-true t))
