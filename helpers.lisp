;; Utilities

(defun generate-ticket ()
  (loop :repeat 6 :collect (random 6)))

(defun exactly-3-match (a b)
  (= 3 (loop :for i :in a :for j :in b
          :when (= i j) :count 1)))

(defun palindromep (tested) (equal tested (reverse tested)))

(defun num->ticket (n)
  (nreverse
   (loop :repeat 6 :collect (mod n 6) :do (setf n (floor n 6)))))
     
(defun ticket->num (ticket)
  (reduce (lambda (a b) (+ (* 6 a) b)) ticket :initial-value 0))

(defun next-ticket (previous)
  (num->ticket (1+ (ticket->num previous))))

(defparameter *all-tickets* (expt 6 6))

(defun factorial (n)
  (loop :for i :upto n
     :for result := 1 :then (* result i)
     :finally (return result)))

(defun binom (n k)
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

;; Tests

(defun chance-of-winning ()
  (/ (loop :with ticket := (generate-ticket)
        :repeat *all-tickets*
        :for attempt := '(0 0 0 0 0 0) :then (next-ticket attempt)
        :when (equal attempt ticket) :count 1)
     *all-tickets*))

(defun chance-of-three-matching ()
  (/ (loop :with ticket := (generate-ticket)
        :repeat *all-tickets*
        :for attempt := '(0 0 0 0 0 0) :then (next-ticket attempt)
        :when (exactly-3-match ticket attempt) :count 1)
     *all-tickets*))

(defun chance-of-palindrome ()
  (/ (loop :repeat *all-tickets*
        :for attempt := '(0 0 0 0 0 0) :then (next-ticket attempt)
        :when (palindromep attempt) :count 1)
     *all-tickets*))

(defun question-1-test ()
  (format t "~&Chance of winning the lotery:     ~f~%~
               Chance of guessing exactly three: ~f~%~
               Chance of palindrome ticket:      ~f"
          (chance-of-winning)
          (chance-of-three-matching)
          (chance-of-palindrome)))
