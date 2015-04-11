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

(defun shift-elements (vec low high)
  (prog1 vec
    (loop :for i :from high :downto low :do
       (setf (aref vec (1+ i)) (aref vec i)))))

(defun initialize-perms (vec element &optional (low 0))
  (prog1 vec
    (loop :with j := low
       :for i :from low :below (length vec)
       :if (eql (aref vec i) element) :do
       (shift-elements vec j (1- i))
       (setf (aref vec j) element j (1+ j)))))

(defun can-move-index (vec element)
  (loop :for i :from (1- (length vec)) :downto 0
     :for current := (aref vec i)
     :with prev := nil
     :when (and prev
                (not (eql prev element))
                (eql current element))
     :do (return i)
     :end :do (setf prev current)))

(defun group-size (vec)
  (loop :with element := (aref vec 0)
     :for e :across vec
     :while (eql e element) :count 1))

(defun move-index (vec index)
  (prog1 vec
    (psetf (aref vec (1+ index)) (aref vec index)
           (aref vec index) (aref vec (1+ index)))))

(defun permute-group (vec element &optional (low 0))
  (cons
   (copy-seq (initialize-perms vec element low))
   (loop :with init := (initialize-perms vec element low)
      :with last := low
      :for moving := (can-move-index init element)
      :while moving
      :do (move-index init moving)
      :when (< moving last) :do
      (initialize-perms init element (1+ moving))
      :end
      :collect (copy-seq init)
      :do (setf last moving))))

(defun canonical (element repeat &optional (previous #()))
  (loop :with result := (make-array (+ repeat (length previous)))
     :for i :below repeat :do
     (setf (aref result i) element)
     :finally 
     (return
       (prog1 result
         (loop :for j :from i :below (length result) :do
            (setf (aref result j) (aref previous (- j i))))))))

(defun permutations-with-repetition (groups)
  (loop :with first := (car groups)
     :with perms := (list (canonical (car first) (cdr first)))
     :for (key . value) :in (cdr groups)
     :do (setf perms
               (loop :for perm :in perms
                  :nconc (permute-group (canonical key value perm) key)))
     :finally (return perms)))
  
(defun print-persm (groups)
  (format t "~&~{~{~d~}~^~%~}"
          (mapcar (lambda (x) (coerce x 'list))
                  (permutations-with-repetition groups))))

(defparameter *all-flags*
  (permutations-with-repetition '((a . 5) (b . 4) (c . 2))))

(defun first-three-a ()
  (/ (loop :for flags :in *all-flags*
        :when (equal (coerce (subseq flags 0 3) 'list) '(a a a))
        :count 1)
     (length *all-flags*)))

(defun togetherp (flags)
  (= 2 (loop :with previous := nil
          :for elt :across flags
          :when (and previous (not (eql elt previous)))
          :count 1 :end
          :do (setf previous elt))))

(defun flags-hang-together ()
  (/ (loop :for flags :in *all-flags*
        :when (togetherp flags)
        :count 1)
     (length *all-flags*)))

(defun between-army-p (flags)
  (not
   (loop :with flags-seen := 0
      :with previous := nil
      :for elt :across flags :do
      (case elt
        (b (when (= flags-seen 1) (return t)))
        (c (when (eql previous 'c) (return t))
           (incf flags-seen)))
      (setf previous elt))))

(defun between-army ()
  (/ (loop :for flags :in *all-flags*
        :when (between-army-p flags)
        :count 1)
     (length *all-flags*)))

(defun first-three-duplicate ()
  (/ (loop :for flags :in *all-flags*
        :when (or (eql (aref flags 0) (aref flags 1))
                  (eql (aref flags 1) (aref flags 2))
                  (eql (aref flags 0) (aref flags 2)))
        :count 1)
     (length *all-flags*)))
