
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Number to String Converters;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Floats and integers need to appear intelligently in their cells
;These routines handle switching in and out of scientific notation
;
;these should also be their own file

(defun mantissa (num)
  (number-to-string (* num (^ 10.0 (- (floor (/ (log num) (log 10))))))))

(defun expo (num)
  (number-to-string (floor (/ (log num) (log 10)))))

(defun sci-note (num)
  (let ((exp (expo (* (if (< num 0) -1 1) num))) (mant (mantissa (* (if (< num 0) -1 1) num))))
    (concat
     (if (< num 0) "-" "")
     (if (and (> num 0) (equal (length exp) 1)) (substring mant 0 3) (substring mant 0 1))
     "e" exp)))

(defun num-to-string (n) "convert number n to a string, using scientific notation if nessesary, always within 5 charactrers or simply truncatable to that"
  (cond
   ((functionp n) "!FUN!")
   ((not (numberp n)) "!EER!")
   ((zerop n) "0")
   ((or (and (< n 100000) (> n .0001)) (and (> n -1000) (< n -.001))) (number-to-string n))
   (t (sci-note n))))
