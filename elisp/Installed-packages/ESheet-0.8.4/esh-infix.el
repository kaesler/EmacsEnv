(defun seek-and-do (s o f x y l &optional c)
  (let ((i 0) (paren 0))
    (catch 'found-one
      (while (< i (- (length s) (length o) -1))
        (cond
         ((equal (substring s i (+ i 1)) "(")
          (setq paren (+ paren 1)))
         ((equal (substring s i (+ i 1)) ")")
          (setq paren (- paren 1)))
         ((and (equal paren 0) (equal (substring s i (+ i (length o))) o))
          (throw 'found-one 
		 (funcall f 
			  (float-if-needed (infix-parse (substring s 0 i) x y l) c)
			  (float-if-needed (infix-parse (substring s (+ i (length o))) x y l) c)) )))
        (setq i (+ i 1)))
      nil)))

(defun seek-for-binary-minus (s x y l)
  (let ((i 0) (paren 0))
    (catch 'found-one
      (while (< i (length s))
        (cond
         ((equal (substring s i (+ i 1)) "(")
          (setq paren (+ paren 1)))
         ((equal (substring s i (+ i 1)) ")")
          (setq paren (- paren 1)))
         ((and (equal paren 0) (equal (substring s i (+ i 1)) "-") 
               (> i 0) (not (or 
                           (equal (substring s (- i 1) i) "+")
                           (equal (substring s (- i 1) i) "-")
                           (equal (substring s (- i 1) i) "*")
                           (equal (substring s (- i 1) i) "/")
                           (equal (substring s (- i 1) i) "^")
                           (equal (substring s (- i 1) i) ",")
                           (equal (substring s (- i 1) i) "(")
                           )))
          (throw 'found-one 
                (funcall #'- 
                        (infix-parse (substring s 0 i) x y l)
                        (infix-parse (substring s (+ i 1)) x y l)) )))
        (setq i (+ i 1)))
      nil)))

(defun seek-and-do-without-parse (s o f x y)
  (let ((i 0) (paren 0))
    (catch 'found-one
      (while (< i (- (length s) (length o) -1))
        (cond
         ((equal (substring s i (+ i 1)) "(")
          (setq paren (+ paren 1)))
         ((equal (substring s i (+ i 1)) ")")
          (setq paren (- paren 1)))
         ((and (equal paren 0) (equal (substring s i (+ i (length o))) o))
          (throw 'found-one 
                (funcall f 
                        (substring s 0 i)
                        (substring s (+ i (length o))) x y))))
        (setq i (+ i 1)))
      nil)))

(defun in-paren (s)
  (if (equal s "") nil
    (let ((i 0) (c 0))
      (catch 'found 
        (while (< i (length s))
          (cond
           ((equal (substring s i (+ i 1)) "(") (setq c (+ c 1)))
           ((equal (substring s i (+ i 1)) ")") (setq c (- c 1)))
           ((equal c 0) (throw 'found nil)))
          (setq i (+ i 1)))
        t))))

(defun list-if-nessesary (o)
  (if (consp o) o (cons o nil)))

(defun func-check (s x y)
  (let ((i 0))
    (catch 'found
      (while (< i (length s))
        (if (equal (substring s i (+ i 1)) "(")
            (cond
             ((equal (substring s 0 i) "cell") (throw 'found
						      (apply #'rcell (append (mapcar #'floor (infix-parse (substring s i)
													  x y 1)) (list x y)))))
             ((equal (substring s 0 i) "range") (throw 'found
						       (apply #'rrange (append (mapcar #'floor (infix-parse (substring s i)
													    x y 1)) (list x y)))))
             (t
              (throw 'found (apply (symbol-function  (car (read-from-string (substring s 0 i))))
				   (list-if-nessesary (infix-parse (substring s i) x y 1)))))))
        (setq i (+ i 1)))
      nil)))

(defun variable-check (s x y)
  (cond
   ((equal s "x") x)
   ((equal s "y") y)
   ((equal s "") 0)))

(defun fromto (a b)
  "returns a list from a to b.  (e.g. (fromto 3 6) => (3 4 5 6) and (fromto 4 2) => (4 3 2))"
  (interactive)
  (setq a (round a)) (setq b (round b))
  (if (equal a b)
      (list a)
    (let ((i b) (o nil) (d (if (> a b) 1 -1)))
      (while (not (equal i a))
        (setq o (cons i o))
        (setq i (+ i d)))
      (cons a o))))

(defun mini-eval (x) 
  "An immensely primitive version of eval, only supporting single atoms"
  (if (symbolp x) (symbol-value x) x))

(defun float-if-needed (n c) (if (and c (integerp n)) (float n) n))
;(defun int-if-wise (n) (if (and (numberp n) (% 5.0 4.0)

(defun infix-parse (s x y &optional l) ;l is for optimization, making it unnessesary to re-check for operaters
  "Parse expressions with infix operators.  Developed for esheet; for full info, see the esheet documentation.  s is the string to parse, x and y the values of x and y, and l the operator to start with"
  (if (not l) (setq l 1))
  (if (in-paren s) (progn (setq s (substring s 1 (- (length s) 1))) (setq l 1)))
  (or
   (variable-check s x y)
   (seek-and-do-without-parse s "?" #'(lambda (a b x y)
					(if (eq (infix-parse a x y) 'false)
					    (infix-parse (cdr (infix-parse b x y)) x y)
					  (infix-parse (car (infix-parse b x y)) x y))) x y)
   (seek-and-do-without-parse s ":" #'(lambda (a b c d) (cons a b)) x y)
   (and (< l 2) (seek-and-do s "," #'(lambda (a b) (if (consp b) (cons a b) (list a b))) x y 1))
   (and (< l 3) (seek-and-do s ">=" #'(lambda (a b) (if (>= a b) 'true 'false)) x y 2 t))
   (and (< l 4) (seek-and-do s "<=" #'(lambda (a b) (if (<= a b) 'true 'false)) x y 3 t))
   (and (< l 5) (seek-and-do s "<" #'(lambda (a b) (if (< a b) 'true 'false)) x y 4 t))
   (and (< l 6) (seek-and-do s ">" #'(lambda (a b) (if (> a b) 'true 'false)) x y 5 t))
   (and (< l 7) (seek-and-do s "==" #'(lambda (a b) (if (equal a b) 'true 'false)) x y 6 t))
   (and (< l 8) (seek-and-do s ".." #'fromto x y 7))
   (and (< l 9) (seek-and-do s "+" #'+ x y 8))
   (and (< l 10) (seek-for-binary-minus s x y 9))
   (and (< l 11) (seek-and-do s "*" #'* x y 10))
   (and (< l 12) (seek-and-do s "/" #'/ x y 11 t))
   (and (< l 13) (seek-and-do s "%" #'% x y 12))
   (and (< l 14) (seek-and-do s "-" #'- x y 13))
   (and (< l 15) (seek-and-do s "^" #'^ x y 14))
   (func-check s x y)
   (mini-eval (car (read-from-string s)))))

;(expt 4 .5)

;(infix-parse "sqrt(3*5-2*-8+1*(2+3)),8" 0 0)
;(infix-parse "sqrt((7--2*(3+7))^(2/3)),6" 0 0)

;(infix-parse "1/2+.5" 0 0)
;(infix-parse "5^4" 0 0)
