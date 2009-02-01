(defun n-spaces (n) "return n spaces"
  (if (> n 0) (concat " " (n-spaces (- n 1))) ""))

(defun make-five-chars-long (s) "cut or lengthen string s to five chars"
  (if (> (length s) 5)
      (substring s 0 5)
    (concat s (n-spaces (- 5 (length s))))))

(defun no-white-space (str) "return t iff str contains no whitespace"
  (if (= (length str) 0) nil
    (catch 'found-some
      (let ((i 0) (m (length str)))
	(while (< i m)
	  (if (or (equal ?  (aref str i)) (equal ?\t (aref str i)) (equal ?\n (aref str i)))
	      (throw 'found-some nil))
	  (setq i (+ i 1)))) 
      t)))

(defun to-float-if-nessesary (x) (if (numberp x) (float x) x))

(apply (eval (car (read-from-string "#'(lambda (c) (* c c))"))) '(3))

(defun rcell (x y cx cy &optional incl)
  "get the formula value of cell (x,y), called from cx cy"
  (to-float-if-nessesary 
   (progn
     (setq flag-autoref3 (list x y cx cy))
     (if cx 
	 (progn (add-misc-prop x y 'calls-me (cons cx cy))
		(add-misc-prop cx cy 'I-call (cons x y))))
     (let ((formula (get-text-property (find-curs-n x y) 'value)))
       (if (and (> (length formula) 0) (equal (substring formula 0 1) "="))
	   (eval (car (read-from-string
		       (concat "(let ((x " (int-to-string x) ") (y " (int-to-string y) ")) " 
			       (substring formula 1 (length formula)) ")"))))
	 (if (and (> (length formula) 0) (equal (substring formula 0 1) "+"))
	     (infix-parse (substring formula 1) x y)
	   (if (no-white-space formula)
	       (let ((o (eval (car (read-from-string (concat "'" formula)))) ))
		 (if (numberp o) o (if incl formula 'identidy-element)))
	     (if incl formula 'identidy-element))))))))

(defmacro cell (px py) "front end for rcell that takes cx and cy from local variables"
  (list 'rcell px py x y)) ;x and y are local variables

(defun collumn (x y1 y2 cx cy incl)
  (let ((i y2) (out nil))
    (while (>= i y1)
      (setq out (cons (rcell x i cx cy incl) out))
      (setq i (- i 1))
      )
    out
    )
  )


(defun rrange (x1 y1 x2 y2 cx cy &optional incl)
  (let ((i x2) (out nil))
    (while (>= i x1)
      (setq out (cons (collumn i y1 y2 cx cy incl) out))
      (setq i (- i 1))
      )
    out
    )
  )

(defmacro range (x1 y1 x2 y2) "front end for rrange handling auot-ref'ing"
  (list 'rrange x1 y1 x2 y2 x y)) ;x and y are local variables
