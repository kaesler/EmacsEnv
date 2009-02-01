(defun format-printable (d)
  "convert d into a six character string fit for human reading"
  (concat " "
	  (substring (format "%5s" 
			     (cond
			      ((stringp d) d)
			      ((numberp d) (num-to-string d))
			      ((functionp d) "#FUN#")
			      ((listp d) "#LST#")
			      ((consp d) "#CON#")
			      ((eq d 'identidy-element) "!NOD!")
			      ((eq d 'true) "TRUE ")
			      ((eq d 'false) "FALSE")
			      (t "!ERR!")))
			     0 5)))

(defun unlink (x y I-c)
  "Tell all cells in I-c that x,y does not call them."
  (dolist (c I-c) (kill-misc-prop (car c) (cdr c) 'calls-me (cons x y)))
  (dolist (c I-c) (kill-misc-prop x y 'I-call c)))


(defun refresh-cell (x y)
  "refresh the content of cell x,y to the value gifen in properties, if the cell contains a formula, re-evaluate it"
  (save-excursion
    (let ((val (get-text-property (+ 2 (find-curs-n x y)) 'value))
	  (I-c (tell-misc-prop x y 'I-call))
	  (c-m (tell-misc-prop x y 'calls-me)))
      (goto-char (find-curs-n x y))
      (delete-char 6)
      (insert " !E!R!");
      (add-text-properties (- (point) 6) (point) (list 'value val))
      (dolist (c c-m) (add-misc-prop x y 'calls-me c))
      (unlink x y I-c)
      (let ((ins (format-printable 
		  (if (and val (> (length val) 0) (equal (substring val 0 1) "="))
		      (eval (car (read-from-string (substring val 1))))
		    (if (and val (> (length val) 0) (equal (substring val 0 1) "+"))
			(infix-parse (substring val 1) x y)
		      (if val val ""))))))
	(let ((new-I-c (tell-misc-prop x y 'I-call)))
	  (delete-char -6)
	  (insert ins)
	  (dolist (c new-I-c) (add-misc-prop x y 'I-call c))))
      (add-text-properties (- (point) 6) (point) (list 'value val))
      (dolist (c c-m) (refresh-cell (car c) (cdr c))))))
