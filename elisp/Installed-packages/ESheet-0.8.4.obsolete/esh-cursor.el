(defun line-length () "determine the length of a line"
  (save-excursion (goto-char 1) (end-of-line) (point)))


(defun add-col () 
  "add a column to right edge of the sheet"
  (save-excursion
    (goto-char 1)
      (while (< (point) (point-max))
	(goto-char (+ (point) 1))
	(end-of-line)
	(insert "      "))))

(defun add-row () 
  "add a row to the bottom of the sheet"
  (save-excursion
    (goto-char (point-max))
    (insert "\n")
    (let ((m (line-length)) (i 1))
      (while (< i m)
	(insert " ")
	(setq i (+ i 1))))))


(defun find-curs-n (x y) 
  "find the point value for the begginning of a cell"
  (if (and (> x -1) (> y -1))
      (let ((ans
	     (let ((len (line-length)))
	       (if (< len (* (+ x 1) 6))
		   nil
		 (+ (* y len) (* 6 x) 1)))))
	(if (and ans (< ans (point-max))) ans nil))
    nil))

(defun curs (x y) "Move the esheet cursor to x,y" (interactive)
  (if (find-curs-n x y)
      (progn
	(let ((n (find-curs-n cursX cursY)))
	  (rest-prop n (+ n 6)))
	(setq cursX x)
	(setq cursY y)
	(let ((n (find-curs-n cursX cursY)))
	  (cov-prop n (+ n 6))
	  (add-text-properties n (+ n 6) '(face curson))
	  (goto-char (+ n 6))
	  (show-cell-value n))
	t)
    (progn (message "can't go there") nil)))
  
;;cursor commands
(defun mess-where-you () "display basic info in the minibuffer"
  (message (concat "cell: " (int-to-string cursX) "," (int-to-string cursY))))

;These two lines are very useful if you try to debug the auto-recalculation
;		   "   I-call:" (prin1-to-string (tell-misc-prop cursX cursY 'I-call))
;		   "   calls-me:" (prin1-to-string (tell-misc-prop cursX cursY 'calls-me)))))

(defun go-down () "move the esheet cursor down" (interactive)
  (unhighlite-region)
  (if (not (find-curs-n cursX (+ cursY 1))) (add-row))
  (curs cursX (+ cursY 1))
  (mess-where-you)
  (setq esheet-region nil))
(defun go-up () "move the esheet cursor up" (interactive)
  (unhighlite-region)
  (curs cursX (- cursY 1))
  (mess-where-you)
  (setq esheet-region nil))
(defun go-left () "move the esheet cursor left" (interactive)
  (unhighlite-region)
  (curs (- cursX 1) cursY)
  (mess-where-you)
  (setq esheet-region nil))
(defun go-right () "move the esheet cursor right" (interactive)
  (unhighlite-region)
  (if (not (find-curs-n (+ cursX 1) cursY))
      (add-col))
  (curs (+ cursX 1) cursY)
  (mess-where-you)
  (setq esheet-region nil))




