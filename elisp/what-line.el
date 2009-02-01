;;; what-line.el - more useful `M-=' and `C-x =' commands
;;; $Id: what-line.el,v 1.3 1991/04/04 09:37:20 sk Exp $

;;; M-= counts not only lines, but also words and characters.
;;; C-x = displays the line number and the character code in dec, oct, hex.

;;; Put the following into your ~/.emacs:
;;;   (autoload 'count-region "what-line" "Info on region." t)
;;;   (autoload 'what-cursor-position-and-line "what-line" "Info on cursor." t)
;;;   (define-key esc-map "=" 'count-region)
;;;   (define-key ctl-x-map "=" 'what-cursor-position-and-line)

(defun count-region (start end)
  "Count lines, words and characters in region."
  (interactive "r")
  (let ((l (count-lines start end))
	(w (count-words start end))
	(c (- end start)))
    (message "Region has %d line%s, %d word%s and %d character%s."
	     l (if (= 1 l) "" "s")
	     w (if (= 1 w) "" "s")
	     c (if (= 1 c) "" "s"))))

(defun count-words (start end)
  "Return number of words between START and END."
  (let ((count 0))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(while (forward-word 1)
	  (setq count (1+ count)))))
    count))

(defun what-cursor-position-and-line ()
  ;; So you don't need what-line any longer.
  "Print info on cursor position (on screen and within buffer):
The character code in octal, decimal and hex, the position of point, the
line and the column in relation to their respective totals."
  (interactive)
  (let* ((char (following-char))
	 (beg (point-min))
	 (end (point-max))
         (pos (point))
	 (total (buffer-size))
	 (percent (if (> total 50000)
		      ;; Avoid overflow from multiplying by 100!
		      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
		    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
	 (hscroll (if (= (window-hscroll) 0)
		      ""
		    (format " Hscroll=%d" (window-hscroll))))
	 (col (current-column))
	 (total-col (save-excursion
		      (end-of-line)
		      (current-column)))
	 ;; if final newline is missing, count that line anyway
	 (total-lines (count-lines 1 (1+ total)))
	 (line (save-restriction
		 (widen)
		 (save-excursion
		   (beginning-of-line)
		   (1+ (count-lines 1 (point)))))))
    
    (if (= pos end)
	;; if at end of (narrowed) buffer, cannot give info on Char,
	;; only on Point, Line and Column:
	(if (or (/= beg 1) (/= end (1+ total)))
	    ;; if narrowed, indicate narrowed region:
	    (message
	     "Point: %d/%d (%d%%) <%d - %d>  Line: %d/%d  Column %d/%d %s"
	     pos total percent beg end line total-lines col total-col hscroll)
	  (message
	   "Point: %d/%d (%d%%)  Line: %d/%d Column: %d/%d %s"
	   pos total percent line total-lines col total-col hscroll))
      ;; not at end, cursor is on some Char
      (if (or (/= beg 1) (/= end (1+ total)))
	  ;; buffer is narrowed, indicate narrowed region
	  (message
	   "Char: %s (0%o=%d=0x%x)  Point: %d/%d (%d%%) <%d - %d>  Line: %d/%d Column: %d/%d %s"
	   (single-key-description char) char char char pos total
	   percent beg end line total-lines col total-col hscroll)
	;; no narrowing in effect
	(message
	 "Char: %s (0%o=%d=0x%x)  Point: %d/%d (%d%%)  Line: %d/%d  Column: %d/%d %s"
	 (single-key-description char) char char char pos total percent
	 line total-lines col total-col hscroll)))))
