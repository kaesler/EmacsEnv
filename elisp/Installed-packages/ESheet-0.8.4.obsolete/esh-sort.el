
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Sort etc.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sort-region () "prompt the user for keys and then sort the region"
  (interactive)
  (if (not esheet-region) (message "no region to sort!")
    (progn
      (setq old-window (selected-window))
      (select-window (get-buffer-window "*edit line*"))
      (erase-buffer)
      (insert "*SORT*   keys (start numbering at 1): '()\n")
      (insert "sort by: columns                        OK Cancel")
      (setq ks (move-marker (make-marker) 39))
      (setq ke (move-marker (make-marker) 43))
      (setq sb (move-marker (make-marker) 52))
      (add-text-properties ks ke '(face secondary-selection))
      (add-text-properties sb (+ sb 7) '(face primary-selection))
      (add-text-properties (+ sb 31) (+ sb 33) '(face primary-selection))
      (add-text-properties (+ sb 34) (+ sb 40) '(face primary-selection))
      (goto-char (- ke 1))
      (local-set-key [left] 'sort-backward-char)
      (local-set-key [right] 'sort-forward-char)
      (local-set-key [return] 'sort-return-key)
      (setq current-sort-widget 0)
      (setq sort-by-rows nil)
      )
    )
  )

(defun sort-backward-char () "go backward one character if acceptable under the (sort-region) inteface"
  (interactive)
  (if (and (equal current-sort-widget 0) (> (point) (+ ks 2))) (backward-char)
    (progn
      (unshow-current-sort-widget)
      (setq current-sort-widget (mod (- current-sort-widget 1) 4))
      (show-current-sort-widget)
      )
    )
  )

(defun sort-forward-char () "go backward one character if acceptable under the (sort-region) inteface"
  (interactive)
  (if (and (equal current-sort-widget 0) (< (point) (- ke 2))) (forward-char)
    (progn
      (unshow-current-sort-widget)
      (setq current-sort-widget (mod (+ current-sort-widget 1) 4))
      (show-current-sort-widget)
      )
    )
  )

(defun unshow-current-sort-widget () "set the face of the current sort widget to non-current"
  (cond
   ((= current-sort-widget 0) (add-text-properties ks ke '(face primary-selection)))
   ((= current-sort-widget 1) (add-text-properties sb (+ sb 7) '(face primary-selection)))
   ((= current-sort-widget 2) (add-text-properties (+ sb 31) (+ sb 33) '(face primary-selection))) 
   ((= current-sort-widget 3) (add-text-properties (+ sb 34) (+ sb 40) '(face primary-selection))) 
   )
  )

(defun show-current-sort-widget () "set the face of the current sort widget to non-current"
  (cond
   ((= current-sort-widget 0) (add-text-properties ks ke '(face secondary-selection)))
   ((= current-sort-widget 1) (add-text-properties sb (+ sb 7) '(face highlight)))
   ((= current-sort-widget 2) (add-text-properties (+ sb 31) (+ sb 33) '(face highlight)))
   ((= current-sort-widget 3) (add-text-properties (+ sb 34) (+ sb 40) '(face highlight)))
   )
  )

(defun sort-done ()
  (local-set-key [left] 'backward-char)
  (local-set-key [right] 'forward-char)
  (local-set-key [return] 'set-cell-value)
  (select-window old-window))

(defun cons-tags (l &optional n)
  (if (not n) (setq n 0))
  (if l (cons (cons n (car l)) (cons-tags (cdr l) (+ n 1))) nil))

(defun sort-return-key () (interactive)
  (cond
   ((= current-sort-widget 1) (goto-char sb) (delete-char 7) (setq sort-by-rows (not sort-by-rows))
    (insert (if sort-by-rows "rows   " "columns")) (show-current-sort-widget))
   ((= current-sort-widget 3) (sort-done) (curs cursX cursY))
   ((= current-sort-widget 2) 
    (let ((k (eval (car (read-from-string (buffer-substring ks ke))))))
      (sort-done)
      (let ((x1 (min (caar esheet-region) (cadr esheet-region)))
	    (y1 (min (cdar esheet-region) (cddr esheet-region)))
	    (x2 (max (caar esheet-region) (cadr esheet-region)))
	    (y2 (max (cdar esheet-region) (cddr esheet-region))))
	(let ((r (cons-tags (if sort-by-rows (rrange x1 y1 x2 y2 nil nil) (transpose (rrange x1 y1 x2 y2 nil nil))))))
	  (let ((nr (carall (sortmtx r k))))
	    (let ((old-clipboard esheet-clipboard))
	      (esheet-copy)
	      (let ((i 0))
		(while (< i (length nr))
		  (if sort-by-rows
		      (let ((y 0))
			(while (< y (length (aref esheet-clipboard 0)))
			  (add-text-properties (find-curs-n (+ x1 i) (+ y1 y)) (+ (find-curs-n (+ x1 i) (+ y1 y)) 5)
					       (list 'value
					       (aref (aref esheet-clipboard (nth i nr)) y)))
			  (refresh-cell (+ x1 i) (+ y1 y))
			  (setq y (+ y 1))))
		    (let ((x 0))
		      (while (< x (length esheet-clipboard))
			  (add-text-properties (find-curs-n (+ x x1) (+ i y1)) (+ (find-curs-n (+ x1 x) (+ y1 i)) 5)
					       (list 'value
					       (aref (aref esheet-clipboard x) (nth i nr))))
			  (refresh-cell (+ x1 x) (+ y1 i))
			  (setq x (+ x 1)))))
		  (setq i (+ i 1))))
	      (setq esheet-clipboard old-clipboard)))))))))
