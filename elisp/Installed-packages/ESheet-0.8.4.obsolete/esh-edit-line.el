(defun show-cell-value (n) 
  "show the value of the cell it the edit line window"
  (save-excursion
    (save-selected-window
     (let ((val (get-text-property n 'value)))
       (select-window (get-buffer-window "*edit line*"))
       (erase-buffer)
       (if val (insert val))))))


(defun edit-cell-value () 
  "edit the content of a cell in the edit line"
  (interactive)
  (setq old-window (selected-window))
  (select-window (get-buffer-window "*edit line*")))

(defun replace-cell-value () 
  "replace the content of a cell in the edit line"
  (interactive)
  (setq old-window (selected-window))
  (select-window (get-buffer-window "*edit line*"))
  (erase-buffer)
  (self-insert-command 1)
)

(defun set-cell-value () 
  "finish editing a given cell" 
  (interactive)
  (let ((cont (buffer-substring (point-min) (point-max))))
    (select-window old-window)
    (curs cursX cursY)
    (add-text-properties (- (point) 6) (point) (list 'value cont)))
  (refresh-cell cursX cursY)
  (go-down))

(defun set-cell-value-and-go-right () "same as set-cell-value, only going right instead of down, bound to [tab]" (interactive)
  (set-cell-value)
  (go-up) ;yes this is ugly, but it's simple
  (go-right))

(defun cov-prop (s e) "save face in old" (interactive)
  (let ((n s))
    (while (< n e)
      (add-text-properties n (+ n 1) (list 'old (get-text-property n 'face)))
      (setq n (+ n 1)))))

(defun rest-prop (s e) "restore old data to face"
  (let ((n s))
    (while (< n e)
      (add-text-properties n (+ n 1) (list 'face (get-text-property n 'old)))
      (setq n (+ n 1)))))


(defun absolute-choose-range-or-cell () "insert (cell) or (range) command into the edit line corresponding to the cursor/region"
  (interactive)
  (let ((text
	 (if esheet-region
	     (concat (if infixFlag "range(" "(range ")
		     (int-to-string (min (caar esheet-region) (cadr esheet-region))) 
		     (if infixFlag "," " ")
		     (int-to-string (min (cdar esheet-region) (cddr esheet-region)))
		     (if infixFlag "," " ")
		     (int-to-string (max (caar esheet-region) (cadr esheet-region)))
		     (if infixFlag "," " ")
		     (int-to-string (max (cdar esheet-region) (cddr esheet-region))) ")")
	   (concat (if infixFlag "cell(" "(cell ")
		   (int-to-string cursX) (if infixFlag "," " ") (int-to-string cursY) ")")
	   )
	 ))
    (unhighlite-region)
    (curs oldCursX oldCursY)
    (local-set-key [delete] 'intel-del)
    (local-set-key [backspace] 'intel-del) ;observe C-h is unbound
    (local-set-key [return] 'edit-cell-value)
    (local-set-key [?\ ] 'edit-cell-value)
    (local-set-key [tab] 'edit-cell-value)
    (let ((c ?!)) (while (<= c ?~) (local-set-key (concat (cons c nil)) 'replace-cell-value) 
			 (setq c (+ c 1))))
    (setq esheet-region nil)
    (select-window (get-buffer-window (get-buffer "*edit line*")))
    (erase-buffer)
    (insert old-edit-line-text)
    (goto-char old-edit-line-position)
    (insert text)
    )
  )

(defun get-absolute-cell-or-region () "leave the edit line temporarily to get a cell or range command conviniently"
  (interactive)
  (setq oldCursX cursX)
  (setq oldCursY cursY)
  (setq infixFlag (and (> (length (buffer-string)) 0) (equal (buffer-substring 1 2) "+")))
  (setq old-edit-line-text (buffer-substring (point-min) (point-max)))
  (setq old-edit-line-position (point))
  (select-window old-window)
  (local-set-key [delete] 'absolute-choose-range-or-cell)
  (local-set-key [backspace] 'absolute-choose-range-or-cell) ;observe C-h is unbound
  (local-set-key [return] 'absolute-choose-range-or-cell)
  (local-set-key [?\ ] 'absolute-choose-range-or-cell)
  (local-set-key [tab] 'absolute-choose-range-or-cell)
  (let ((c ?!)) (while (<= c ?~) (local-set-key (concat (cons c nil)) 'absolute-choose-range-or-cell)
		       (setq c (+ c 1))))
  )


(defun relative-choose-range-or-cell () "insert (cell) or (range) command into the edit line corresponding to the cursor/region"
  (interactive)
  (let ((text
	 (if esheet-region
	     (concat (if infixFlag "range(" "(range ")
		     (if infixFlag "x+" "(+ x ")
		     (int-to-string (- (min (caar esheet-region) (cadr esheet-region)) oldCursX))
		     (if infixFlag "," ") ")
		     (if infixFlag "y+" "(+ y ")
		     (int-to-string (- (min (cdar esheet-region) (cddr esheet-region)) oldCursY))
		     (if infixFlag "," ") ")
		     (if infixFlag "x+" "(+ x ")
		     (int-to-string (- (max (caar esheet-region) (cadr esheet-region)) oldCursX))
		     (if infixFlag "," ") ")
		     (if infixFlag "y+" "(+ y ")
		     (int-to-string (- (max (cdar esheet-region) (cddr esheet-region)) oldCursY))
		     ")" (if (not infixFlag) ")"))
	   (concat (if infixFlag "cell(x+(" "(cell (+ x ")
		   (int-to-string (- cursX oldCursX)) 
		   (if infixFlag "),y+(" ") (+ y ")
		   (int-to-string (- cursY oldCursY)) "))")
	   )
	 ))
    (unhighlite-region)
    (curs oldCursX oldCursY)
    (local-set-key [delete] 'intel-del)
    (local-set-key [backspace] 'intel-del) ;observe C-h is unbound
    (local-set-key [return] 'edit-cell-value)
    (local-set-key [?\ ] 'edit-cell-value)
    (local-set-key [tab] 'edit-cell-value)
    (let ((c ?!)) (while (<= c ?~) (local-set-key (concat (cons c nil)) 'replace-cell-value) 
			 (setq c (+ c 1))))
    (setq esheet-region nil)
    (select-window (get-buffer-window (get-buffer "*edit line*")))
    (erase-buffer)
    (insert old-edit-line-text)
    (goto-char old-edit-line-position)
    (insert text)
    )
  )

(defun get-relative-cell-or-region () "leave the edit line temporarily to get a cell or range command conviniently"
  (interactive)
  (setq oldCursX cursX)
  (setq oldCursY cursY)
  (setq infixFlag (and (> (length (buffer-string)) 0) (equal (buffer-substring 1 2) "+")))
  (setq old-edit-line-text (buffer-substring (point-min) (point-max)))
  (setq old-edit-line-position (point))
  (select-window old-window)
  (local-set-key [delete] 'relative-choose-range-or-cell)
  (local-set-key [backspace] 'relative-choose-range-or-cell) ;observe C-h is unbound
  (local-set-key [return] 'relative-choose-range-or-cell)
  (local-set-key [?\ ] 'relative-choose-range-or-cell)
  (local-set-key [tab] 'relative-choose-range-or-cell)
  (let ((c ?!)) (while (<= c ?~) (local-set-key (concat (cons c nil)) 'relative-choose-range-or-cell)
		       (setq c (+ c 1))))
  )

