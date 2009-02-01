(defmacro reset-mice (drag up)
  `#'(lambda (a b) (interactive) 
       (setq mouse-track-drag-hook ,drag)
       (setq mouse-track-drag-up-hook ,up)))

(defun esheet-mouse-down (a b)
  (interactive)
  (unhighlite-region)
  (if (eq major-mode 'esheet-mode)
      (if (equal (buffer-substring (- (point) 1) (point)) "\n")
	  (progn (fset 'esheet-temporary-mouse-func (reset-mice mouse-track-drag-hook mouse-track-drag-up-hook))
		 (setq mouse-track-drag-hook nil)
		 (setq mouse-track-drag-up-hook (cons esheet-temporary-mouse-func nil)))

      (progn
        (unhighlite-region)
        (default-mouse-track-set-point-in-window a (selected-window))
        (curs (/ (current-column) 6) (- (count-lines 1 (point)) 1))
        (mess-where-you)
        (setq esheet-region nil)
        t) nil)))

(defun esheet-block-click (a b)
  (interactive)
  (eq major-mode 'esheet-mode))

(defun esheet-drag (a &rest args)
  (interactive)
  (unhighlite-region)
  (default-mouse-track-set-point-in-window a (selected-window))
  (setq esheet-region
        (cons
         (cons cursX cursY)
         (cons (/ (current-column) 6) (- (count-lines 1 (point)) 1))))
  (highlite-region)
  (mess-where-you))

(defun esheet-drag-up (a b)
  (interactive)
  (unhighlite-region)
  (default-mouse-track-set-point-in-window a (selected-window))
  (curs (/ (current-column) 6) (- (count-lines 1 (point)) 1))
  (highlite-region) t)

