;;!emacs
;;
;; FILE:         hui-window.el
;; SUMMARY:      Smart Mouse Key window and modeline depress/release actions.
;; USAGE:        GNU Emacs Lisp Library, Load only when mouse is available.
;; KEYWORDS:     hypermedia, mouse
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola, Inc., PWDG
;;
;; ORIG-DATE:    21-Sep-92
;; LAST-MOD:      6-Oct-95 at 12:56:48 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1992-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;;
;;   Must be loaded AFTER hmouse-alist has been defined in
;;   "hui-mouse.el".
;;
;;   Handles drags in same window or across windows and modeline depresses.
;;
;; What drags and modeline presses do.
;; ==============================================================================
;;                                              Smart Keys
;; Context                         Action Key                 Assist Key
;; ==============================================================================
;; Drag horizontally within window
;;     Left to right               Scroll to buffer end       Split window across
;;     Right to left               Scroll to buffer begin     Delete window
;; Click in modeline
;;     Left window edge            Bury buffer                Unbury bottom buffer
;;     Right window edge           Info                       Smart Key Summary
;;     Otherwise                   Action Key Hook            Assist Key Hook
;; Modeline depress & wind release Resize window height       <- same
;; Drag from shared window side    Resize window's width      <- same
;; Drag from one window to another Create/modify a link but   Swap buffers
;; Drag vertically within window   Split window sideways      <- same
;; Drag diagonally within window   Save ring frame-config     Restore ring config
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar action-key-modeline-hook 'hmouse-context-menu
  "A list of functions to call when the Action Mouse Key is clicked in the center portion of a modeline.")

(defvar assist-key-modeline-hook nil
  "A list of functions to call when the Assist Mouse Key is clicked in the center portion of a modeline.")

(defvar hmouse-edge-sensitivity 3
  "*Number of characters from window edges within which a click is considered at an edge.")

(defvar hmouse-side-sensitivity (if hyperb:emacs19-p 2 1)
  "*Characters in either direction from window side within which a click is considered on the side.")

(defvar hmouse-x-drag-sensitivity 5
  "*Number of chars mouse must move horizontally between depress/release to register a horizontal drag.")

(defvar hmouse-y-drag-sensitivity 3
  "*Number of lines mouse must move vertically between depress/release to register a vertical drag.")

(defvar hmouse-x-diagonal-sensitivity 4
  "*Number of chars mouse must move horizontally between depress/release to register a diagonal drag.")
(defvar hmouse-y-diagonal-sensitivity 3
  "*Number of lines mouse must move vertically between depress/release to register a diagonal drag.")

;;;
;;; Add mode line handling to hmouse-alist dispatch table.
;;;
(if (not (boundp 'hmouse-alist))
    (error
      "\"hui-modeln.el\": hmouse-alist must be defined before loading this.")
  (or (memq 'hmouse-drag-window-side
	    (mapcar (function (lambda (elt) (let ((pred (car elt)))
					      (if (listp pred) (car pred)))))
		    hmouse-alist))
      (setq hmouse-alist
	    (append
	      '(
		((hmouse-drag-window-side) .
		 ((hmouse-resize-window-side) .
		  (hmouse-resize-window-side 'assist)))
		((setq hkey-value 
		       (and (not (hmouse-drag-between-windows))
			    (hmouse-drag-horizontally))) .
		 ((hmouse-horizontal) . (hmouse-horizontal-assist)))
		((hmouse-modeline-depress) .
		 ((action-key-modeline) . (assist-key-modeline)))
		((hmouse-drag-between-windows) .
		 ((hui:link-directly) . (hmouse-swap-buffers 'assist)))
		((hmouse-drag-vertically) .
		 ((sm-split-window-horizontally) .
		  (sm-split-window-horizontally)))
		((setq hkey-value (hmouse-drag-diagonally)) .
		 ((wconfig-ring-save) .
		  (wconfig-yank-pop
		    (prefix-numeric-value current-prefix-arg))))
		)
	      hmouse-alist))))


;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hmouse-drag-between-windows ()
  "Returns non-nil if last Action Key depress and release were in different windows.
If free variable 'assist-flag' is non-nil, uses Assist Key."
  (if assist-flag
      (and assist-key-depress-window assist-key-release-window
	   (not (eq assist-key-depress-window
		    assist-key-release-window)))
    (and action-key-depress-window action-key-release-window
	 (not (eq action-key-depress-window action-key-release-window)))))

(defun hmouse-drag-diagonally ()
  "Returns non-nil iff last Action Key use was a diagonal drag within a single window.
If free variable 'assist-flag' is non-nil, uses Assist Key.
Value returned is nil if not a diagonal drag, or one of the following symbols
depending on the direction of the drag: southeast, southwest, northwest, northeast."
  (let ((last-depress-x) (last-release-x)
	(last-depress-y) (last-release-y))
    (if assist-flag
	(setq last-depress-x (hmouse-x-coord assist-key-depress-args)
	      last-release-x (hmouse-x-coord assist-key-release-args)
	      last-depress-y (hmouse-y-coord assist-key-depress-args)
	      last-release-y (hmouse-y-coord assist-key-release-args))
      (setq last-depress-x (hmouse-x-coord action-key-depress-args)
	    last-release-x (hmouse-x-coord action-key-release-args)
	    last-depress-y (hmouse-y-coord action-key-depress-args)
	    last-release-y (hmouse-y-coord action-key-release-args)))
    (and last-depress-x last-release-x last-depress-y last-release-y
	 (>= (- (max last-depress-x last-release-x)
		(min last-depress-x last-release-x))
	     hmouse-x-diagonal-sensitivity)
	 (>= (- (max last-depress-y last-release-y)
		(min last-depress-y last-release-y))
	     hmouse-y-diagonal-sensitivity)
	 (cond
	   ((< last-depress-x last-release-x)
	    (if (< last-depress-y last-release-y)
		'southeast 'northeast))
	   (t (if (< last-depress-y last-release-y)
		  'southwest 'northwest))))))

(defun hmouse-drag-horizontally ()
  "Returns non-nil iff last Action Key use was a horizontal drag within a single window.
If free variable 'assist-flag' is non-nil, uses Assist Key.
Value returned is nil if not a horizontal drag, 'left if drag moved left or
'right otherwise."
  (let ((last-depress-x) (last-release-x)
	(last-depress-y) (last-release-y))
    (if assist-flag
	(setq last-depress-x (hmouse-x-coord assist-key-depress-args)
	      last-release-x (hmouse-x-coord assist-key-release-args)
	      last-depress-y (hmouse-y-coord assist-key-depress-args)
	      last-release-y (hmouse-y-coord assist-key-release-args))
      (setq last-depress-x (hmouse-x-coord action-key-depress-args)
	    last-release-x (hmouse-x-coord action-key-release-args)
	    last-depress-y (hmouse-y-coord action-key-depress-args)
	    last-release-y (hmouse-y-coord action-key-release-args)))
    (and last-depress-x last-release-x last-depress-y last-release-y
	 (>= (- (max last-depress-x last-release-x)
		(min last-depress-x last-release-x))
	     hmouse-x-drag-sensitivity)
	 ;; Don't want to register vertical drags here, so ensure any
	 ;; vertical movement was less than the vertical drag sensitivity.
	 (< (- (max last-depress-y last-release-y)
	       (min last-depress-y last-release-y))
	    hmouse-y-drag-sensitivity)
	 (if (< last-depress-x last-release-x) 'right 'left))))

(defun hmouse-drag-vertically ()
  "Returns non-nil iff last Action Key use was a vertical drag within a single window.
If free variable 'assist-flag' is non-nil, uses Assist Key.
Value returned is nil if not a vertical line drag, 'up if drag moved up or
'down otherwise."
  (let ((last-depress-x) (last-release-x)
	(last-depress-y) (last-release-y))
    (if assist-flag
	(setq last-depress-x (hmouse-x-coord assist-key-depress-args)
	      last-release-x (hmouse-x-coord assist-key-release-args)
	      last-depress-y (hmouse-y-coord assist-key-depress-args)
	      last-release-y (hmouse-y-coord assist-key-release-args))
      (setq last-depress-x (hmouse-x-coord action-key-depress-args)
	    last-release-x (hmouse-x-coord action-key-release-args)
	    last-depress-y (hmouse-y-coord action-key-depress-args)
	    last-release-y (hmouse-y-coord action-key-release-args)))
    (and last-depress-x last-release-x last-depress-y last-release-y
	 (>= (- (max last-depress-y last-release-y)
		(min last-depress-y last-release-y))
	     hmouse-y-drag-sensitivity)
	 ;; Don't want to register horizontal drags here, so ensure any
	 ;; horizontal movement was less than or equal to the horizontal drag
	 ;; sensitivity.
	 (<= (- (max last-depress-x last-release-x)
		(min last-depress-x last-release-x))
	     hmouse-x-drag-sensitivity)
	 (if (< last-depress-y last-release-y) 'down 'up))))

(or (fboundp 'abs)
    (defun abs (number)
      "Return the absolute value of NUMBER."
      (cond
	((< number 0)
	 (- 0 number))
	(t number))))

(defun hmouse-drag-window-side ()
  "Returns non-nil if Action Key was dragged from a window side divider.
If free variable 'assist-flag' is non-nil, uses Assist Key."
  (cond (hyperb:xemacs-p
	 ;; Depress events in scrollbars or in non-text area of buffer are
	 ;; not visible or identifiable at the Lisp-level, so always return
	 ;; nil.
	 nil)
	(hyperb:window-system
	 (let* ((depress-args (if assist-flag assist-key-depress-args
				action-key-depress-args))
		(release-args (if assist-flag assist-key-release-args
				action-key-release-args))
		(w (smart-window-of-coords depress-args))
		(side-ln (and w (1- (nth 2 (window-edges w)))))
		(last-press-x   (hmouse-x-coord depress-args))
		(last-release-x (hmouse-x-coord release-args)))
	   (and last-press-x last-release-x side-ln
		(/= last-press-x last-release-x)
		(/= (1+ side-ln) (frame-width))
		(<= (max (- last-press-x side-ln) (- side-ln last-press-x))
		    hmouse-side-sensitivity))))))

(defun sm-split-window-horizontally ()
  "Splits current window in two evenly, side by side.
Beeps and prints message if can't split window further."
  (interactive)
  (let ((window-min-width 5))
    (condition-case ()
	(split-window-horizontally nil)
      (error (progn (beep)
		    (message
		     "(sm-split-window-horizontally): Can't split window further."))))))

(defun sm-split-window-vertically ()
  "Splits current window in two evenly, one above the other.
Beeps and prints message if can't split window further."
  (interactive)
  (let ((window-min-height 2))
    (condition-case ()
	(if (fboundp 'split-window-quietly)
	    (split-window-quietly nil)
	  (split-window-vertically nil))
      (error
	(progn
	  (beep)
	  (message
	    "(sm-split-window-vertically): Can't split window further."))))))

(defun smart-coords-in-window-p (coords window)
  "Tests if COORDS are in WINDOW.  Returns WINDOW if they are, nil otherwise."
  (cond ((and hyperb:emacs19-p (eventp coords))
	 (eq (posn-window (event-start coords)) window))
	((if hyperb:xemacs-p
	     (if (eventp coords)
		 (eq (event-window coords) window)
	       (eq (car coords) window))))
	((fboundp 'window-edges)
	 (let* ((edges (window-edges window))
		  (w-xmin (nth 0 edges))
		  (w-ymin (nth 1 edges))
		  (w-xmax (nth 2 edges))
		  (w-ymax (nth 3 edges))
		  (x  (hmouse-x-coord coords))
		  (y  (hmouse-y-coord coords)))
	     (and (<= w-xmin x) (<= x w-xmax)
		  (<= w-ymin y) (<= y w-ymax)
		  window)))))

(defun smart-window-of-coords (coords)
  "Returns window in which COORDS fall or nil if none.
Ignores minibuffer window."
  (cond ((and hyperb:emacs19-p (eventp coords))
	 (posn-window (event-start coords)))
	((if hyperb:xemacs-p
	     (if (eventp coords)
		 (event-window coords)
	       (car coords))))
	(t (let ((window-list (hypb:window-list 'no-minibuf))
		 (window)
		 (w))
	     (while (and (not window) window-list)
	       (setq w (car window-list)
		     window-list (cdr window-list)
		     window (smart-coords-in-window-p coords w)))
	     window))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hmouse-context-menu ()
  "If running under a window system, display or hide the buffer menu.
If not running under a window system and Smart Menus are loaded, display the
appropriate Smart Menu for the context at point."
  (if (and (fboundp 'smart-menu)
	   (or (null window-system)
	       (not (or hyperb:lemacs-p hyperb:emacs19-p))))
      (smart-menu)
    (let ((wind (get-buffer-window "*Buffer List*"))
	  owind)
      (if wind
	  (unwind-protect
	      (progn (setq owind (selected-window))
		     (select-window wind)
		     (bury-buffer nil))
	    (select-window owind))
	(buffer-menu nil)))))

(defun hmouse-horizontal ()
  "Goes to buffer end if drag was to the right, otherwise goes to beginning."
  (if (eq hkey-value 'right)
      (end-of-buffer)
    (beginning-of-buffer)))

(defun hmouse-horizontal-assist ()
  "Splits window vertically if drag was to the right, otherwise deletes window."
  (if (eq hkey-value 'right)
      (sm-split-window-vertically)
    (delete-window)))

(defun action-key-modeline ()
  "Handles Action Key depresses on a window mode line.
If key is:
 (1) clicked on left edge of a window's modeline,
     window's buffer is buried (placed at bottom of buffer list);
 (2) clicked on right edge of a window's modeline,
     the Info buffer is displayed, or if already displayed and the
     modeline clicked belongs to a window displaying Info, the Info
     buffer is hidden;
 (3) clicked anywhere in the middle of a window's modeline,
     the functions listed in 'action-key-modeline-hook' are called;
 (4) dragged vertically from modeline to within a window,
     the modeline is moved to point of key release, thereby resizing
     its window and potentially its vertical neighbors."
  (let ((w (smart-window-of-coords action-key-depress-args)))
    (if w (select-window w))
    (cond ((hmouse-modeline-click)
	   (cond ((hmouse-release-left-edge)  (bury-buffer))
		 ((hmouse-release-right-edge)
		  (if (eq major-mode 'Info-mode)
		      (Info-exit)
		    (info)))
		 (t (run-hooks 'action-key-modeline-hook))))
	  (t (hmouse-modeline-resize-window)))))

(defun assist-key-modeline ()
  "Handles Assist Key depresses on a window mode line.
If secondary key is:
 (1) clicked on left edge of a window's modeline,
     bottom buffer in buffer list is unburied and placed in window;
 (2) clicked on right edge of a window's modeline,
     the summary of Smart Key behavior is displayed, or if already
     displayed and the modeline clicked belongs to a window displaying
     the summary, the summary buffer is hidden;
 (3) clicked anywhere in the middle of a window's modeline,
     the functions listed in 'assist-key-modeline-hook' are called;
 (4) dragged vertically from modeline to within a window,
     the modeline is moved to point of key release, thereby resizing
     its window and potentially its vertical neighbors."
  (let ((buffers)
	(w (smart-window-of-coords assist-key-depress-args)))
    (if w (select-window w))
    (cond ((hmouse-modeline-click 'assist)
	   (cond ((hmouse-release-left-edge 'assist)
		  (if (fboundp 'last)
		      (switch-to-buffer (car (last (buffer-list))))
		    (setq buffers (buffer-list))
		    (switch-to-buffer (nth (1- (length buffers)) buffers))))
		 ((hmouse-release-right-edge 'assist)
		  (if (equal (buffer-name) (hypb:help-buf-name "Smart"))
		      (hkey-help-hide)
		    (hkey-summarize 'current-window)))
		 (t (run-hooks 'assist-key-modeline-hook))))
	  (t (hmouse-modeline-resize-window 'assist)))))

(defun hmouse-modeline-click (&optional assist-flag)
  "Returns non-nil if last Action Key depress and release was at same point in a modeline.
Optional ASSIST-FLAG non-nil means test for Assist Key click instead."
  ;; Assume depress was in modeline and that any drag has already been handled.
  ;; So just check that release was in modeline.
  (hmouse-modeline-release assist-flag))

(defun hmouse-modeline-depress ()
  "Returns non-nil if Action Key was depressed on a window mode line.
If free variable 'assist-flag' is non-nil, uses Assist Key."
  (let ((args (if assist-flag assist-key-depress-args
		action-key-depress-args)))
    (if (and hyperb:window-system args)
	(if (fboundp 'event-over-modeline-p)
	    (event-over-modeline-p args)
	  (let* ((w (smart-window-of-coords args))
		 (mode-ln (if w (nth 3 (window-edges w))))
		 (last-press-y (hmouse-y-coord args)))
	    ;; Mode-line is always 1 less than the bottom of the window, unless it
	    ;; is a minibuffer window which does not have a modeline.
	    (if (not (eq w (minibuffer-window))) (setq mode-ln (1- mode-ln)))
	    (and last-press-y mode-ln (= last-press-y mode-ln)))))))

(defun hmouse-modeline-release (&optional assist-flag)
  "Returns non-nil if Action Key was released on a window mode line.
Optional non-nil ASSIST-FLAG means test release of Assist Key instead."
  (let ((args (if assist-flag assist-key-release-args
		action-key-release-args)))
    (if (and hyperb:window-system args)
	(if (fboundp 'event-over-modeline-p)
	    (event-over-modeline-p args)
	  (let* ((w (smart-window-of-coords args))
		 (mode-ln (and w (1- (nth 3 (window-edges w)))))
		 (last-press-y (hmouse-y-coord args)))
	    (and last-press-y mode-ln (= last-press-y mode-ln)))))))

(defun hmouse-modeline-resize-window (&optional assist-flag)
  "Resizes window whose mode line was depressed upon by the Action Key.
Resize amount depends upon the vertical difference between press and release
of the Action Key.  Optional arg ASSIST-FLAG non-nil means use values from
Assist Key instead."
  (cond ((not hyperb:window-system) nil)
	((and hyperb:xemacs-p (not (fboundp 'window-edges)))
	 (error "Drag from a mode-line with button1 to resize windows."))
	(t (let* ((owind (selected-window))
		  (window (smart-window-of-coords
			   (if assist-flag assist-key-depress-args
			     action-key-depress-args)))
		  (mode-ln (and window (1- (nth 3 (window-edges window)))))
		  (last-release-y
		   (hmouse-y-coord
		    (if assist-flag assist-key-release-args
		      action-key-release-args)))
		  (shrink-amount (- mode-ln last-release-y)))
	     ;; Restore position of point prior to Action Key release.
	     (if action-key-release-prev-point
		 (let ((obuf (current-buffer)))
		   (unwind-protect
		       (progn
			 (set-buffer
			  (marker-buffer action-key-release-prev-point))
			 (goto-char
			  (marker-position action-key-release-prev-point)))
		     (set-buffer obuf))))
	     (cond
	      ((>= (+ mode-ln 2) (frame-height))
	       (error
		"(hmouse-modeline-resize-window): Can't move bottom window in frame."))
	      ((< (length (hypb:window-list 'no-minibuf)) 2)
	       (error
		"(hmouse-modeline-resize-window): Can't resize sole window in frame."))
	      (t (unwind-protect
		     (progn
		       (select-window window)
		       (shrink-window shrink-amount)
		       ;; Keep redisplay from scrolling other window.
		       (select-window (next-window nil 'no-mini))
		       (condition-case ()
			   (scroll-down shrink-amount)
			 (error nil)))
		   (select-window owind))))))))

(defun hmouse-release-left-edge (&optional assist-flag)
  "Returns non-nil if last Action Key release was at left window edge.
'hmouse-edge-sensitivity' value determines how near to actual edge the
release must be."
  (let ((args (if assist-flag assist-key-release-args
		 action-key-release-args))
	window-left last-release-x)
    (if (fboundp 'window-lowest-p) ;; XEmacs >= 19.12 
	(setq last-release-x (and args (eq (event-window args)
					   (selected-window))
				  (hmouse-x-coord args))
	      window-left 0)
      (setq window-left (car (window-edges))
	    last-release-x (and args (hmouse-x-coord args))))
    (and last-release-x (< (- last-release-x window-left)
			   hmouse-edge-sensitivity)
	 (>= (- last-release-x window-left) 0))))

(defun hmouse-release-right-edge (&optional assist-flag)
  "Returns non-nil if last Action Key release was at right window edge.
'hmouse-edge-sensitivity' value determines how near to actual edge the
release must be."
  (let ((args (if assist-flag assist-key-release-args
		 action-key-release-args))
	window-right last-release-x)
    (if (fboundp 'window-lowest-p) ;; XEmacs >= 19.12 
	(setq last-release-x (and args (eq (event-window args)
					   (selected-window))
				  (hmouse-x-coord args))
	      window-right (window-width))
      (setq window-right (nth 2 (window-edges))
	    last-release-x (and args (hmouse-x-coord args))))
    (and last-release-x (>= (+ last-release-x hmouse-edge-sensitivity)
			    window-right)
	 (>= (- window-right last-release-x) 0))))

(defun hmouse-resize-window-side (&optional assist-flag)
  "Resizes window whose side was depressed upon by the Action Key.
Resize amount depends upon the horizontal difference between press and release
of the Action Key.  Optional arg ASSIST-FLAG non-nil means use values from
Assist Key instead."
  (cond (hyperb:xemacs-p
	 ;; Depress events in scrollbars or in non-text area of buffer are
	 ;; not visible or identifiable at the Lisp-level, so always return
	 ;; nil.
	 nil)
	(hyperb:window-system
	 (let* ((owind (selected-window))
		(window (smart-window-of-coords
			 (if assist-flag assist-key-depress-args
			   action-key-depress-args)))
		(side-ln (and window (1- (nth 2 (window-edges window)))))
		(last-release-x
		 (hmouse-x-coord
		  (if assist-flag assist-key-release-args
		    action-key-release-args)))
		(shrink-amount (- side-ln last-release-x))
		)
	   ;; Restore position of point prior to Action Key release.
	   (if action-key-release-prev-point
	       (let ((obuf (current-buffer)))
		 (unwind-protect
		     (progn
		       (set-buffer (marker-buffer action-key-release-prev-point))
		       (goto-char (marker-position action-key-release-prev-point)))
		   (set-buffer obuf))))
	   (cond
	    ((>= (+ side-ln 2) (frame-width))
	     (error
	      "(hmouse-resize-window-side): Can't change width of full frame width window."))
	    ((< (length (hypb:window-list 'no-minibuf)) 2)
	     (error
	      "(hmouse-resize-window-side): Can't resize sole window in frame."))
	    (t (unwind-protect
		   (progn
		     (select-window window)
		     (shrink-window-horizontally shrink-amount))
		 (select-window owind))))))))

(defun hmouse-swap-buffers (&optional assist-flag)
  "Swaps buffers in windows selected with last Action Key depress and release.
If optional arg ASSIST-FLAG is non-nil, uses Assist Key."
  (let* ((w1 (if assist-flag assist-key-depress-window
	       action-key-depress-window))
	 (w2 (if assist-flag assist-key-release-window
	       action-key-release-window))
	 (w1-buf (and w1 (window-buffer w1)))
	 (w2-buf (and w2 (window-buffer w2)))
	 )
    (or (and w1 w2)
	(error "(hmouse-swap-buffers): Last depress or release not within a window."))
    ;; Swap window buffers.
    (set-window-buffer w1 w2-buf)
    (set-window-buffer w2 w1-buf)))

(defun hmouse-swap-windows (&optional assist-flag)
  "Swaps windows selected with last Action Key depress and release.
If optional arg ASSIST-FLAG is non-nil, uses Assist Key."
  (let* ((w1 (if assist-flag assist-key-depress-window
	       action-key-depress-window))
	 (w2 (if assist-flag assist-key-release-window
	       action-key-release-window))
	 (w1-width  (and w1 (window-width w1)))
	 (w1-height (and w1 (window-height w1)))
	 (w2-width  (and w2 (window-width w2)))
	 (w2-height (and w2 (window-height w2)))
	 )
    (or (and w1 w2)
	(error "(hmouse-swap-windows): Last depress or release not within a window."))
    (unwind-protect
	(progn
	  (select-window w1)
	  (if (not (= w1-height (frame-height)))
	      (shrink-window (- w1-height w2-height)))
	  (if (not (= w1-width (frame-width)))
	      (shrink-window-horizontally (- w1-width w2-width)))
	  (select-window w2)
	  (setq w2-width (window-width w2)
		w2-height (window-height w2))
	  (if (not (= w2-height (frame-height)))
	      (shrink-window (- w2-height w1-height)))
	  (if (not (= w2-width (frame-width)))
	      (shrink-window-horizontally (- w2-width w1-width)))
	  )
      (select-window w2)
      )))

(defun hmouse-x-coord (args)
  "Returns x coordinate in chars from window system dependent ARGS."
  (let ((x (eval (cdr (assoc hyperb:window-system
			     '(("emacs19" . (if (eventp args)
						(+ (car (posn-col-row
							 (event-start args)))
						   (nth 0 (window-edges
							   (car
							    (car (cdr args))
							    ))))
					      (car args)))
			       ("lemacs" .  (if (eventp args)
						(event-x args)
					      (car args)))
			       ("xterm"  .  (car args))
			       ("epoch"  .  (nth 0 args))   ;; Epoch V4
			       ("sun"    .  (nth 1 args))
			       ("next"   .  (nth 1 args))
			       ("apollo" .  (car args))
			       ))))))
    (if (integerp x) x (error "(hmouse-x-coord): invalid X coord: %s" x))))

(defun hmouse-y-coord (args)
  "Returns y coordinate in frame lines from window system dependent ARGS."
  (let ((y (eval (cdr (assoc hyperb:window-system
			     '(("emacs19" . (if (eventp args)
						(+ (cdr (posn-col-row
							 (event-start args)))
						   (nth 1 (window-edges
							   (car
							    (car (cdr args))
							    ))))
					      (cdr args)))
			       ("lemacs" .  (if (eventp args)
						(event-y args)
					      (cdr args)))
			       ("xterm"  .  (nth 1 args))
			       ("epoch"  .  (nth 1 args))   ;; Epoch V4
			       ("sun"    .  (nth 2 args))
			       ("next"   .  (nth 2 args))
			       ("apollo" .  (nth 1 args))
			       ))))))
    (if (integerp y) y (error "(hmouse-y-coord): invalid Y coord: %s" y))))


;;; ************************************************************************
;;; Private variables
;;; ************************************************************************


(provide 'hui-window)
