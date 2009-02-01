;;!emacs
;;
;; FILE:         hmouse-drv.el
;; SUMMARY:      Smart Key/Mouse driver functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia, mouse
;;
;; AUTHOR:       Bob Weiner
;; ORIG-DATE:    04-Feb-90
;; LAST-MOD:      1-Nov-95 at 21:44:52 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1989-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hypb)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar action-key-depress-window nil
  "The last window in which the Action Key was depressed or nil.")
(defvar assist-key-depress-window nil
  "The last window in which the Assist Key was depressed or nil.")
(defvar action-key-release-window nil
  "The last window in which the Action Key was released or nil.")
(defvar assist-key-release-window nil
  "The last window in which the Assist Key was released or nil.")

(defvar action-key-depress-prev-point nil
  "Marker at point prior to last Action Key depress.
Note that this may be a buffer different than where the depress occurs.")
(defvar assist-key-depress-prev-point nil
  "Marker at point prior to last Assist Key depress.
Note that this may be a buffer different than where the depress occurs.")
(defvar action-key-release-prev-point nil
  "Marker at point prior to last Action Key release.
Note that this may be a buffer different than where the release occurs.")
(defvar assist-key-release-prev-point nil
  "Marker at point prior to last Assist Key release.
Note that this may be a buffer different than where the release occurs.")

(defvar action-key-cancelled nil
  "When non-nil, cancels last Action Key depress.")
(defvar assist-key-cancelled nil
  "When non-nil, cancels last Assist Key depress.")

(defvar action-key-help-flag nil
  "When non-nil, forces display of help for next Action Key release.")
(defvar assist-key-help-flag nil
  "When non-nil, forces display of help for next Assist Key release.")

;;; ************************************************************************
;;; Hyperbole context-sensitive key driver functions
;;; ************************************************************************

(defun action-mouse-key (&rest args)
  "Set point to the current mouse cursor position and execute 'action-key'.
Any ARGS will be passed to 'hmouse-function'."
  (interactive)
  (require 'hsite)
  ;; Make this a no-op if some local mouse key binding overrode the global
  ;; action-key-depress command invocation.
  (if action-key-depressed-flag
      (let ((hkey-alist hmouse-alist))
	(setq action-key-depressed-flag nil)
	(cond (action-key-cancelled
		(setq action-key-cancelled nil
		      assist-key-depressed-flag nil))
	      (assist-key-depressed-flag
		(hmouse-function nil nil args))
	      ((action-mouse-key-help nil args))
	      (t (hmouse-function 'action-key nil args))))))

(defun assist-mouse-key (&rest args)
  "Set point to the current mouse cursor position and execute 'assist-key'.
Any ARGS will be passed to 'hmouse-function'."
  (interactive)
  (require 'hsite)
  ;; Make this a no-op if some local mouse key binding overrode the global
  ;; assist-key-depress command invocation.
  (if assist-key-depressed-flag
      (let ((hkey-alist hmouse-alist))
	(setq assist-key-depressed-flag nil)
	(cond (assist-key-cancelled
		(setq assist-key-cancelled nil
		      action-key-depressed-flag nil))
	      (action-key-depressed-flag
		(hmouse-function nil t args))
	      ((action-mouse-key-help t args))
	      (t (hmouse-function 'assist-key t args))))))

(defun hmouse-function (func assist-flag set-point-arg-list)
  "Executes FUNC for Action Key (Assist Key with ASSIST-FLAG non-nil) and sets point from SET-POINT-ARG-LIST.
FUNC may be nil in which case no function is called.
SET-POINT-ARG-LIST is passed to the call of the command bound to
'hmouse-set-point-command'.  Returns nil if 'hmouse-set-point-command' variable
is not bound to a valid function."
  (if (fboundp hmouse-set-point-command)
      (let ((release-args (hmouse-set-point set-point-arg-list)))
	(if assist-flag
	    (setq assist-key-release-window (selected-window)
		  assist-key-release-args release-args
		  assist-key-release-prev-point (point-marker))
	  (setq action-key-release-window (selected-window)
		action-key-release-args release-args
		action-key-release-prev-point (point-marker)))
	(and (eq major-mode 'br-mode)
	     (setq action-mouse-key-prev-window 
		   (if (br-in-view-window-p)
		       (save-window-excursion
			 (br-next-listing-window)
			 (selected-window))
		     (selected-window))))
	(setq action-mouse-key-prefix-arg current-prefix-arg)
	(if (null func)
	    nil
	  (funcall func)
	  (setq action-mouse-key-prev-window nil
		action-mouse-key-prefix-arg nil))
	t)))

(defun action-mouse-key-help (assist-flag args)
  "If a Smart Key help flag is set and the other Smart Key is not down, shows help.
Takes two args:  ASSIST-FLAG should be non-nil iff command applies to the Assist Key.
ARGS is a list of arguments passed to 'hmouse-function'.
Returns t if help is displayed, nil otherwise."
  (let ((help-shown)
	(other-key-released (not (if assist-flag
				     action-key-depressed-flag
				   assist-key-depressed-flag))))
    (unwind-protect
	(setq help-shown
	      (cond ((and  action-key-help-flag other-key-released)
		     (setq action-key-help-flag nil)
		     (hmouse-function 'hkey-help assist-flag args)
		     t)
		    ((and  assist-key-help-flag other-key-released)
		     (setq assist-key-help-flag nil)
		     (hmouse-function 'assist-key-help assist-flag args)
		     t)))
      (if help-shown
	  ;; Then both Smart Keys have been released. 
	  (progn (setq action-key-cancelled nil
		       assist-key-cancelled nil)
		 t)))))

(defun action-key ()
  "Use one key to perform functions that vary by buffer.
Default function is given by 'action-key-default-function' variable.
Returns t unless 'action-key-default-function' variable is not bound to a valid
function."
  (interactive)
  (require 'hsite)
  (or (hkey-execute nil)
      (if (fboundp action-key-default-function)
	 (progn (funcall action-key-default-function)
		t))))

(defun assist-key ()
  "Use one assist-key to perform functions that vary by buffer.
Default function is given by 'assist-key-default-function' variable.
Returns non-nil unless 'assist-key-default-function' variable is not bound
to a valid function."
  (interactive)
  (require 'hsite)
  (or (hkey-execute t)
      (if (fboundp assist-key-default-function)
	  (progn (funcall assist-key-default-function)
		 t))))

(defun hkey-execute (assist-flag)
  "Evaluate Action Key form (or Assist Key form with ASSIST-FLAG non-nil) for first non-nil predicate from 'hkey-alist'.
Non-nil ASSIST-FLAG means evaluate second form, otherwise evaluate first form.
Returns non-nil iff a non-nil predicate is found."
    (let ((pred-forms hkey-alist)
	  (pred-form) (pred-t))
      (while (and (null pred-t) (setq pred-form (car pred-forms)))
	(if (setq pred-t (eval (car pred-form)))
	    (eval (if assist-flag (cdr (cdr pred-form)) (car (cdr pred-form))))
	  (setq pred-forms (cdr pred-forms))))
      pred-t))

(defun hkey-help (&optional assist-flag)
  "Display help for the Action Key command in current context.
With optional ASSIST-FLAG non-nil, display help for the Assist Key command.
Returns non-nil iff associated help documentation is found."
  (interactive "P")
  (require 'hsite)
  (let ((pred-forms hkey-alist)
	(pred-form) (pred-t) (call) (cmd-sym) (doc))
    (while (and (null pred-t) (setq pred-form (car pred-forms)))
      (or (setq pred-t (eval (car pred-form)))
	  (setq pred-forms (cdr pred-forms))))
    (if pred-t
	(setq call (if assist-flag (cdr (cdr pred-form))
		     (car (cdr pred-form)))
	      cmd-sym (car call))
      (setq cmd-sym
	    (if assist-flag assist-key-default-function action-key-default-function)
	    call cmd-sym))
    (setq hkey-help-msg
	  (if (and cmd-sym (symbolp cmd-sym))
	      (progn
		(setq doc (documentation cmd-sym))
		(let* ((condition (car pred-form))
		       (temp-buffer-show-hook
			 (function
			   (lambda (buf)
			     (set-buffer buf)
			     (setq buffer-read-only t)
			     (if (br-in-browser)
				 (save-excursion
				   (let ((owind (selected-window)))
				     (br-to-view-window)
				     (select-window (previous-window))
				     (display-buffer buf 'other-win)
				     (select-window owind)))
			       (display-buffer buf 'other-win)))))
		       (temp-buffer-show-function temp-buffer-show-hook))
		  (with-output-to-temp-buffer (hypb:help-buf-name "Smart")
		    (princ (format "A click of the %s Key"
				   (if assist-flag "Assist" "Action")))
		    (terpri)
		    (princ "WHEN  ")
		    (princ
		      (or condition
			  "there is no matching context"))
		    (terpri)
		    (princ "CALLS ") (princ call)
		    (if doc (progn (princ " WHICH:") (terpri) (terpri)
				   (princ doc)))
		    (if (memq cmd-sym '(hui:hbut-act hui:hbut-help))
			(progn
			  (princ (format "\n\nBUTTON SPECIFICS:\n\n%s\n"
					 (actype:doc 'hbut:current t)))
			  (hattr:report
			    (nthcdr 2 (hattr:list 'hbut:current)))))
		    (terpri)
		    ))
		"")
	    (message "No %s Key command for current context."
		     (if assist-flag "Assist" "Action"))))
    doc))

(defun assist-key-help ()
  "Display doc associated with Assist Key command in current context.
Returns non-nil iff associated documentation is found."
  (interactive)
  (hkey-help 'assist))

(defun hkey-help-hide ()
  "Restores frame to configuration prior to help buffer display.
Point must be in the help buffer."
  (let ((buf (current-buffer)))
    (if *hkey-wconfig*
	(set-window-configuration *hkey-wconfig*)
      (switch-to-buffer (other-buffer)))
    (bury-buffer buf)
    (setq *hkey-wconfig* nil)))

(defun hkey-help-show (buffer &optional current-window)
  "Saves prior frame configuration if BUFFER displays help.  Displays BUFFER.

Optional second arg CURRENT-WINDOW non-nil forces display of buffer within
the current window.  By default, it is displayed in another window."
  (if (bufferp buffer) (setq buffer (buffer-name buffer)))
  (and (stringp buffer)
       (string-match "Help\\*$" buffer)
       (not (memq t (mapcar (function
			     (lambda (wind)
			       (string-match
				"Help\\*$"
				(buffer-name (window-buffer wind)))))
			    (hypb:window-list 'no-mini))))
       (setq *hkey-wconfig* (current-window-configuration)))
  (let* ((buf (get-buffer-create buffer))
	 (wind (if current-window
		   (progn (switch-to-buffer buf)
			  (selected-window))
		 (display-buffer buf))))
    (setq minibuffer-scroll-window wind)))

(defun hkey-operate (arg)
  "Uses the keyboard to emulate Smart Mouse Key drag actions.
Each invocation alternates between starting a drag and ending it.
Prefix ARG non-nil means emulate Assist Key rather than the Action Key.

Only works when running under a window system, not from a dumb terminal."
  (interactive "P")
  (or hyperb:window-system
      (hypb:error "(hkey-operate): Drag actions require mouse support"))
  (if arg
      (if assist-key-depressed-flag
	  (progn (assist-mouse-key)
		 (message "Assist Key released."))
	(assist-key-depress)
	(message
	  "Assist Key depressed; go to release point and hit {%s %s}."
	  (substitute-command-keys "\\[universal-argument]")
	  (substitute-command-keys "\\[hkey-operate]")
	  ))
    (if action-key-depressed-flag
	(progn (action-mouse-key)
	       (message "Action Key released."))
      (action-key-depress)
      (message "Action Key depressed; go to release point and hit {%s}."
	       (substitute-command-keys "\\[hkey-operate]"))
      )))

(defun hkey-summarize (&optional current-window)
  "Displays smart key operation summary in help buffer.
Optional arg CURRENT-WINDOW non-nil forces display of buffer within
the current window.  By default, it is displayed in another window."
  (let* ((doc-file (hypb:mouse-help-file))
	 (buf-name (hypb:help-buf-name "Smart"))
	 (wind (get-buffer-window buf-name))
	 owind)
    (if (file-readable-p doc-file)
	(progn
	  (if (br-in-browser)
	      (br-to-view-window))
	  (setq owind (selected-window))
	  (unwind-protect
	      (progn
		(if wind
		    (select-window wind)
		  (hkey-help-show buf-name current-window)
		  (select-window (get-buffer-window buf-name)))
		(setq buffer-read-only nil) (erase-buffer)
		(insert-file-contents doc-file)
		(goto-char (point-min))
		(set-buffer-modified-p nil))
	    (select-window owind))))))

;; ************************************************************************
;; Private variables
;; ************************************************************************

(defvar action-key-depress-args nil
  "List of mouse event args from most recent depress of the Action Key.")
(defvar assist-key-depress-args nil
  "List of mouse event args from most recent depress of the Assist Key.")

(defvar action-key-release-args nil
  "List of mouse event args from most recent release of the Action Key.")
(defvar assist-key-release-args nil
  "List of mouse event args from most recent release of the Assist Key.")

(defvar action-mouse-key-prev-window nil
  "Window point was in prior to current invocation of 'action/assist-mouse-key'.")

(defvar action-mouse-key-prefix-arg nil
  "Prefix argument to pass to 'smart-br-cmd-select'.")

(defvar action-key-depressed-flag nil "t while Action Key is depressed.")
(defvar assist-key-depressed-flag nil "t while Assist Key is depressed.")
(defvar hkey-help-msg "" "Holds last Smart Key help message.")
(defvar *hkey-wconfig* nil
  "Screen configuration prior to display of a help buffer.")

;;; ************************************************************************
;;; public support functions
;;; ************************************************************************

;; "hsite.el" contains documentation for this variable.
(or (boundp 'smart-scroll-proportional) (setq smart-scroll-proportional nil))

;; The smart keys scroll buffers when pressed at the ends of lines.
;; These next two functions do the scrolling and keep point at the end
;; of line to simplify repeated scrolls when using keyboard smart keys.
;;
;; These functions may also be used to test whether the scroll action would
;; be successful, no action is taken if it would fail (because the beginning
;; or end of a buffer is already showing) and nil is returned.
;; t is returned whenever scrolling is performed.

(defun smart-scroll-down ()
  "Scrolls down according to value of smart-scroll-proportional.
If smart-scroll-proportional is nil or if point is on the bottom window line,
scrolls down (backward) a windowful.  Otherwise, tries to bring current line
to bottom of window.  Leaves point at end of line and returns t if scrolled,
nil if not."
  (interactive)
  (let ((rtn t))
    (if smart-scroll-proportional
	;; If selected line is already last in window, then scroll backward
	;; a windowful, otherwise make it last in window.
	(if (>= (point) (save-excursion
			  (goto-char (1- (window-end)))
			  (beginning-of-line) (point)))
	    (if (pos-visible-in-window-p (point-min))
		(setq rtn nil)
	      (scroll-down))
	  (recenter -1))
      (if (pos-visible-in-window-p (point-min))
	  (setq rtn nil)
	(scroll-down)))
    (end-of-line)
    (or rtn (progn (beep) (message "Beginning of buffer")))
    rtn))

(defun smart-scroll-up ()
  "Scrolls up according to value of smart-scroll-proportional.
If smart-scroll-proportional is nil or if point is on the top window line,
scrolls up (forward) a windowful.  Otherwise, tries to bring current line to
top of window.  Leaves point at end of line and returns t if scrolled, nil if
not."
  (interactive)
  (let ((rtn t))
    (if smart-scroll-proportional
	;; If selected line is already first in window, then scroll forward a
	;; windowful, otherwise make it first in window.
	(if (<= (point) (save-excursion
			  (goto-char (window-start))
			  (end-of-line) (point)))
	    (if (pos-visible-in-window-p (point-max))
		(setq rtn nil)
	      (scroll-up))
	  (recenter 0))
      (if (pos-visible-in-window-p (point-max))
	  (setq rtn nil)
	(scroll-up)))
    (end-of-line)
    (or rtn (progn (beep) (message "End of buffer")))
    rtn))

(provide 'hmouse-drv)
