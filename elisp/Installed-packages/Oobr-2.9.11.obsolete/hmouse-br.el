;;!emacs
;;
;; FILE:         hmouse-br.el
;; SUMMARY:      Hyperbole Key control for the OO-Browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     mouse, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    Sep-04-90
;; LAST-MOD:      1-Nov-95 at 20:32:56 by Bob Weiner
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br)

;;; ************************************************************************
;;; smart-br functions
;;; ************************************************************************

;;; Unused unless the "br.el" library, part of the OO-Browser package, has
;;; been loaded.

(defun smart-br ()
  "Controls OO-Browser listing buffers with one key or mouse key.

Invoked via a key press when in an OO-Browser listing window.  It assumes
that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) in a blank buffer or at the end of a buffer, browser help
     information is displayed in the viewer window;
 (2) at the beginning of a (non-single character) class name, the class'
     ancestors are listed;
 (3) at the end of an entry line, the listing is scrolled up;
 (4) on the `...', following a class name, point is moved to the class
     descendency expansion;
 (5) before an element name, the implementor classes of the name are listed;
 (6) anywhere else on an entry line, the entry's source is displayed for
     editing."

  (interactive)
  (br-browse)
  (cond ((eobp)
	 (br-help)
	 (and action-mouse-key-prev-window
	      (select-window action-mouse-key-prev-window)))
	((eolp) (smart-scroll-up))
	((br-find-feature-entry)
	 (if (bolp) (br-implementors) (br-feature)))
	((and (bolp)
	      (let ((cl (br-find-class-name)))
		(and cl (not (= (length cl) 1)))))
	 (br-ancestors))
	((br-to-tree))
	((br-edit))))

(defun smart-br-assist ()
  "Controls OO-Browser listing buffers with one assist-key or mouse assist-key.

Invoked via an assist-key press when in an OO-Browser listing window.  It
assumes that its caller has already checked that the assist-key was pressed in
an appropriate buffer and has moved the cursor to the selected buffer.

If assist-key is pressed:
 (1) in a blank buffer, a selection list of buffer files is displayed;
 (2) at the beginning of a (non-single character) class, the class'
     descendants are listed;
 (3) at the end of an entry line, the listing is scrolled down;
 (4) on the `...', following a class name, point is moved to the class
     expansion;
 (5) anywhere else on a class line, the class' elements are listed;
 (6) anywhere else on an element line, the element's implementor
     classes are listed;
 (7) on a blank line following all entries, the current listing buffer
     is exited."
  
  (interactive)
  (br-browse)
  (cond ((equal 0 (string-match br-buffer-prefix-blank (buffer-name)))
	 (br-buffer-menu))
	((eobp) (br-exit-level 1))
	((eolp) (smart-scroll-down))
	((br-find-feature-entry) (br-implementors))
	((and (bolp)
	      (let ((cl (br-find-class-name)))
		(and cl (not (= (length cl) 1)))))
	 (br-descendants))
	((br-to-tree))
	(t (br-features 1))))


(defun smart-br-dispatch ()
  (if (or (br-listing-window-p) (eq major-mode 'br-mode))
      ;; In an OO-Browser listing window.
      (smart-br)
    (cond ((eq major-mode 'Info-mode)
	   (smart-info))
	  ((eq major-mode 'Buffer-menu-mode)
	   (smart-buffer-menu t))
	  ((eolp) (smart-scroll-up))
	  ((and (boundp 'br-src-file-regexp)
		buffer-file-name
		(fboundp (symbol-function 'br-to-definition))
		(string-match br-src-file-regexp buffer-file-name))
	   (br-to-definition))
	  ((and action-mouse-key-prev-window
		(or (smart-br-cmd-select nil)
		    (error "(Action Key): No command bound to key."))))
	  (t (scroll-up)))))

(defun smart-br-assist-dispatch ()
  (if (or (br-listing-window-p) (eq major-mode 'br-mode))
      ;; In an OO-Browser listing window.
      (smart-br-assist)
    (cond ((eq major-mode 'Info-mode)
	   (smart-info-assist))
	  ((eq major-mode 'Buffer-menu-mode)
	   (smart-buffer-menu-assist))
	  ((eolp) (smart-scroll-down))
	  ((and action-mouse-key-prev-window
		(or (smart-br-cmd-select 'assist)
		    (error "(Assist Key): No command bound to key."))))
	  (t (scroll-down)))))

(defun smart-br-cmd-select (&optional assist-flag)
  "Selects an OO-Browser command with its key binding at point.
By default executes the command, with optional ASSIST-FLAG non-nil, shows help for
command.  Returns t if a command is selected.  Nil indicates no key binding was
found on the current line.  Key bindings are delimited by {}."
  (let ((start) (end) (tmp-buf) (tmp-buf-nm) (obuf (current-buffer)))
    (and (save-excursion
	   (or (eobp) (forward-char))
	   (save-excursion
	     (beginning-of-line)
	     (setq start (point)))
	   (and (re-search-backward "\\(^\\|[^\\]\\){" start t)
		(progn 
		  (goto-char (match-end 0))
		  (setq start (point))
		  (save-excursion
		    (end-of-line)
		    (setq end (point)))
		  (and (re-search-forward "[^\\]}" end t)
		       (setq end (1- (point)))))))
	 (progn
	   (setq tmp-buf-nm "*smart-br-tmp*"
		 tmp-buf (progn (if (get-buffer tmp-buf-nm)
				    (kill-buffer tmp-buf-nm))
				(get-buffer-create tmp-buf-nm)))
	   (or tmp-buf
	       (error
		"(Action Key): (smart-br-cmd-select) - Can't create tmp-buf."))
	   (copy-to-buffer tmp-buf start end)
	   (set-buffer tmp-buf)
	   (let ((case-fold-search nil) (case-replace t)
		 (keys)
		 (pref-arg action-mouse-key-prefix-arg))
	     ;; Quote Control and Meta key names
	     (goto-char (point-min))
	     (replace-regexp "[ \t]+" "")
	     (goto-char (point-min))
	     (replace-string "SPC" "\040")
	     (goto-char (point-min))
	     (replace-string "DEL" "\177")
	     (goto-char (point-min))
	     (replace-regexp "ESC" "M-")
	     (goto-char (point-min))
	     ;; Unqote special {} chars.
	     (replace-regexp "\\\\\\([{}]\\)" "\\1")
	     (goto-char (point-min))
	     (if (looking-at "C-u")
		 (progn (delete-char 3)
			(and (or (null pref-arg)
				 (equal pref-arg 1))
			     (setq pref-arg '(4)))))
	     (while (search-forward "C-" nil t)
	       (replace-match "")
	       (setq keys (1+ (- (downcase (following-char)) ?a)))
	       (delete-char 1)
	       (insert keys))
	     (goto-char (point-min))
	     (while (search-forward "M-" nil t)
	       (replace-match "")
	       (setq keys (+ 128 (downcase (following-char))))
	       (delete-char 1)
	       (insert keys))
	     (setq keys (buffer-string))
	     (kill-buffer tmp-buf-nm)
	     (set-buffer obuf)
	     (and (boundp 'action-mouse-key-prev-window)
		  action-mouse-key-prev-window
		  (select-window action-mouse-key-prev-window))
	     (let ((current-prefix-arg pref-arg)
		   (binding (key-binding keys)))
	       (if binding
		   (progn
		     (if assist-flag
			 (br-cmd-help keys)
		       (call-interactively binding))
		     t))))))))

;;; ************************************************************************
;;; Hyperbole info browsing functions
;;; ************************************************************************

(autoload 'Info-handle-in-note "hmous-info"
          "Follows Info documentation references.")
(autoload 'smart-info "hmous-info" "Follows Info documentation references." t)
(autoload 'smart-info-assist "hmous-info"
          "Follows Info documentation references." t)

(provide 'hmouse-br)
