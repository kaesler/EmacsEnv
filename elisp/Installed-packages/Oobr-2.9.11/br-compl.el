;;!emacs
;;
;; FILE:         br-compl.el
;; SUMMARY:      Most functions for performing completion on OO constructs.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     matching, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    27-Mar-90
;; LAST-MOD:      4-May-95 at 17:08:48 by Bob Weiner
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

(global-set-key "\M-\C-i" 'br-complete-symbol)

;; ************************************************************************
;; Other required Elisp libraries
;; ************************************************************************

;; Requires a number of functions from "br-lib.el", part of the OO-Browser
;; package.  See the code for functions called but not defined within this
;; file.

;; ************************************************************************
;; Public functions
;; ************************************************************************

(defun br-buffer-menu ()
  "Display list of buffers for current browser language in the viewer window."
  (interactive)
  (or (br-in-view-window-p)
      (setq *br-prev-listing-window* (selected-window)))
  (let ((owind (selected-window))
	(ovbuf (save-window-excursion
		 (br-to-view-window)
		 (current-buffer))))
    (buffer-menu 'files-only)
    (narrow-to-region (point) (point-max))
    (let ((buffer-read-only nil)
	  (buf-name))
      (while (setq buf-name (br-buffer-menu-buffer-name))
	(if (not (string-match br-src-file-regexp buf-name))
	    (delete-region (point) (progn (forward-line 1) (point)))
	  (forward-line 1))))
    (goto-char (point-min))
    (widen)
    (if (looking-at "^$")    ;; No matching buffers
	(progn
	  (switch-to-buffer ovbuf)
	  (select-window owind)
	  (beep)
	  (message
	   "(OO-Browser):  No appropriate buffers available for selection."))
      (set-window-start nil 1)
      (substitute-key-definition 'Buffer-menu-select 'br-buffer-menu-select
				 Buffer-menu-mode-map)
      (message "(OO-Browser):  Select a buffer for display."))))

(defun br-buffer-menu-buffer-name ()
  "Return name of buffer on curren buffer menu line or nil.
Leaves point at the beginning of the current line."
  (if (= (point) (point-max))
      nil
    (beginning-of-line)
    (forward-char Buffer-menu-buffer-column)
    (let ((start (point)))
      ;; End of buffer name marked by tab or two spaces.
      (if (not (re-search-forward "\t\\|  "))
	  nil
	(skip-chars-backward " \t")
	(prog1
	    (buffer-substring start (point))
	  (beginning-of-line))))))

(defun br-buffer-menu-select ()
  "Display buffer associated with the line that point is on."
  (interactive)
  (substitute-key-definition 'br-buffer-menu-select 'Buffer-menu-select 
			     Buffer-menu-mode-map)
  (let ((buff (Buffer-menu-buffer t))
	(menu (current-buffer)))
    (if buff
	(progn (switch-to-buffer buff)
	       (or (eq menu buff)
		   (bury-buffer menu)))
      (beep))))

(defun br-complete-entry (&optional prompt)
  "Interactively completes class or feature name and returns it or nil.
Optional PROMPT is initial prompt string for user."
  (interactive)
  (let ((default (and (br-in-browser)
		      (not (br-in-view-window-p))
		      (br-find-class-name)))
	(completion-ignore-case t)
	completions
	element-name)
    (if (not (br-class-path default)) (setq default nil))
    ;; Prompt with possible completions of element-name.
    (setq prompt (or prompt "Class/Element name:")
	  completions (append (br-class-completions)
			      (br-feature-completions))
	  element-name
	  (if completions
	      (completing-read
		(format "%s (default %s) " prompt (or default "<None>"))
		completions nil 'must-match)
	    (read-string
	      (format "%s (default %s) " prompt (or default "<None>")))))
    (if (equal element-name "") (setq element-name default))
    element-name))

(defun br-complete-symbol ()
  "Complete symbol preceding point."
  (interactive)
  (cond ((and (fboundp 'br-lang-mode)
	      (eq major-mode (symbol-function 'br-lang-mode)))
	 (br-complete-type))
	(t
	 (lisp-complete-symbol))))

(defun br-complete-class-name (&optional must-match prompt)
  "Interactively completes class name if possible, and returns class name.
Optional MUST-MATCH means class name must match a completion table entry.
Optional PROMPT is intial prompt string for user."
  (interactive)
  (let ((default (br-find-class-name))
	(completion-ignore-case t)
	completions
	class-name)
    ;; Prompt with possible completions of class-name.
    (setq prompt (or prompt "Class name:")
	  completions (br-class-completions)
	  class-name
	  (if completions
	      (completing-read
		(format "%s (default %s) " prompt default)
		completions nil must-match)
	    (read-string
	      (format "%s (default %s) " prompt default))))
    (if (equal class-name "") default class-name)))

(defun br-lisp-mode-p ()
  (or (eq major-mode 'lisp-mode)
      (eq major-mode 'emacs-lisp-mode)
      (eq major-mode 'scheme-mode)
      (eq major-mode 'lisp-interaction-mode)))

(defun br-complete-type ()
  "Perform in-buffer completion of a type or element identifier before point.
That symbol is compared against current Environment entries and any needed
characters are inserted."
  (interactive)
  (let* ((completion-ignore-case nil)
	 (end (point))
	 (beg (save-excursion
		(if (br-lisp-mode-p)
		    nil
		  (skip-chars-backward "^()")
		  (if (eq (preceding-char) ?\()
		      (skip-chars-backward " \t\(")
		    (goto-char end))
		  )
		(skip-chars-backward (concat br-identifier-chars ":"))
		(point)))
	 (pattern (br-set-case (buffer-substring beg end)))
	 (type-p)
	 (completion-alist (if (string-match br-feature-signature-regexp
					     pattern)
			       (br-feature-completions)
			     (setq type-p t)
			     (br-class-completions)))
	 (completion (try-completion pattern completion-alist)))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for '%s'" pattern)
	   (ding))
	  ((not (string-equal pattern completion))
	   (delete-region beg end)
	   (insert (if type-p
		       (br-set-case-type completion)
		     completion)))
	  (t
	    (message "Making completion list...")
	    (let ((list (sort (all-completions pattern completion-alist)
			      'string-lessp)))
	      (let (new)
		(while list
		  (setq new (cons (car list) new)
			list (cdr list)))
		(setq list (nreverse new)))
	      (with-output-to-temp-buffer "*Completions*"
		(display-completion-list list)))
	    (message "Making completion list...%s" "done")))))

;; Derived from saveconf.el.
(defun br-window-list ()
  "Returns a list of Lisp window objects for all Emacs windows.
Do not count the minibuffer window even if it is active."
  (let* ((first-window (next-window (previous-window (selected-window))))
	 (windows (cons first-window nil))
	 (current-cons windows)
	 (w (next-window first-window)))
    (while (not (eq w first-window))
      (setq current-cons (setcdr current-cons (cons w nil)))
      (setq w (next-window w)))
    windows))

;; ************************************************************************
;; Private functions
;; ************************************************************************

(defun br-all-classes (&optional htable-type duplicates-flag)
  "Return list of class names in Environment or optional HTABLE-TYPE.
HTABLE-TYPE may be \"sys\" or \"lib\" or an actual hash table.
List is not sorted unless optional DUPLICATES-FLAG is non-nil, which means cons
the the sorted list of duplicate classes onto the front of the unique class
names list."
  (let ((classes
	 (apply 'append
		(hash-map
		 (function (lambda (val-key-cons)
			     ;; Copy so that hash-table values are not
			     ;; disturbed.
			     (copy-sequence (car val-key-cons))))
		 (cond ((and (stringp htable-type)
			     (not (string-equal htable-type "")))
			(br-get-htable (concat htable-type "-paths")))
		       ((hashp htable-type) htable-type)
		       (t (br-get-paths-htable)))))))
    (if duplicates-flag
	(br-duplicate-and-unique-strings (sort classes 'string-lessp))
      classes)))

(defun br-class-completions ()
  "Return alist of elements whose cars are all class names in lookup table."
  (mapcar (function (lambda (elt) (cons elt nil)))
	  (br-class-list-filter (sort (br-all-classes) 'string-lessp))))

(defun br-find-class-name (&optional keep-indent)
  "Return class name that point is within in a listing buffer, else nil.
Optional KEEP-INDENT non-nil means keep indentation preceding class name."
  (if (= (point) (point-max)) (skip-chars-backward " \t\n"))
  (save-excursion
    (if (looking-at (concat "[ \t]*" br-feature-type-regexp "?[ \t]+"))
	(goto-char (match-end 0)))
    (let ((objc (string-equal br-lang-prefix "objc-"))
	  (class))
      (if objc
	  ;; Include [] characters for default classes, <> for Objective-C
	  ;; protocols and () for Objective-C class categories.
	  (skip-chars-backward (concat "\]\[()<>" br-identifier-chars))
	(skip-chars-backward (concat "\]\[" br-identifier-chars)))
      (if (or (and objc
		   (or
		    ;; Objective-C protocol
		    (looking-at (concat "<" br-identifier ">"))
		    ;; Objective-C class(category)
		    (looking-at (concat br-identifier "(" br-identifier ")"))
		    ;; Objective-C class(category)
		    (if (looking-at
			 (concat "\\((" br-identifier ")\\)" br-identifier))
			(setq class (concat (buffer-substring (match-end 1) 
							      (match-end 0))
					    (buffer-substring
					     (match-beginning 1)
					     (match-end 1)))))))
	      (looking-at br-identifier)
	      ;; default class
	      (looking-at (concat "\\[" br-identifier "\\]")))
	  (progn (if keep-indent (beginning-of-line))
		 (br-set-case (or class
				  (buffer-substring (point)
						    (match-end 0)))))))))

(provide 'br-compl)
