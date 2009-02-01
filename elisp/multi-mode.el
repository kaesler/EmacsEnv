;;; multi-mode.el --- Switches between two or more modes

;; Author:  Peter Breton
;; Created: Wed May 31 2000
;; Version: $Id: multi-mode.el,v 1.1 2000/06/05 15:56:56 pbreton Exp $
;; Keywords:
;; Time-stamp: <2000-08-08 18:53:03 esler>

;; This file is NOT part of GNU Emacs, but is distributed in the same spirit.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Based on  two-modes.el by David Welton <davidw@efn.org>.

;;; Change log:
;; $Log: multi-mode.el,v $
;; Revision 1.1  2000/06/05 15:56:56  pbreton
;; Minor mode which allows you to run two or more modes in the same buffer
;;
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar multi-mode-alist nil
  "An alist which describes the modes which can be used in a buffer.
The alist should consist of lists of the form:

  START-REGEXP END-REGEXP MAJOR-MODE

Since the regexps will be invoked QUITE often, they should be as tuned and
specific as possible.
")

(defvar multi-mode-default-mode  nil
  "The default mode for a multi-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun define-multi-mode (name default-mode multi-mode-alist &optional description)
  "Create a new multi mode with NAME.
DEFAULT-MODE is the normal mode for the buffer.
MULTI-MODE-ALIST is a list of regexps and matching modes.
DESCRIPTION describes the multi-mode."
  ;; Define a function for it using `defalias' (not `fset') to make
  ;; the mode appear on load-history.
  (defalias name
    `(lambda nil
       ,(or description (concat "Multi mode for type " (symbol-name name)))
       (interactive)
       (multi-mode-internal ',name ',default-mode ',multi-mode-alist))))

(defun multi-mode-setup ()
  "Set up a multi-mode."
  ;; Add our function to post-command-hook
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'multi-mode-update-mode nil t)
  ;; Add ourselves to the minor mode alist
  (make-local-variable 'minor-mode-alist)
  (or (assq 'multi-mode minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(multi-mode " multi-mode")
		  minor-mode-alist))))

(defun multi-mode-change-mode (to-mode)
  "Change the mode of the buffer to TO-MODE."
  (unless (string= to-mode (symbol-name major-mode))
    (progn
      ;; Save the current values of local variables
      (let ((multi-alist multi-mode-alist)
	    (default-mode multi-mode-default-mode)
	    (multi-mode-name mode-name))
	;; Call the new mode
      (funcall to-mode)
      ;; Then set up multi-mode again
      (multi-mode-internal multi-mode-name default-mode multi-alist))
      ;; Fontify
;;       (and (eq font-lock-mode t)
;; 	  (font-lock-fontify-buffer))
      )))

(defun multi-mode-update-mode ()
  "Update a multi-mode."
  (if (or (bobp) (eobp))
      (multi-mode-change-mode multi-mode-default-mode)
    (progn
      (let (start-regexp end-regexp the-mode start end answer)
	(setq answer
	      (catch 'answer
		(mapcar
		 (function
		  (lambda(multi-mode-item)
		    (setq start-regexp (elt multi-mode-item 0))
		    (setq end-regexp   (elt multi-mode-item 1))
		    (setq the-mode     (elt multi-mode-item 2))
		    (save-excursion
		      (and (re-search-backward start-regexp (point-min) t)
			   (setq start (point))))
		    (save-excursion
		      (and (re-search-backward end-regexp (point-min) t)
			   (setq end (point))))
		    (and start (or (not end) (> start end))
			 (throw 'answer the-mode))))
		 multi-mode-alist)
		(throw 'answer multi-mode-default-mode)))
	(and answer
	     (multi-mode-change-mode answer))))))

(defun multi-mode-internal (name default-mode alist)
  (interactive)
  (make-local-variable 'multi-mode-default-mode)
  (setq multi-mode-default-mode default-mode)
  (make-local-variable 'multi-mode-alist)
  (setq multi-mode-alist alist)
  (multi-mode-setup))

(provide 'multi-mode)

;;; multi-mode.el ends here
