;;!emacs
;;
;; FILE:         br-ftr.el
;; SUMMARY:      OO-Browser feature browsing support.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    20-Aug-91 at 18:16:36
;; LAST-MOD:     25-Aug-95 at 16:54:53 by Bob Weiner
;;
;; Copyright (C) 1991-1995, Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst br-feature-type-regexp "[-+=@%>1/]"
  "Regular expression which matches the first non-whitespace characters in an OO-Browser feature listing.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun br-find-feature (&optional feature-entry view-only other-win)
  "Display feature definition for FEATURE-ENTRY in VIEW-ONLY mode if non-nil.
Return feature path if FEATURE-ENTRY is successfully displayed, nil
otherwise.  Can also signal an error when called interactively."
  (interactive)
  (and (interactive-p) (setq view-only current-prefix-arg))
  (let ((feature-path))
    (setq feature-entry
	  (br-feature-signature-and-file
	   (or feature-entry
	       (br-feature-complete 'must-match "Show feature definition:")))
	  feature-path (cdr feature-entry)
	  feature-entry (car feature-entry))
    (br-edit-feature feature-entry feature-path other-win view-only)))

(defun br-edit-feature (tag-entry feature-path &optional other-win view-only)
  "Edit feature for OO-Browser TAG-ENTRY of file FEATURE-PATH, optionally in OTHER-WIN.
With optional VIEW-ONLY, view feature definition instead of editing it.
Return FEATURE-PATH if feature definition is found, else nil."
  (let ((err))
    (cond ((and feature-path (file-readable-p feature-path))
	   (cond ((br-feature-found-p feature-path tag-entry nil other-win)
		  (br-major-mode)
		  (if view-only 
		      (setq buffer-read-only t)
		    ;; Handle case of already existing buffer in
		    ;; read only mode.
		    (and buffer-read-only
			 (file-writable-p feature-path)
			 (setq buffer-read-only nil)))
		  ;; Force mode-line redisplay
		  (set-buffer-modified-p (buffer-modified-p)))
		 ((interactive-p)
		  (setq err
			(format
			 "(OO-Browser):  No '%s' feature defined in Environment."
			 tag-entry)
			feature-path nil))))
	  ((interactive-p)
	   (setq err
		 (format
		  "(OO-Browser):  '%s' - src file not found or not readable, %s"
		  tag-entry feature-path)
		 feature-path nil)))
    (if err (error err))
    feature-path))

(defun br-find-feature-entry ()
  "Return feature entry that point is within or nil."
  (if (= (point) (point-max)) (skip-chars-backward " \t\n"))
  (save-excursion
    (beginning-of-line)
    (if (or 
	 (progn (skip-chars-forward " \t")
		(looking-at br-feature-entry))
	 ;; Get current feature signature, if any.
	 (br-feature-get-signature))
	(let ((feature (buffer-substring
			(point)
			(progn (skip-chars-forward "^\t\n\r") (point)))))
	  (if (and (equal br-lang-prefix "objc-")
		   ;; Remove any trailing class from a category entry.
		   (string-match "@ ([^\)]+)" feature))
	      (substring feature 0 (match-end 0))
	    feature)))))

(defun br-feature-complete (&optional must-match prompt)
  "Interactively completes feature entry if possible, and returns it.
Optional MUST-MATCH means must match a completion table entry.
Optional PROMPT is intial prompt string for user."
  (interactive)
  (let ((default (br-find-feature-entry))
	(completion-ignore-case t)
	completions
	ftr-entry)
    ;; Prompt with possible completions of ftr-entry.
    (setq prompt (or prompt "Feature entry:")
	  completions (br-feature-completions)
	  ftr-entry
	  (if completions
	      (completing-read
		(format "%s (default %s) " prompt default)
		completions nil must-match)
	    (read-string
	      (format "%s (default %s) " prompt default))))
    (if (equal ftr-entry "") default ftr-entry)))

(defun br-feature-completions ()
  "Return completion alist of all current Environment elements."
  (cond ((not (and br-feature-tags-file (file-exists-p br-feature-tags-file)
		   (file-readable-p br-feature-tags-file)))
	 nil)
	((and br-feature-tags-completions
	      (eq
	       (car (cdr br-feature-tags-completions)) ;; tags last mod time
	       (apply '+ (nth 5 (file-attributes br-feature-tags-file))))
	      (equal br-env-file (car br-feature-tags-completions)))
	 (car (cdr (cdr br-feature-tags-completions))))
	(t
	 (let ((ftr-buf (get-buffer-create "*ftr-buf*"))
	       (ftr-alist))
	   (save-excursion
	     (br-feature-tags-init)
	     (copy-to-buffer ftr-buf 1 (point-max))
	     (set-buffer ftr-buf)
	     (goto-char 1)
	     (while (search-forward "\^L" nil t)
	       (forward-line 1)
	       ;; Skip past pathname where features are defined.
	       (while (and (= (forward-line 1) 0)
			   (not (looking-at "\^L\\|\\'")))
		 (setq ftr-alist (cons (cons (br-feature-signature-to-name
					      (br-feature-current)
					      t)
					     nil)
				       ftr-alist)))))
	   (kill-buffer ftr-buf)
	   (setq br-feature-tags-completions 
		 (list br-env-file
		       ;; tags last mod time
		       (apply '+ (nth 5 (file-attributes
					 br-feature-tags-file)))
		       ftr-alist))
	   ftr-alist))))

(defun br-feature-def-file (feature-regexp)
  "Return file name in which feature matching FEATURE-REGEXP is, if any.
Assume feature tags file is current buffer and leave point at the start of
matching feature tag, if any."
  (goto-char 1)
  (and (re-search-forward feature-regexp nil t)
       ;; This ensures that point is left on the same line as the feature tag
       ;; which is found.
       (goto-char (match-beginning 0))
       (br-feature-file-of-tag)))

(defun br-feature-file (feature-sig)
  "Return file name in which feature matching FEATURE-SIG is, if any."
  (let ((obuf (current-buffer))
	(file))
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    (if (search-forward feature-sig nil t)
	(setq file (br-feature-file-of-tag)))
    (set-buffer obuf)
    file))

(defun br-feature-found-p (buf-file feature-tag
			   &optional deferred-class other-win regexp-flag)
  "Search BUF-FILE for FEATURE-TAG.
Return nil if not found, otherwise display it and return non-nil."
  (if buf-file
      (let ((found-def)
	    (opoint (point))
	    (prev-buf)
	    (prev-point)
	    (config (current-window-configuration)))
	(setq prev-buf (get-file-buffer buf-file))
	(funcall br-edit-file-function buf-file other-win)
	(setq prev-point (point))
	(widen)
	(goto-char (point-min))
	(setq found-def 
	      (cond (deferred-class
		      (br-feature-locate-p feature-tag deferred-class))
		    (regexp-flag
		     (br-feature-locate-p feature-tag regexp-flag))
		    (t (br-feature-locate-p feature-tag))))
	(if found-def
	    ;; Set appropriate mode for file.
	    (br-major-mode)
	  (setq buf-file (get-file-buffer buf-file))
	  (if prev-buf
	      (goto-char prev-point)
	    (if buf-file
		(kill-buffer buf-file)
	      (goto-char prev-point)))
	  (set-window-configuration config)
	  (goto-char opoint))
	found-def)))

(defun br-feature-name (ftr-entry)
  "Return name part of FTR-ENTRY."
  (if (equal (string-match br-feature-entry ftr-entry) 0)
      (substring ftr-entry (match-beginning 1))
    ""))

(defun br-feature-signature-and-file (class-and-feature-name)
  "Return (feature signature . feature-def-file-name) of CLASS-AND-FEATURE-NAME."
  (let ((obuf (current-buffer))
	;; Find only exact matches
	(name-regexp (br-feature-name-to-regexp class-and-feature-name))
	(result))
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    (if (re-search-forward name-regexp nil t)
	(progn (goto-char (match-beginning 0))
	       (setq result (cons (br-feature-current)
				  (br-feature-file-of-tag)))))
    (set-buffer obuf)
    result))

(defun br-feature-signature (&optional arg)
  "Show full feature signature in the view window.
With optional prefix ARG, display signatures of all features from the current
buffer."
  (interactive "P")
  (let* ((buf (buffer-name))
	 (owind (selected-window))
	 (features (delq nil (if arg (br-feature-get-tags)
			       (list (br-feature-get-signature))))))
    (if (null features)
	(progn (beep) (message "No elements."))
      (br-to-view-window)
      (switch-to-buffer (get-buffer-create (concat buf "-Elements")))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapcar (function (lambda (feature) (insert feature "\n")))
	      features)
      (br-major-mode)
      (goto-char 1)
      (select-window owind)
      (message ""))))

;;; ************************************************************************
;;; Listing buffer entry tag property handling.
;;; ************************************************************************

(if (string-match "^19\." emacs-version)
    (progn
      ;;
      ;; Emacs 19 buffer entry tags functions
      ;;

      (defun br-feature-clear-signatures (&optional buf-nm)
	"Erase any feature signatures saved with current buffer or optional BUF-NM."
	(save-excursion
	  (if buf-nm (set-buffer (get-buffer buf-nm)))
	  (save-restriction
	    (widen)
	    (remove-text-properties (point-min) (point-max) '(tag)))))

      (defun br-feature-get-signature (&optional line-num-minus-one)
	(save-excursion
	  (if (numberp line-num-minus-one)
	      (goto-line (1+ line-num-minus-one)))
	  (end-of-line)
	  (car (cdr (memq 'tag (text-properties-at (1- (point))))))))

      (defun br-feature-get-tags ()
	(save-excursion
	  (goto-char (point-max))
	  (let ((found t)
		(tags)
		tag)
	    (while found
	      (setq tag (get-text-property (1- (point)) 'tag))
	      (if tag (setq tags (cons tag tags)))
	      (setq found (= (forward-line -1) 0))
	      (end-of-line))
	    tags)))

      ;; Tag property is placed at end of line in case leading indent is
      ;; removed by an OO-Browser operation.  In that case, we don't want to
      ;; lose the tag property.
      (defun br-feature-put-signatures (ftr-sigs)
	(while ftr-sigs
	  (end-of-line)
	  (put-text-property (- (point) 2) (point) 'tag (car ftr-sigs))
	  (setq ftr-sigs (cdr ftr-sigs))
	  (if (and ftr-sigs (/= (forward-line 1) 0))
	      (error "(br-feature-put-signatures): Too few lines in this buffer"))))

      )

  ;;
  ;; Emacs 18 buffer entry tags functions
  ;;

  (defun br-feature-clear-signatures (&optional buf-nm)
    "Erase any feature signatures saved with current buffer or optional BUF-NM."
    (put (intern (or buf-nm (buffer-name))) 'features nil))

  (defun br-feature-get-signature (&optional line-num)
    (or (numberp line-num)
	(save-excursion
	  (beginning-of-line)
	  (setq line-num (count-lines 1 (point)))))
    (cdr (assq line-num (get (intern-soft (buffer-name)) 'features))))

  (defun br-feature-get-tags ()
    (get (intern-soft (buffer-name)) 'features))

  (defun br-feature-put-signatures (ftr-sigs)
    (beginning-of-line)
    (let* ((line (count-lines 1 (point)))
	   (meth-alist (mapcar (function
				(lambda (meth)
				  (prog1 (cons line meth)
				    (setq line (1+ line)))))
			       ftr-sigs))
	   (buf-sym (intern (buffer-name))))
      (put buf-sym 'features
	   (nconc (get buf-sym 'features) meth-alist))))
  )

;;; ************************************************************************
;;; END - Listing buffer entry tag property handling.
;;; ************************************************************************

(defun br-feature-tags-init ()
  "Set up 'br-feature-tags-file' for writing."
  (setq br-feature-tags-completions nil
	br-feature-tags-file (br-feature-tags-file-name br-env-file)
	br-tags-file (concat br-env-file "-TAGS"))
  (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
  (setq buffer-read-only nil))

(defun br-feature-tags-file-name (env-file)
  (concat env-file "-FTR"))

(defun br-feature-tags-save ()
  "Filter out extraneous lines and save 'br-feature-tags-file'."
  (let ((obuf (current-buffer)))
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char (point-min))
    (delete-matching-lines "^[ \t]*$")
    (goto-char (point-min))
    (replace-regexp "^[ \t]+\\|[ \t]+$" "")
    (and br-c-tags-flag
	 (br-member br-lang-prefix '("c++-" "objc-"))
	 (progn (c-build-element-tags)
		(goto-char (point-min))
		(replace-regexp "[ \t]*//.*" "")))
    (goto-char (point-min))
    (delete-matching-lines "^$")
    (save-buffer)
    (set-buffer obuf)))

(defun br-insert-features (feature-tag-list &optional indent)
  "Insert feature names from FEATURE-TAG-LIST in current buffer indented INDENT columns."
  (let ((start (point)))
    (mapcar (function
	     (lambda (feature-tag)
	       (if indent (indent-to indent))
	       (if feature-tag
		   (insert (br-feature-signature-to-name feature-tag nil t)
			   "\n"))))
	    feature-tag-list)
    (save-excursion
      (goto-char start)
      (br-feature-put-signatures feature-tag-list))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun br-feature-current ()
  "Extract current feature from tags file and leave point at the end of line."
  (beginning-of-line)
  (buffer-substring (point) (progn (end-of-line) (point))))

(defun br-feature-file-of-tag ()
  "Return the file name of the file whose tag point is within.
Assumes the tag table is the current buffer."
  (save-excursion
    (search-backward "" nil t)
    (forward-line 1)
    (let ((start (point)))
      (end-of-line)
      (buffer-substring start (point)))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst br-feature-entry
  (concat br-feature-type-regexp " \\([^\t\n\r]*[^ \t\n\r]\\)")
  "Regexp matching a feature entry in a browser listing buffer.")

(defvar br-feature-tags-completions nil
  "List of (envir-name tags-file-last-mod-time tags-completion-alist).")

(defvar br-feature-tags-file nil
  "Pathname where current object-oriented feature tags are stored.")

(defvar br-tags-file nil
  "Pathname where current non-object-oriented feature tags are stored.")

(provide 'br-ftr)
