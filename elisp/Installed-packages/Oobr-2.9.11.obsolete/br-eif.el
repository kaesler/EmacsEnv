;;!emacs
;;
;; FILE:         br-eif.el
;; SUMMARY:      Support routines for Eiffel inheritance browsing and error parsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:     7-Dec-89
;; LAST-MOD:     21-Sep-95 at 14:13:50 by Bob Weiner
;;
;; Copyright (C) 1989-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-lib)

;;; ************************************************************************
;;; User visible variables
;;; ************************************************************************

(defvar eif-lib-search-dirs nil
  "List of directories below which Eiffel Library source files are found.
Subdirectories of Library source are also searched.  A Library is a stable
group of classes.")

(defvar eif-sys-search-dirs nil
  "List of directories below which Eiffel System source files are found.
Subdirectories of System source are also searched.  A System class is one
that is not yet reusable and is likely to change before release.")

(defconst eif-narrow-view-to-class nil
 "*Non-nil means narrow buffer to just the matching class definition when displayed.")

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun eif-get-classes-from-source (filename &optional skip-tags
				    skip-tags-cleanup)
  "Scans FILENAME and returns cons of class list with parents-class alist.
Handles multiple inheritance.  Assumes file existence and readability have
already been checked.
   With optional SKIP-TAGS non-nil, does not compute and store lookup tags
for element definitions.  If SKIP-TAGS is nil, normally a cleanup
function is called after scanning the elements.  SKIP-TAGS-CLEANUP
non-nil suppresses this action."
  ;; Multiple classes per file allowed
  (let ((no-kill (get-file-buffer filename))
	classes class end parents signatures start)
    (if no-kill
	(set-buffer no-kill)
      (funcall br-view-file-function filename))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward eif-class-def-regexp nil t)
	  (setq start (point)
		class
		(eif-set-case
		 (buffer-substring (match-beginning 2) (match-end 2)))
		classes (cons class classes)
		parents (cons (cons (eif-get-parents-from-source
				     filename class)
				    class)
			      parents))
	  (eif-to-class-end)
	  (setq end (point))
	  (or skip-tags
	      ;; Scan class features
	      (setq signatures
		    (eif-scan-features-in-class class start end))))))
    (if skip-tags
	nil
      (eif-get-feature-tags
       buffer-file-name (eif-sort-features signatures))
      (or skip-tags-cleanup (br-feature-tags-save)))
    (or no-kill (kill-buffer (current-buffer)))
    (cons classes (delq nil parents))))

(defun eif-get-parents-from-source (filename &optional class-name)
  "Return list of downcased parents of Eiffel class in FILENAME.
Return nil if file is not readable.  Assume that Eiffel keywords
'inherit' and 'feature' occur at the beginning of lines, when present."
  (let ((parents) (par)
	(no-kill (get-file-buffer filename))
	(obuf (current-buffer)))
    (if no-kill
	(set-buffer no-kill))
    (if (or no-kill
	    (if (file-readable-p filename)
		 (let ((br-view-file-function 'br-insert-file-contents))
		   (message "Scanning %s ..." filename)
		   (funcall br-view-file-function filename)
		   t)))
	(progn
	  (save-restriction
	    (save-excursion
	      (widen)
	      (goto-char (point-min))
	      (let ((case-fold-search t) ;; Ignore case in searches
		    (end)
		    indent)
		(if (or (null class-name)
			(let ((class-def (concat eif-class-name-before 
						 (eif-set-case-type class-name)
						 eif-class-name-after)))
			  (re-search-forward class-def nil t)))
		    (if (not (re-search-forward (concat "^inherit[ \t\n]+"
							eif-parent-regexp)
						nil t))
			nil
		      ;; Save first parent
		      (setq parents (list
				     (eif-set-case (buffer-substring
						    (match-beginning 2)
						    (match-end 2))))
			    indent (save-excursion
				     (goto-char (match-beginning 2))
				     (current-column)))
		      ;; Save any succeeding parents
		      (save-excursion
			(if (re-search-forward "^[a-zA-Z]" nil t)
			    (setq end (1- (point)))))
		      (forward-line 1)
 		      (while (< (point) end)
			(back-to-indentation)
			(and (<= (current-column) indent)
			     (looking-at eif-identifier)
			     (setq par (eif-set-case (buffer-substring
						      (match-beginning 1)
						      (match-end 1))))
			     (if (or (br-member par parents)
				     (br-member-sorted-strings
				      par eif-reserved-words))
				 nil
			       (setq parents (cons par parents))))
			(forward-line 1)))))))
	  (or no-kill (kill-buffer (current-buffer)))
	  (set-buffer obuf)
	  (nreverse parents)))))

(defun eif-select-path (paths-htable-elt &optional feature-p)
  "Select proper pathname from PATHS-HTABLE-ELT based upon value of optional FEATURE-P.
Selection is between path of class definition and path for features associated
with the class."
  (cdr paths-htable-elt))

(defun eif-set-case (type)
  "Return string TYPE identifier for use as a class name."
  (downcase type))

(defun eif-set-case-type (class-name)
  "Return string CLASS-NAME for use as a type identifier."
  (upcase class-name))


(defun eif-to-class-end ()
  "Assuming point is at start of class, move to start of line after end of class."
  (interactive)
  (if (and (re-search-forward "^end[ \t\n-]" nil t)
	   (= (forward-line 1) 0))
      nil
    (goto-char (point-max))))

(defun eif-to-comments-begin ()
  "Skip back from current point past any preceding blank lines and comments."
  (let ((opoint))
    (while
	(progn (setq opoint (point))
	       ;; To previous line
	       (and (= 0 (forward-line -1))
		    ;; If begins with "--", then is a comment.
		    (cond ((looking-at "[ \t]*--"))
			  ((looking-at "[ \t]*$"))))))
    (goto-char opoint)
    ;; Skip past whitespace
    (skip-chars-forward " \t\n")
    (beginning-of-line)))

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defconst eif-class-name-before "^[ \t]*\\(deferred[ \t\n]+\\|expanded[ \t\n]+\\)?class[ \t\n]+"
  "Regexp preceding the class name in a class definition.")

(defconst eif-class-name-after "[ \t\n]+"
  "Regexp following the class name in a class definition.")

(defconst eif-identifier-chars "A-Za-z0-9_"
  "String of chars and char ranges that may be used within an Eiffel identifier.")

(defconst eif-identifier (concat "\\([a-zA-Z][" eif-identifier-chars "]*\\)")
  "Regular expression matching an Eiffel identifier.")

(defconst eif-class-def-regexp
  (concat eif-class-name-before eif-identifier eif-class-name-after)
  "Regular expression used to match to class definitions in source text.
Class name identifier is grouped expression 2.")

(defconst eif-class-name-preceding
  "\\([\[\{>;:][ \t\n]*\\|[a-zA-z][ \t\n]+\\)"
  "Pattern preceding any valid non-comment use of an Eiffel class/type name.")

(defconst eif-class-name-pat
  (concat eif-class-name-preceding eif-identifier)
  "Class name is grouped expression 2.")

(defconst eif-lang-prefix "eif-"
  "Prefix string that starts \"br-eif.el\" symbol names.")


(defconst eif-parent-regexp (concat "[ \t\n]*\\(--.*[\n]\\)*[ \t\n]*"
				    eif-identifier)
  "Parent identifier is grouped expression 2.")

(defconst eif-src-file-regexp ".\\.e$"
  "Regular expression matching a unique part of Eiffel class filenames and no others.")

(defvar eif-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse Eiffel inheritance graph.  'br-build-children-htable' builds
this list.")
(defvar eif-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse Eiffel inheritance graph.  'br-build-parents-htable' builds
this list.")
(defvar eif-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . DIRECTORY).
DIRECTORY gives the location of classes found in LIST-OF-CLASS-NAMES.
'br-build-paths-htable' builds this list.")

(defvar eif-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.")
(defvar eif-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . DIRECTORY).
DIRECTORY gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.")

(defvar eif-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the list.")
(defvar eif-sys-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . DIRECTORY).
DIRECTORY gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.")

(defvar eif-lib-prev-search-dirs nil
  "Used to check if 'eif-lib-paths-htable' must be regenerated.")
(defvar eif-sys-prev-search-dirs nil
  "Used to check if 'eif-sys-paths-htable' must be regenerated.")

(defvar eif-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require updating.")

(provide 'br-eif)
