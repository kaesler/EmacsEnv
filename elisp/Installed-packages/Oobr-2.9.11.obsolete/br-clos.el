;;!emacs
;;
;; FILE:         br-clos.el
;; SUMMARY:      Support routines for CLOS inheritance browsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     lisp, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    29-Jul-90
;; LAST-MOD:     21-Sep-95 at 12:29:40 by Bob Weiner
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;   Properly supports CLOS multiple inheritance.
;;
;;   See 'clos-class-def-regexp' for regular expression that matches class
;;   definitions.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-lib)

;;; ************************************************************************
;;; User visible variables
;;; ************************************************************************

(defvar clos-lib-search-dirs nil
  "List of directories below which CLOS Library source files are found.
Subdirectories of Library source are also searched.  A Library is a stable
group of classes.")

(defvar clos-sys-search-dirs nil
  "List of directories below which CLOS System source files are found.
Subdirectories of System source are also searched.  A System class is one
that is not yet reusable and is likely to change before release.")

(defconst clos-narrow-view-to-class nil
 "*Non-nil means narrow buffer to just the matching class definition when displayed.")

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun clos-get-classes-from-source (filename &optional skip-tags
				     skip-tags-cleanup)
  "Scans FILENAME and returns cons of class list with parents-class alist.
Handles multiple inheritance.  Assumes file existence and readability have
already been checked.
   With optional SKIP-TAGS non-nil, does not compute and store lookup tags
for element definitions.  If SKIP-TAGS is nil, normally a cleanup
function is called after scanning the elements.  SKIP-TAGS-CLEANUP
non-nil suppresses this action."
  (let ((no-kill (get-file-buffer filename))
	classes class parents parent-cons parent-list signatures)
    (if no-kill
	(set-buffer no-kill)
      (funcall br-view-file-function filename))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(if skip-tags
	    nil
	  (setq signatures (clos-scan-features))
	  (goto-char (point-min)))
	(while (re-search-forward clos-class-def-regexp nil t)
	  (setq class (buffer-substring (match-beginning 1) (match-end 1))
		parent-list nil)
	  (while (looking-at clos-parent-regexp)
	    (setq parent-list
		  (cons (buffer-substring
			 (match-beginning 1)
			 (match-end 1))
			parent-list))
	    (goto-char (match-end 0)))
	  (setq parent-list (nreverse parent-list))
	  (if (and (null parent-list)
		   (not (equal class "t")))
	      ;; All classes have t as an ancestor, so if
	      ;; no parents are listed, make t the sole parent.
	      (setq parent-list '("t")))
	  (setq parent-cons (cons parent-list class))
	  ;; Don't have to check whether class-def pattern begins
	  ;; after a comment since the regexp used for matching
	  ;; precludes this.
	  (setq classes (cons class classes)
		parents (cons parent-cons parents)))))
    (if skip-tags
	nil
      (clos-get-feature-tags
       buffer-file-name (clos-sort-features signatures))
      (or skip-tags-cleanup (br-feature-tags-save)))
    (or no-kill (kill-buffer (current-buffer)))
    (cons classes (delq nil parents))))

(defun clos-get-parents-from-source (filename class-name)
  "Scan source in FILENAME and return list of parents of CLASS-NAME.
Assume file existence has already been checked."
    (cond ((null class-name) nil)
	  ((equal filename br-null-path)
	   ;; This means there is no source for this class, so 
	   ;; since all classes have t as an ancestor and there is no where
	   ;; to look for parents, make t the sole parent.
	   '("t"))
	  (t (car (car (br-rassoc
			class-name
			(cdr (clos-get-classes-from-source filename t))))))))

(defun clos-select-path (paths-htable-elt &optional feature-p)
  "Select proper pathname from PATHS-HTABLE-ELT based upon value of optional FEATURE-P.
Selection is between path of class definition and path for features associated
with the class."
  (let ((elt (cdr paths-htable-elt)))
    (if (consp elt) 
	(if feature-p (cdr elt) (car elt))
      ;; Both paths are the same.
      elt)))

(defun clos-set-case (type)
  "Return string TYPE identifier for use as a class name."
  type)

(defun clos-set-case-type (class-name)
  "Return string CLASS-NAME for use as a type identifier."
  class-name)

(defun clos-to-class-end ()
  "Assuming point is at start of class, move to start of line after end of class."
  (interactive)
  (goto-char (point-max))
  )

(defun clos-to-comments-begin ()
  "Skip back from current point past any preceding CLOS comments."
  (let ((opoint))
    (while
	(progn (setq opoint (point))
	       ;; To previous line
	       (if (= 0 (forward-line -1))
		   (cond
		     ;; If begins with ";", then is a comment.
		     ((looking-at "[ \t]*\\(;\\|$\\)"))
		     (nil)))))
    (goto-char opoint)
    ;; Skip past whitespace
    (skip-chars-forward " \t\n")
    (beginning-of-line)))

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defconst clos-class-keyword
  "(defclass[ \t]+"
  "Keyword regexp preceding a clos class definition.")

(defconst clos-class-name-before
  (concat "^[ \t]*" clos-class-keyword)
  "Regexp preceding the class name in a class definition.")

(defconst clos-class-name-after
  "[ \t\n]*\("
  "Regexp following the class name in a class definition.")


(defconst clos-identifier-chars      "a-zA-Z0-9+*/_~!@$%^&=:<>{}|.-"
  "String of chars and char ranges that may be used within a CLOS identifier.")

(defconst clos-type-identifier-chars "][a-zA-Z0-9+*/_~!@$%^&=<>{}|.-"
  "String of chars and char ranges that may be used within a CLOS class name.
No colons allowed.")

(defconst clos-identifier (concat "\\([" clos-identifier-chars "]+\\)")
  "Regular expression matching a CLOS identifier.")

(defconst clos-class-def-regexp
  (concat clos-class-name-before clos-identifier clos-class-name-after)
  "Regular expression used to match to class definitions in source text.
Class name identifier is grouped expression 1.  Parent class names
follow this expression, which terminates with the parenthesis that begins
the parent class group.")

(defconst clos-lang-prefix "clos-"
 "Prefix string that starts \"br-clos.el\" symbol names.")

(defconst clos-parent-regexp
  (concat "[ \t\n]*" clos-identifier)
  "Parent identifier is grouped expression 1.")

(defconst clos-src-file-regexp ".\\.\\(lisp\\|lsp\\|cl\\|el\\)$"
  "Regular expression matching a unique part of CLOS source file names and no others.")

(defvar clos-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse CLOS inheritance graph.  'br-build-children-htable' builds
this list.")
(defvar clos-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse CLOS inheritance graph.  'br-build-parents-htable' builds
this list.")
(defvar clos-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
'br-build-paths-htable' builds this list.")


(defvar clos-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.")
(defvar clos-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.")

(defvar clos-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the list.")
(defvar clos-sys-paths-htable nil
  "Alist whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.")

(defvar clos-lib-prev-search-dirs nil
  "Used to check if 'clos-lib-classes-htable' must be regenerated.")
(defvar clos-sys-prev-search-dirs nil
  "Used to check if 'clos-sys-classes-htable' must be regenerated.")

(defvar clos-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require updating.")

(provide 'br-clos)
