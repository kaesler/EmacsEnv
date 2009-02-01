;;!emacs
;;
;; FILE:         br-smt.el
;; SUMMARY:      Support routines for Smalltalk inheritance browsing and error parsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    26-Jul-90
;; LAST-MOD:     21-Sep-95 at 12:31:20 by Bob Weiner
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;   See 'smt-class-def-regexp' for regular expression that matches class
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

(defvar smt-lib-search-dirs nil
  "List of directories below which Smalltalk Library source files are found.
Subdirectories of Library source are also searched.  A Library is a stable
group of classes.")

(defvar smt-sys-search-dirs nil
  "List of directories below which Smalltalk System source files are found.
Subdirectories of System source are also searched.  A System class is one
that is not yet reusable and is likely to change before release.")

(defconst smt-narrow-view-to-class nil
 "*Non-nil means narrow buffer to just the matching class definition when displayed.")

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun smt-get-classes-from-source (filename &rest ignore)
  "Scans FILENAME and returns cons of class list with parents-class alist.
Handles multiple inheritance.  Assumes file existence and readability have
already been checked."
  (let ((no-kill (get-file-buffer filename))
	classes class parents parent-cons)
    (if no-kill
	(set-buffer no-kill)
      (funcall br-view-file-function filename))
    (save-restriction
      (save-excursion
	(widen)
	(goto-char (point-min))
	(while (re-search-forward smt-class-def-regexp nil t)
	  (setq class (buffer-substring (match-beginning 3) (match-end 3))
		parent-cons
		(cons
		 (and (match-end 1) (> (match-end 1) 0)
		      (list (buffer-substring
			     (match-beginning 1)
			     (match-end 1))))
		 class))
	  ;; Assume class name not found within a comment.
	  (setq classes (cons class classes)
		parents (cons parent-cons parents)))))
    (or no-kill (kill-buffer (current-buffer)))
    (cons classes (delq nil parents))))

(defun smt-get-parents-from-source (filename class-name)
  "Scan source in FILENAME and return list of parents of CLASS-NAME.
Assume file existence has already been checked."
    (or (null class-name)
	(car (car (br-rassoc
		   class-name
		   (cdr (smt-get-classes-from-source filename)))))))

(defun smt-select-path (paths-htable-elt &optional feature-p)
  "Select proper pathname from PATHS-HTABLE-ELT based upon value of optional FEATURE-P.
Selection is between path of class definition and path for features associated
with the class."
  (cdr paths-htable-elt))

(defun smt-set-case (type)
  "Return string TYPE identifier for use as a class name."
  type)

(defun smt-set-case-type (class-name)
  "Return string CLASS-NAME for use as a type identifier."
  class-name)

(defun smt-to-class-end ()
  "Assuming point is at start of class, move to best guess start of line after end of class."
  (interactive)
  (goto-char (point-max)))

(defun smt-to-comments-begin ()
  "Skip back from current point past any preceding Smalltalk comments.
Presently a no-op."
  )

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defconst smt-type-tag-separator "@"
  "String that separates a tag's type from its normalized definition form.
This should be a single character which is unchanged when quoted for use as a
literal in a regular expression.")

(defconst smt-subclass-separator
  "\\(variableSubclass:\\|variableWordSubclass:\\|variableByteSubclass:\\|subclass:\\)"
  "Regexp matching delimiter following parent identifier.")

(defconst smt-identifier-chars "a-zA-Z0-9"
  "String of chars and char ranges that may be used within a Smalltalk identifier.")

(defconst smt-identifier (concat "\\([a-zA-Z][" smt-identifier-chars "]*\\)")
  "Regular expression matching a Smalltalk identifier.")


(defconst smt-class-name-before
  (concat "^[ \t]*" smt-identifier
	  "[ \t\n]+" smt-subclass-separator
	  "[ \t\n]*#")
  "Regexp preceding the class name in a class definition.")

(defconst smt-class-name-after
  ""
  "Regexp following the class name in a class definition.")

(defconst smt-class-def-regexp
  (concat smt-class-name-before smt-identifier smt-class-name-after)
  "Regular expression used to match to class definitions in source text.
Class name identifier is grouped expression 3.  'subclass:' inheritance
indicator is grouped expression 2.  Parent identifier is grouped
expression 1.")


(defconst smt-lang-prefix "smt-"
 "Prefix string that starts \"br-smt.el\" symbol names.")

(defconst smt-src-file-regexp ".\\.st$"
  "Regular expression matching a unique part of Smalltalk source file name and no others.")

(defvar smt-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse Smalltalk inheritance graph.  'br-build-children-htable' builds
this list.")
(defvar smt-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse Smalltalk inheritance graph.  'br-build-parents-htable' builds
this list.")
(defvar smt-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
'br-build-paths-htable' builds this list.")


(defvar smt-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.")
(defvar smt-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.")

(defvar smt-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the list.")
(defvar smt-sys-paths-htable nil
  "Alist whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.")

(defvar smt-lib-prev-search-dirs nil
  "Used to check if 'smt-lib-classes-htable' must be regenerated.")
(defvar smt-sys-prev-search-dirs nil
  "Used to check if 'smt-sys-classes-htable' must be regenerated.")

(defvar smt-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require updating.")

(provide 'br-smt)
