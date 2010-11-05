;;!emacs
;;
;; FILE:         br-info.el
;; SUMMARY:      Support routines for Info file hierarchy browsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     docs, help, hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:     7-Dec-89
;; LAST-MOD:     21-Sep-95 at 12:29:58 by Bob Weiner
;;
;; Copyright (C) 1989-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;   See 'info-class-def-regexp' for regular expression that matches class
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

(defvar info-lib-search-dirs nil
  "List of directories below which Info Library source files are found.
Subdirectories of Library source are also searched.  A Library is a stable
group of classes.")

(defvar info-sys-search-dirs nil
  "List of directories below which Info System source files are found.
Subdirectories of System source are also searched.  A System class is one
that is not yet reusable and is likely to change before release.")

(defconst info-narrow-view-to-class t
 "*Non-nil means narrow buffer to just the matching class definition when displayed.")

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun info-find-nd (filename node edit)
  "Show (FILENAME)NODE in current window.
If EDIT is non-nil, NODE is made editable."
  (if (string-match "-[1-9][0-9]*$" filename)
      (setq filename (substring filename 0 (match-beginning 0))) )
  (Info-find-node filename node t)
  (if edit (let ((Info-enable-edit t))
		 (Info-edit))))

(defun info-get-classes-from-source (filename &rest ignore)
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
	(while (re-search-forward info-class-def-regexp nil t)
	  (setq class (buffer-substring (match-beginning 1) (match-end 1))
		parent-cons
		(cons 
		  (if (looking-at info-parent-regexp)
		      (list (buffer-substring
			     (match-beginning 1)
			     (match-end 1))))
		  class)
		classes (cons class classes)
		parents (cons parent-cons parents)))))
    (or no-kill (kill-buffer (current-buffer)))
    (cons classes (delq nil parents))))

(defun info-get-parents-from-source (filename class-name)
  "Scan source in FILENAME and return list of parents of CLASS-NAME.
Assume file existence has already been checked."
    (or (null class-name)
	(car (car (br-rassoc
		   class-name
		   (cdr (info-get-classes-from-source filename)))))))

(defun info-select-path (paths-htable-elt &optional feature-p)
  "Select proper pathname from PATHS-HTABLE-ELT based upon value of optional FEATURE-P.
Selection is between path of class definition and path for features associated
with the class."
  (cdr paths-htable-elt))

(defun info-set-case (type)
  "Return string TYPE identifier for use as a class name."
  type)

(defun info-set-case-type (class-name)
  "Return string CLASS-NAME for use as a type identifier."
  class-name)

(defun info-to-class-end ()
  "Assuming point is at start of node, move to start of line after end of node."
  (interactive)
  (skip-chars-forward " \t\n")
  (if (re-search-forward "[]" nil t)
      (beginning-of-line)
    (goto-char (point-max))))

(defun info-to-comments-begin ()
  "Skip back from current point past any preceding Info comments."
  (skip-chars-forward " \t\n"))

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defconst info-class-name-before
  "?[\n][\n\t ]*.*Node:[ \t]+"
  "Regexp preceding the class name in a class definition.")

(defconst info-identifier-chars "-_()a-zA-Z0-9 "
  "String of chars and char ranges that may be used within an Info identifier.")

(defconst info-identifier (concat "\\([a-zA-Z0-9()][" info-identifier-chars "]*\\)")
  "Regular expression matching an Info identifier.")

(defconst info-class-name-after
  "[\t,\n]+"
  "Regexp following the class name in a class definition.")

(defconst info-class-def-regexp
  (concat info-class-name-before info-identifier info-class-name-after)
  "Regular expression used to match to class definitions in source text.
Class name identifier is grouped expression 1.")

(defconst info-parent-regexp
  (concat ".*Up:[ \t]+" info-identifier)
  "Regular expression whose grouping number 1 matches Info parent identifier.")

(defconst info-lang-prefix "info-"
 "Prefix string that starts \"br-info.el\" symbol names.")

(defconst info-src-file-regexp ".$"
  "Regular expression matching a unique part of Info source.")

(defvar info-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse Info inheritance graph.  'br-build-children-htable' builds
this list.")
(defvar info-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse Info inheritance graph.  'br-build-parents-htable' builds
this list.")
(defvar info-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
'br-build-paths-htable' builds this list.")


(defvar info-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.")
(defvar info-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.")

(defvar info-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the list.")
(defvar info-sys-paths-htable nil
  "Alist whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.")

(defvar info-lib-prev-search-dirs nil
  "Used to check if 'info-lib-classes-htable' must be regenerated.")
(defvar info-sys-prev-search-dirs nil
  "Used to check if 'info-sys-classes-htable' must be regenerated.")

(defvar info-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require updating.")

(provide 'br-info)
