;;!emacs
;;
;; FILE:         br-objc.el
;; SUMMARY:      Support routines for Objective-C inheritance browsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:     7-Dec-89
;; LAST-MOD:     21-Sep-95 at 12:31:05 by Bob Weiner
;;
;; Copyright (C) 1989-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;   See 'objc-class-def-regexp' for regular expression that matches class
;;   definitions.
;;            
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-lib br-c-ft))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar objc-lib-search-dirs nil
  "List of directories below which Objective-C Library source files are found.
Subdirectories of Library source are also searched.  A Library is a stable
group of classes.")

(defvar objc-sys-search-dirs nil
  "List of directories below which Objective-C System source files are found.
Subdirectories of System source are also searched.  A System class is one
that is not yet reusable and is likely to change before release.")

(defconst objc-narrow-view-to-class nil
 "*Non-nil means narrow buffer to just the matching class definition when displayed.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun objc-get-classes-from-source (filename &optional skip-tags
					      skip-tags-cleanup)
  "Scans FILENAME and returns cons of class list with parents-class alist.
Assumes file existence and readability have already been checked.
   With optional SKIP-TAGS non-nil, does not compute and store lookup tags
for feature definitions.  If SKIP-TAGS is nil, normally a cleanup
routine is called after scanning the features.  SKIP-TAGS-CLEANUP
non-nil suppresses this action."
  (let ((no-kill (get-file-buffer filename))
	(parents-and-class)
	(signatures)
	class class-of-category class-separator class-type
	classes category def-match-data in-comment-flag parent-list
	protocol-list)
    (if no-kill
	(set-buffer no-kill)
      (funcall br-view-file-function filename))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(if skip-tags
	    nil
	  ;; Get all method definitions within this file.
	  (setq signatures (objc-scan-features))
	  (goto-char (point-min)))
	;; Search for class or protocol interface specification.
	(while (re-search-forward objc-class-def-regexp nil t)
	  (setq class nil class-type nil
		category nil parent-list nil protocol-list nil
		def-match-data (match-data))
	  ;;
	  ;; If definition is within a C comment, ignore it.
	  ;; Regexp used for matching a def precludes any "//"
	  ;; comment.
	  (if (setq in-comment-flag
		    (and (c-within-comment-p) (search-forward "*/" nil t)))
	      nil
	    (store-match-data def-match-data)
	    (setq class-type (buffer-substring
			      (match-beginning objc-class-def-type-grpn)
			      (match-end objc-class-def-type-grpn))
		  class-separator
		  (if (match-beginning objc-class-def-separator-grpn)
		      (buffer-substring
		       (match-beginning objc-class-def-separator-grpn)
		       (match-end objc-class-def-separator-grpn)))))
	  ;;
	  (cond (in-comment-flag) ;; Ignore
		;;
		((string-equal class-type "@interface")
		 ;; Class or category definition
		 (setq class (buffer-substring
			      (match-beginning objc-class-name-grpn)
			      (match-end objc-class-name-grpn)))
		 (cond ((null class-separator)
			;; top class definition without any protocols,
			;; nothing more to do
			)
		       ((string-equal class-separator ":")
			;; class definition with parent
			(if (re-search-forward objc-parent-regexp nil t)
			    (setq parent-list
				  (list (buffer-substring
					 (match-beginning
					  objc-parent-name-grpn)
					 (match-end objc-parent-name-grpn))))
			  (error "(objc-get-classes-from-source): '%s' parent definition is invalid."
				 class))
			;; Check if class conforms to protocol list
			(if (and (null skip-tags) (= (following-char) ?\<))
			    (setq protocol-list (objc-scan-protocol-list))))
		       ;;
		       ((string-equal class-separator "\(")
			;; class category definition
			(if (null skip-tags)
			    ;; Check if class conforms to protocol list
			    (progn
			      (skip-chars-forward " \t\n\r")
			      (setq class-of-category
				    (buffer-substring
				     (match-beginning objc-class-name-grpn)
				     (match-end objc-class-name-grpn))
				    category
				    (if (looking-at objc-identifier)
					(progn
					  (goto-char (match-end 0))
					  (skip-chars-forward "\) \t\n\r")
					  (concat
					   "\(" (buffer-substring
						 (match-beginning
						  objc-identifier-grpn)
						 (match-end 
						  objc-identifier-grpn))
					   "\)"))
				      ;; If get here, there is a problem.
				      (error "(objc-get-classes-from-source): '%s' class contains invalid category () delimiters"))
				    class (concat class-of-category category)
				    signatures
				    ;; Add this category def to the default
				    ;; categories class.
				    (cons (objc-feature-normalize
					   ;; Yes, this net line should be
					   ;; (category)class-of-category.
					   (concat category class-of-category)
					   objc-default-category-class)
				    ;; Add a category tag to
				    ;; class-of-category.
					  (cons (objc-feature-normalize
						 category
						 class-of-category)
						signatures)))
			      ;; Check if category conforms to protocol list
			      (if (= (following-char) ?\<)
				  (setq protocol-list
					(objc-scan-protocol-list))))))
		       ;;
		       ((string-equal class-separator "\<")
			;; top class definition conforming to protocols
			(if (null skip-tags)
			    (setq protocol-list (objc-scan-protocol-list))))
		       ;;
		       ;; If get here, there is a bug, so signal an error.
		       (t (error "(objc-get-classes-from-source): '%s' class uses '%s' unhandled definition separator"
				 class class-separator))))
		;;
		(t
		 ;;
		 ;; Protocol definition
		 ;;
		 ;;   Record '<'protocol-name '>' as a class along with its
		 ;;   parent protocols, if any.
		 ;;   If not skip-tags, add the protocol's method
		 ;;   *declarations* as feature tags.
		 (setq class (concat "<"
				     (buffer-substring
				      (match-beginning objc-class-name-grpn)
				      (match-end objc-class-name-grpn))
				     ">")
		       parent-list
		       (if (string-equal class-separator "\<")
			   (objc-scan-protocol-list)))
		 (if (null skip-tags)
		     (setq signatures
			   ;; Add this protocol def to the default protocols
			   ;; class.
			   (cons (objc-feature-normalize
				  class objc-default-protocol-class)
				 signatures)))))
	  (if (null class-type)
	      nil
	    (if class (setq classes (cons class classes)
			    parents-and-class
			    (cons (cons parent-list class)
				  parents-and-class)))
	    (if protocol-list
		;; record all of class' protocols as tags
		(setq signatures
		      (nconc signatures
			     (mapcar
			      (function (lambda (protocol)
					  (objc-feature-normalize
					   protocol class)))
			      protocol-list))))))))
    (if skip-tags
	nil
      (objc-get-feature-tags buffer-file-name signatures)
      (or skip-tags-cleanup (br-feature-tags-save)))
    (or no-kill (kill-buffer (current-buffer)))
    (cons classes (delq nil parents-and-class))))

(defun objc-get-parents-from-source (filename class-name)
  "Scan source in FILENAME and return list of parents of CLASS-NAME.
Assume file existence has already been checked."
    (or (null class-name)
	(car (car (br-rassoc
		   class-name
		   (cdr (objc-get-classes-from-source filename t)))))))

(defun objc-select-path (paths-htable-elt &optional feature-p)
  "Select proper pathname from PATHS-HTABLE-ELT based upon value of optional FEATURE-P.
Selection is between path of class definition and path for features associated
with the class."
  (let ((elt (cdr paths-htable-elt)))
    (if (consp elt) 
	(if feature-p (cdr elt) (car elt))
      ;; Both paths are the same.
      elt)))

(defun objc-set-case (type)
  "Return string TYPE identifier for use as a class name."
  type)

(defun objc-set-case-type (class-name)
  "Return string CLASS-NAME for use as a type identifier."
  class-name)

(defun objc-to-class-end ()
  "Assuming point is at start of class, move to start of line after end of class."
  (interactive)
  (condition-case ()
      (forward-list)
    (error (progn (or (re-search-forward "^}" nil t)
		      (goto-char (point-max))))))
  (forward-line 1))

(defun objc-to-comments-begin ()
  "Skip back from current point past any preceding Objective-C comments.
Presumes no \"/*\" strings are nested within multi-line comments."
  (let ((opoint))
    (while
	(progn (setq opoint (point))
	       ;; To previous line
	       (if (= 0 (forward-line -1))
		   (cond
		     ;; If begins with "//" or ends with "*/", then is a comment.
		     ((looking-at "[ \t]*\\(//\\|$\\)"))
		     ((looking-at ".*\\*/[ \t]*$")
		      (progn (end-of-line)
			     (search-backward "/*" nil t)))
		     (nil)))))
    (goto-char opoint)
    ;; Skip past whitespace
    (skip-chars-forward " \t\n")
    (beginning-of-line)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst objc-class-keyword
  "\\(@interface\\|@protocol\\)[ \t\n]+"
  "Keyword regexp preceding an Objective-C class or protocol definition.
Type of definition is indicated by grouping 'objc-class-def-type-grpn'.")

(defconst objc-class-def-type-grpn 1)

(defconst objc-class-name-before
  (concat "^[ \t]*" objc-class-keyword)
  "Regexp preceding the class name in a class definition.")

(defconst objc-class-name-after
  "\\([ \t\n]+//.*[\n]\\)*[ \t\n]*\\([:\<\(]\\)?"
  "Regexp following the class name in a class definition.")

(defconst objc-interface-before
  "^[ \t]*\\(@interface\\)[ \t\n]+"
  "Regexp preceding the class name in a non-protocol class definition.")

(defconst objc-implementation-before
  "^[ \t]*\\(@implementation\\)[ \t\n]+"
  "Regexp preceding the class name in a class method definition section.")

(defconst objc-protocol-before
  "^[ \t]*\\(@protocol\\)[ \t\n]+"
  "Regexp preceding the protocol name in a formal protocol definition.")

(defconst objc-identifier-chars "_a-zA-Z0-9"
  "String of chars and char ranges that may be used within an Objective-C identifier.")

(defconst objc-identifier
  (concat "\\([_a-zA-Z][" objc-identifier-chars "]*\\)")
  "Regular expression matching an Objective-C identifier.
The identifier is grouping 'objc-identifier-grpn'.")

(defconst objc-identifier-grpn 1)

(defconst objc-class-def-regexp
  (concat objc-class-name-before objc-identifier objc-class-name-after)
  "Regular expression used to match to class definitions in source text.
Type of definition is indicated by grouping 'objc-class-def-type-grpn'.
Class name identifier is grouping 'objc-class-name-grpn'.  Entire grouped
expression ends with one of the following (optional grouping
'objc-class-def-separator-grpn'):
  a ':', indicating that class inherits from parent class following the colon;
  a '\(', indicating a class category definition;
  a '<', indicating protocols to which class conforms;
  no grouping match, indicating that this is a root class with no parent.")

(defconst objc-class-def-separator-grpn 4)

(defconst objc-lang-prefix "objc-"
 "Prefix string that starts \"br-objc.el\" symbol names.")

(defconst objc-parent-regexp
  (concat "[ \t\n]*" objc-identifier "\\([ \t\n]+//.*[\n]\\)?[ \t\n]*")
  "Parent identifier is grouping 'objc-parent-name-grpn'.")

(defconst objc-parent-name-grpn 1)

(defconst objc-src-file-regexp ".\\.[hcmHCM]$"
  "Regular expression matching a unique part of Objective-C source or header file name and no others.")

(defvar objc-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse Objective-C inheritance graph.  'br-build-children-htable' builds
this list.")
(defvar objc-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse Objective-C inheritance graph.  'br-build-parents-htable' builds
this list.")
(defvar objc-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
'br-build-paths-htable' builds this list.")


(defvar objc-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.")
(defvar objc-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.")

(defvar objc-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the list.")
(defvar objc-sys-paths-htable nil
  "Alist whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.")

(defvar objc-lib-prev-search-dirs nil
  "Used to check if 'objc-lib-classes-htable' must be regenerated.")
(defvar objc-sys-prev-search-dirs nil
  "Used to check if 'objc-sys-classes-htable' must be regenerated.")

(defvar objc-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require updating.")

(provide 'br-objc)
