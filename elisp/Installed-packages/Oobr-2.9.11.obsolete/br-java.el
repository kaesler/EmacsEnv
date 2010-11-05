;;!emacs
;;
;; FILE:         br-java.el
;; SUMMARY:      Support routines for Java inheritance browsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    01-Aug-95
;; LAST-MOD:      4-Oct-95 at 13:31:43 by Bob Weiner
;;
;; Copyright (C) 1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-lib hypb hasht))

;;; ************************************************************************
;;; User visible variables
;;; ************************************************************************

(defvar java-class-keyword
  "\\(class\\|interface\\)[ \t\n]+"
  "*Keyword regexp preceding a java class declaration or definition.")

(defvar   java-lib-search-dirs nil
  "List of directories below which java Library source files are found.
Subdirectories of Library source are also searched.  A Library is a stable
group of classes.")

(defvar   java-sys-search-dirs nil
  "List of directories below which java System source files are found.
Subdirectories of System source are also searched.  A System class is one
that is not yet reusable and is likely to change before release.")

(defvar java-package-name nil
  "Name of current packge if any.  Nil otherwise.")

(defconst java-narrow-view-to-class nil
 "*Non-nil means narrow buffer to just the matching class definition when displayed.")

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun java-get-classes-from-source (filename &optional skip-tags
				     skip-tags-cleanup)
  "Scans FILENAME and returns cons of class list with parents-class alist.
Handles multiple inheritance.  Assumes file existence and readability have
already been checked.
   With optional SKIP-TAGS non-nil, does not compute and store lookup tags
for member definitions.  If SKIP-TAGS is nil, normally a cleanup
function is called after scanning the members.  SKIP-TAGS-CLEANUP
non-nil suppresses this action."
  (let ((no-kill (get-file-buffer filename))
	class-name-end classes class has-parents open-brace-point end
	parents-start parents parent-cons parent-list signatures)
    (if no-kill
	(set-buffer no-kill)
      (funcall br-view-file-function filename))
    ;; Don't bother saving anything for this temporary buffer
    (buffer-disable-undo (current-buffer))
    (setq buffer-auto-save-file-name nil)
    ;; Make life simpler
    (br-lang-mode)
    ;; Static initializers confuse the parser and don't define anything
    ;; that we need, so remove them.
    (java-strip-static-code)
    ;; Is more than one package statement allowed?
    (setq java-package-name (java-get-package-name))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward java-class-def-regexp nil t)
	  (setq has-parents
		(not (= ?{ (char-after
			    (match-beginning java-class-def-derived-grpn))))
		parents-start (match-beginning java-class-def-derived-grpn)
		end (match-end 0)
		class-name-end (match-end java-class-def-name-grpn)
		;;
		;; Now since we've saved all the match expressions we need
		;; from our last regexp match, we can call functions which
		;; change the match data below here.
		class (java-normalize-class-match))
	  (goto-char parents-start)
	  (setq parent-list (if has-parents
				;; Return parents as a list.
				(java-scan-parents end)))
	  (if (and (null parent-list)
		   (not (equal class "Object")))
	      ;; All classes have Object as an ancestor, so if
	      ;; no parents are listed, make Object the sole parent.
	      (setq parent-list '("Object")))
	  ;; Ensure class name not found within a comment
	  (if (c-within-comment-p)
	      (progn (search-forward "*/" nil t)
		     (setq class nil parent-cons nil))
	    (setq parent-cons (cons parent-list class)
		  classes (cons class classes)
		  parents (cons parent-cons parents))
	    (or skip-tags
		;; Scan members defined within class
		(progn (goto-char class-name-end)
		       (if (search-forward "{" nil t)
			   (progn (setq open-brace-point (point))
				  (backward-char)
				  ;; Move to class close brace but ignore
				  ;; any error if braces are unbalanced.
				  ;; Let the compiler tell the user about
				  ;; this.
				  (if (condition-case ()
					  (progn (forward-sexp) t)
					(error nil))
				      (setq signatures
					    (append
					     signatures
					     (java-scan-features
					      class open-brace-point
					      (point)))))))))))))
    (if skip-tags
	nil
      (java-get-feature-tags buffer-file-name (java-sort-features signatures))
      (or skip-tags-cleanup (br-feature-tags-save)))
    (or no-kill
	(progn (set-buffer-modified-p nil)
	       (kill-buffer (current-buffer))))
    (cons classes (delq nil parents))))

(defun java-get-package-name()
  "Return the package name of the current file."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward java-package-name-regexp nil t)
	(buffer-substring (match-beginning java-package-name-grpn)
			  (match-end java-package-name-grpn))
      "")))

(defun java-split-identifier (name)
  "Return list of component words (in reverse order) of the given NAME."
  (or (hash-lookup name java-package-htable)
      (let ((s name)
	    start words tmp)
	(while (and (not (null s)) (> (length s) 0))
	  (setq start (string-match java-package-word-regexp s))
	  (if start
	      (progn
		(setq tmp (substring s (match-beginning 1) (match-end 1)))
		(setq s (substring s (match-end 0)))
		(setq words (cons tmp words)))))
	(hash-add words java-package-name java-package-htable))))

(defun java-normalize-class-name (name)
  "Convert class NAME to make it globally unique using current package."
  ;; Currently incomplete.  THe defined class has a package name, but
  ;; the parents do not.  How do we match the parents to the correct
  ;; class if there are multiple matches?
  (or (car (java-split-identifier name))
      (if (null java-package-name)
	  (car (java-split-identifier name))
	;; Note: maybe allow user to pick how many words to prepend.
	(let ((prefix (car (java-split-identifier java-package-name))))
	  (if (and prefix (> (length prefix) 0))
	      (concat prefix "." (car (java-split-identifier name)))
	    (car (java-split-identifier name)))))))

(defun java-class-definition-regexp (class &optional regexp-flag)
  "Return regexp to uniquely match the definition of CLASS name.
Optional REGEXP-FLAG non-nil means CLASS has already been quoted for use in a
regular expression."
  (concat "[ \t]*"
	  java-class-keyword
	    (if regexp-flag
		class
	      (regexp-quote class))
	    java-class-name-after))

(defun java-normalize-class-match ()
  "After a regexp match to a class definition, return the matching class name."
    (java-normalize-class-name
     (buffer-substring (match-beginning java-class-def-name-grpn)
			(match-end java-class-def-name-grpn))))

(defun java-scan-parents (end)
  "Return list of parent names from a java class definition.
Since java permits only single inheritance, the list will include at most one
parent name.  Point must be before the implements or extends keyword that
precedes the parent class name."
  (let (parent-list parent)
    (while (re-search-forward java-parent-regexp end t)
      (setq parent (java-normalize-class-name
		     (buffer-substring (match-beginning java-parent-name-grpn)
				       (match-end java-parent-name-grpn)))
	    parent-list (cons parent parent-list)))
    (nreverse parent-list)))

(defun java-get-parents-from-source (filename class-name)
  "Scan source in FILENAME and return list of parents of CLASS-NAME.
Assume file existence has already been checked."
    (cond ((null class-name) nil)
	  ((equal filename br-null-path)
	   ;; This means there is no source for this class, so 
	   ;; since all classes have Object as an ancestor and there is no
	   ;; where to look for parents, make Object the sole parent.
	   '("Object"))
	  (t (car (car (br-rassoc
			class-name
			(cdr (java-get-classes-from-source filename t))))))))

(defun java-select-path (paths-htable-elt &optional feature-p)
  "Select proper pathname from PATHS-HTABLE-ELT based upon value of optional FEATURE-P.
Selection is between path of class definition and path for features associated
with the class."
  (let ((elt (cdr paths-htable-elt)))
    (if (consp elt) 
	(if feature-p (cdr elt) (car elt))
      ;; Both paths are the same.
      elt)))

(defun java-set-case (type)
  "Return string TYPE identifier for use as a class name."
  type)

(defun java-set-case-type (class-name)
  "Return string CLASS-NAME for use as a type identifier."
  class-name)

(defun java-to-class-end ()
  "Assuming point is at start of class, move to start of line after end of class."
  (interactive)
  (condition-case ()
      (forward-list)
    (error (progn (or (re-search-forward "^}" nil t)
		      (goto-char (point-max))))))
  (forward-line 1))

(defun java-to-comments-begin ()
  "Skip back from current point past any preceding blank lines and comments.
Presumes no \"/*\" strings are nested within multi-line comments."
  (let ((opoint))
    (while (progn (setq opoint (point))
		  ;; To previous line
		  (if (= 0 (forward-line -1))
		      (cond
		       ;; If begins with "//" or ends with "*/", then is a
		       ;; comment.
		       ((looking-at "[ \t]*\\(//\\|$\\)"))
		       ((looking-at ".*\\*/[ \t]*$")
			(end-of-line)
			;; Avoid //*** single line comments here.
			(re-search-backward "\\(^\\|[^/]\\)/\\*" nil t))
		       ((looking-at "[ \t]*$"))))))
    (goto-char opoint)
    ;; Skip past whitespace
    (skip-chars-forward " \t\n")
    (beginning-of-line)))

;; Static initializers confuse the parser, and don't define anything
;; that we need
(defun java-strip-static-code ()
  "Strip the static initializers from this buffer."
  (let (buffer-read-only)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward java-static-init-regexp (point-max) t)
	(goto-char (1- (match-end 0)))
	(let ((start (point)))
	  (if (= (following-char) ?{)
	      (condition-case ()
		  (forward-sexp)
		(error nil)))
	  (delete-region start (point))
	  (delete-region (match-beginning 0) (1- (match-end 0)))
	  )))))

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defconst java-class-modifier-keyword
  "\\(public\\|protected\\|final\\|abstract\\|[ \t\n\^M]+\\)*")

(defconst java-class-name-before
  (concat "^[ \t]*" java-class-modifier-keyword java-class-keyword)
  "Regexp preceding the class name in a class definition.")

(defconst java-class-name-after
  "[ \t\n]+\\({\\|extends\\|implements\\)"
  "Regexp following the class name in a class definition.
Last character matched is either the colon preceding the list of class
parents, or the curly brace beginning the class body definition.")

(defconst java-identifier-chars "_$.a-zA-Z0-9"
  "String of chars and char ranges that may be used within a Java identifier.")

(defconst java-return-type-chars java-identifier-chars
  "String of chars and char ranges that may be used within a Java return type identifier.")

(defconst java-identifier (concat "\\([_$a-zA-Z][" java-identifier-chars "]*\\)")
  "Regular expression matching a Java identifier.")

(defconst java-class-def-regexp
  (concat java-class-name-before java-identifier java-class-name-after
	  "[^{(;]+")
  "Regular expression used to match to class definitions in source text.
Class name identifier is grouping 'java-class-def-name-grpn'. 
':' derived class indicator begins grouping 'java-class-def-derived-grpn,'
unless the class is not derived, in which case this grouping begins with
'{'.")

(defconst java-class-def-name-grpn 3)
(defconst java-class-def-derived-grpn 4)

(defconst java-lang-prefix "java-"
 "Prefix string that starts \"br-java.el\" symbol names.")

(defconst java-parent-regexp
  (concat "\\(\\(implements\\|extends\\|,\\)?[ \t\n]+\\)*[ \t\n]+"
	  java-identifier "[ \t\n]*[ {;]")
  "Parent identifier is group 'java-parent-name-grpn'.")
;; part 2 of original
;;	  "\\(\\(public\\|private\\|protected\\|final\||abstract\\|implements\\|extends\\)[,]?[ \t\n]+\\)?\\)?"

(defconst java-parent-name-grpn 3)

(defconst java-package-name-regexp
  (concat "[ \t\n]*" java-identifier "[ \t\n]*;")
  "Regexp matching a package statement.  Package name is java-package-name-grpn.")

(defconst java-package-name-grpn 1)

(defconst java-package-word-regexp
  "\\([a-zA-z_0-9]*\\)\\.?"
   "Return a single component of a package name.")

(defconst java-static-init-regexp
  "[ \t\n]*static[ \t\n]+{"
  "Regexp matching start of static initializer block.")

(defvar java-package-htable
  (hash-make 7)
  "Hash table of split package names.")

(defconst java-src-file-regexp "[^.]\\.\\(java\\)$"
  "Regular expression matching a unique part of java source or header file name and no others.")

(defvar java-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse java inheritance graph.  'br-build-children-htable' builds
this list.")
(defvar java-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse java inheritance graph.  'br-build-parents-htable' builds
this list.")
(defvar java-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
'br-build-paths-htable' builds this list.")


(defvar java-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.")
(defvar java-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.")

(defvar java-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the
list.")
(defvar java-sys-paths-htable nil
  "Alist whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.")

(defvar java-lib-prev-search-dirs nil
  "Used to check if 'java-lib-classes-htable' must be regenerated.")
(defvar java-sys-prev-search-dirs nil
  "Used to check if 'java-sys-classes-htable' must be regenerated.")

(defvar java-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require
updating.")

(provide 'br-java)
