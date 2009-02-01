;;!emacs
;;
;; FILE:         br-c++.el
;; SUMMARY:      Support routines for C++ inheritance browsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:     7-Dec-89
;; LAST-MOD:     21-Sep-95 at 12:28:56 by Bob Weiner
;;
;; Copyright (C) 1989-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;   Properly supports C++ multiple inheritance.
;;
;;   See 'c++-class-def-regexp' for regular expression that matches class
;;   definitions.  You may want to modify it to your own tastes, for
;;   example if you do not want to consider 'struct' definitions as classes
;;   even though grammatically, they are.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-lib hypb br-c-ft))

;;; ************************************************************************
;;; User visible variables
;;; ************************************************************************

(defvar c++-class-keyword
  "\\(class\\|struct\\|union\\)[ \t\n]+"
  "*Keyword regexp preceding a C++ class declaration or definition.")

(defvar   c++-lib-search-dirs nil
  "List of directories below which C++ Library source files are found.
Subdirectories of Library source are also searched.  A Library is a stable
group of classes.")

(defvar   c++-sys-search-dirs nil
  "List of directories below which C++ System source files are found.
Subdirectories of System source are also searched.  A System class is one
that is not yet reusable and is likely to change before release.")


(defconst c++-narrow-view-to-class nil
 "*Non-nil means narrow buffer to just the matching class definition when displayed.")

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun c++-get-classes-from-source (filename &optional skip-tags
					     skip-tags-cleanup)
  "Scans FILENAME and returns cons of class list with parents-class alist.
Handles multiple inheritance.  Assumes file existence and readability have
already been checked.
   With optional SKIP-TAGS non-nil, does not compute and store lookup tags
for member definitions.  If SKIP-TAGS is nil, normally a cleanup
function is called after scanning the members.  SKIP-TAGS-CLEANUP
non-nil suppresses this action."
  (let ((no-kill (get-file-buffer filename))
	class-name-end classes class has-parents open-brace-point
	parents parent-cons signatures)
    (if no-kill
	(set-buffer no-kill)
      (funcall br-view-file-function filename))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(or skip-tags
	    (progn (setq signatures (c++-scan-features))
		   (goto-char (point-min))))
	(while (re-search-forward c++-class-def-regexp nil t)
	  (setq has-parents
		(= ?: (char-after
		       (match-beginning c++-class-def-derived-grpn)))
		class-name-end (match-end c++-class-def-name-grpn)
		;;
		;; Now since we've saved all the match expressions we need
		;; from our last regexp match, we can call functions which
		;; change the match data below here.
		class (c++-normalize-class-match t)
		parent-cons (cons (if has-parents
				      ;; Return parents as a list.
				      (c++-scan-parents))
				  class))
	  ;; Ensure class name not found within a comment
	  (if (c-within-comment-p)
	      (progn (search-forward "*/" nil t)
		     (setq class nil parent-cons nil))
	    (setq classes (cons class classes)
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
					      (c++-scan-features-in-class
					       class open-brace-point
					       (point)))))))))))))
    (if skip-tags
	nil
      (c++-get-feature-tags buffer-file-name (c++-sort-features signatures))
      (or skip-tags-cleanup (br-feature-tags-save)))
    (or no-kill (kill-buffer (current-buffer)))
    (cons classes (delq nil parents))))

(defun c++-class-definition-regexp (class &optional regexp-flag)
  "Return regexp to uniquely match the definition of CLASS name.
Optional REGEXP-FLAG non-nil means CLASS has already been quoted for use in a
regular expression."
  (let ((template-args-regexp (c++-template-args-regexp class)))
    (concat "^[ \t]*"
	    (if template-args-regexp
		;; Only match to a class definition with the same number of
		;; template parameters as <class> since some modules use #ifdef
		;; to define classes with the same name but a different number
		;; of template parameters.
		(format "\\(template[ \t\n\^M]*%s[ \t\n\^M]*\\)"
			template-args-regexp))
	    c++-class-keyword
	    (if regexp-flag
		(c++-class-non-template-name class)
	      (regexp-quote (c++-class-non-template-name class)))
	    c++-class-name-after)))

(defun c++-template-args-regexp (class)
  "Return a regexp matching the number of template args in CLASS or nil when there are no such arguments."
  (if (string-match "<[^!]+>\\'" class)
      (let* ((param "[^,<>]+")
	     (comma (concat "," param)))
	(format "<%s%s>"
		param (mapconcat
		       (function (lambda (c) (if (= c ?\,) comma)))
		       (substring class (1+ (match-beginning 0))
				  (1- (match-end 0)))
		       "")))))

;; Remove only *trailing* template identifiers when class name is looked up.
(defun c++-class-non-template-name (class)
  "Return CLASS name sans any trailing <template> component.
Does not remove whitespace from CLASS."
  (if (and (stringp class) (string-match "<[^!]+>\\'" class))
      (substring class 0 (match-beginning 0))
    class))

(defun c++-get-class-name (class-name template-signature rename-arguments-flag)
  "Return a possibly, parameterized class identifier built from CLASS-NAME and TEMPLATE-SIGNATURE.
If RENAME-ARGUMENTS-FLAG is non-nil, template class argument names are
normalized also to T1,T2,T3, etc.
TEMPLATE-SIGNATURE may be of any of the following forms:
   nil
   template <class T>
   template <class T1, class T2>
   <class T1, class T2>
   <int = 0>."
  (cond ((null template-signature)
	 class-name)
	((stringp template-signature)
	 (let ((types) (start 0))
	   (while (string-match
		   c++-template-parameter-regexp
		   template-signature start)
	     (setq start (match-end 0)
		   types (cons (substring
				template-signature
				(match-beginning c++-template-parameter-grpn)
				(match-end c++-template-parameter-grpn))
			       types)))
	   (if (null types)
	       class-name
	     (setq class-name
		   (format "%s<%s>" class-name
			   (mapconcat 'identity (nreverse types) ",")))
	     (if rename-arguments-flag
		 (c++-normalize-template-arguments class-name)
	       class-name))))
	(t (error "(c++-get-class-name): Second argument, '%s', must be a string or nil."
		  template-signature))))

(defun c++-normalize-class-match (rename-arguments-flag)
  "After a regexp match to a class definition, return the matching class name.
Class name is normalized for use in OO-Browser lookups.
If RENAME-ARGUMENTS-FLAG is non-nil, template class argument names are
normalized also to T1,T2,T3, etc."
 (c++-get-class-name
  (buffer-substring (match-beginning c++-class-def-name-grpn)
		    (match-end c++-class-def-name-grpn))
  (if (match-beginning c++-class-def-template-grpn)
      (buffer-substring
       (match-beginning c++-class-def-template-grpn)
       (match-end c++-class-def-template-grpn)))
  rename-arguments-flag))

(defun c++-normalize-template-arguments (class)
  "Return class with any template arguments renamed to <T> or <T1,T2,T3>."
  (setq class (br-delete-space class))
  (cond ((not (string-match "[<,][ \t\n\^M]*[^,>]*[,>]" class))
	 ;; No type parameters.
	 class)
	;;
	;; Some type parameter.
	((= ?> (aref class (1- (match-end 0))))
	 ;; Class has only one type parameter.
	 (hypb:replace-match-string c++-template-parameter-regexp
				    class "<T>" t))
	(t
	 ;; Class has two or more type parameters.
	 (let ((count 1) (start 0) before after)
	   (while (string-match "[<,][ \t\n\^M]*[^,>]*" class start)
	     (setq before (substring class
				     0 (1+ (match-beginning 0)))
		   after (substring class (match-end 0))
		   class
		   (format "%sT%d%s" before count after)
		   ;; class may have just shrunk, so don't use
		   ;; match-data from above string-match below here.
		   start (- (length class) (length after))
		   count (1+ count)))
	   class))))

(defun c++-scan-parents ()
  "Return list of parents names from a C++ class definition.
Point must be after the colon that begins the parent list and before the
first parent entry when this function is called."
  (let ((parent-list) (again t)
	parent)
    (while (and again (re-search-forward c++-parent-regexp nil t))
      (setq again (= ?, (preceding-char))
	    parent (c++-get-parent-name
		    (buffer-substring (match-beginning c++-parent-name-grpn)
				      (match-end c++-parent-name-grpn)))
	    parent-list (cons parent parent-list)))
    (nreverse parent-list)))

(defun c++-get-parent-name (parent-name)
  ;; We need to handle class definitions like this:
  ;;   template <class T> class PtrList : private List<type-name> {}
  ;; where the parent class is an instantiation of a parameterized class.
  ;; For now, we change the type name to <T> or <T1,T2,T3> when there are 3
  ;; parameters, for purposes of class name matching.
  ;;	
  ;; Test cases:
  ;;
  ;;   (mapcar 'c++-get-parent-name
  ;;	'("par <class _T1=myclass , class _T2 = you >" "parent"
  ;;	  "class<_T1,T2>" "class< __me , int>" "" "<template>"
  ;;      "par<_template>")) 
  ;;   Should yield:
  ;;     ("par<T1,T2>" "parent" "class<T1,T2>" "class<T1,T2>" "" "<template>"
  ;;      "par<T>")
  ;;
  (if (string-match "<\\(.\\|\n\\)+>\\'" parent-name)
      (let ((parent (substring parent-name 0 (match-beginning 0)))
	    (template (substring parent-name (match-beginning 0))))
	(setq parent (hypb:replace-match-string "\\s " parent "" t)
	      parent-name (c++-get-class-name parent template t)))
    parent-name))

(defun c++-get-parents-from-source (filename class-name)
  "Scan source in FILENAME and return list of parents of CLASS-NAME.
Assume file existence has already been checked."
    (or (null class-name)
	(car (car (br-rassoc
		   class-name
		   (cdr (c++-get-classes-from-source filename t)))))))

(defun c++-select-path (paths-htable-elt &optional feature-p)
  "Select proper pathname from PATHS-HTABLE-ELT based upon value of optional FEATURE-P.
Selection is between path of class definition and path for features associated
with the class."
  (let ((elt (cdr paths-htable-elt)))
    (if (consp elt) 
	(if feature-p (cdr elt) (car elt))
      ;; Both paths are the same.
      elt)))

(defun c++-set-case (type)
  "Return string TYPE identifier for use as a class name."
  type)

(defun c++-set-case-type (class-name)
  "Return string CLASS-NAME for use as a type identifier."
  class-name)

(defun c++-to-class-end ()
  "Assuming point is at start of class, move to start of line after end of class."
  (interactive)
  (condition-case ()
      (forward-list)
    (error (progn (or (re-search-forward "^}" nil t)
		      (goto-char (point-max))))))
  (forward-line 1))

(defun c++-to-comments-begin ()
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

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defconst c++-template-prefix
  "\\(template[ \t\n\^M]*<[^>;.{}]+>[ \t\n\^M]*\\)?"
  "Regexp matching a template class or element definition or declaration.
Entire expression is an optional match, so it may be added as a conditional
expression to other regexps.")

(defconst c++-class-name-before
  (concat "^[ \t]*" c++-template-prefix c++-class-keyword)
  "Regexp preceding the class name in a class definition.")

(defconst c++-comment-regexp "\\([ \t\n]*//.*[\n]\\)*[ \t\n]*")

(defconst c++-class-name-after
  (concat c++-comment-regexp "\\([{:]\\)")
  "Regexp following the class name in a class definition.
Last character matched is either the colon preceding the list of class
parents, or the curly brace beginning the class body definition.")

(defconst c++-identifier-chars "_~<>a-zA-Z0-9"
  "String of chars and char ranges that may be used within a C++ or G++ identifier.")

(defconst c++-template-identifier-chars "_a-zA-Z0-9"
  "String of chars and char ranges that may be used within a standard C++ template identifier.
This excludes the template arguments.")

(defconst c++-return-type-chars "_<>a-zA-Z0-9"
  "String of chars and char ranges that may be used within a C++ or G++ return type identifier.")

;; Modified on 3/28/95 to handle C++ names with multiple template
;; parameters, e.g. class<T1,T2,T3>.
(defconst c++-identifier (concat
			  "\\([_~<a-zA-Z][" c++-template-identifier-chars "]*"
			  "[ \t\n\^M]*<[^>;{}]+[ \t\n\^M>]*>\\|[_~<a-zA-Z]["
			  c++-identifier-chars "]*\\)")
  "Regular expression matching a C++ or G++ identifier.")

(defconst c++-class-def-regexp
  (concat c++-class-name-before c++-identifier c++-class-name-after)
  "Regular expression used to match to class definitions in source text.
Class name identifier is grouping 'c++-class-def-name-grpn'.  Optional
class template parameter signature is grouping 'c++-class-def-template-grpn'.
':' derived class indicator begins grouping 'c++-class-def-derived-grpn,'
unless the class is not derived, in which case this grouping begins with
'{'.")

(defconst c++-class-def-template-grpn 1)
(defconst c++-class-def-name-grpn 3)
(defconst c++-class-def-derived-grpn 5)

(defconst c++-lang-prefix "c++-"
 "Prefix string that starts \"br-c++.el\" symbol names.")

(defconst c++-parent-regexp
  (concat c++-comment-regexp
	  "\\(\\(public\\|private\\|protected\\|virtual\\)[ \t\n]+"
	  "\\(\\(public\\|private\\|protected\\|virtual\\)[ \t\n]+\\)?\\)?"
	  c++-identifier c++-comment-regexp "[,{;]")
  "Parent identifier is group 'c++-parent-name-grpn'.")

(defconst c++-parent-name-grpn 6)

(defconst c++-template-parameter-regexp
  "[< \t\n\^M]+\\([^=<> \t\n\^M]+\\)[ \t\n\^M]*\\(=[^,>]+\\)?[,>]"
  "Regexp matching a single C++ <template> specifier argument name.
For example in 'template <class T, int size = 0>', there are two parameter
names, 'T' and 'size'.  The parameter name is grouping
'c++-template-parameter-grpn'.")

(defconst c++-template-parameter-grpn 1)

;; Ellemtel C++ recommendations specify that inline definition files should
;; use the suffix ".icc" and other people use ".I" for such files, so those
;; suffixes are included here.
(defconst c++-src-file-regexp
  "[^.]\\.\\([ch]xx\\|[chCH][chpCHP]?[pP]?\\|icc\\|I\\)$"
  "Regular expression matching a unique part of C++ source or header file name and no others.")

(defvar c++-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse C++ inheritance graph.  'br-build-children-htable' builds
this list.")
(defvar c++-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse C++ inheritance graph.  'br-build-parents-htable' builds
this list.")
(defvar c++-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
'br-build-paths-htable' builds this list.")


(defvar c++-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.")
(defvar c++-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.")

(defvar c++-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the list.")
(defvar c++-sys-paths-htable nil
  "Alist whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.")

(defvar c++-lib-prev-search-dirs nil
  "Used to check if 'c++-lib-classes-htable' must be regenerated.")
(defvar c++-sys-prev-search-dirs nil
  "Used to check if 'c++-sys-classes-htable' must be regenerated.")

(defvar c++-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require updating.")

(provide 'br-c++)
