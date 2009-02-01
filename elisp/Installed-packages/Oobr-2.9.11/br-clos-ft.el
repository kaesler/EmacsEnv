;;!emacs
;;
;; FILE:         br-clos-ft.el
;; SUMMARY:      CLOS OO-Browser class and element functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     lisp, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    03-Oct-90
;; LAST-MOD:      6-Aug-95 at 01:52:28 by Bob Weiner
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-clos set))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst clos-type-identifier
  (concat "[" clos-type-identifier-chars "]+"))

(defconst clos-type-tag-separator ","
  "String that separates a tags type from its normalized definition form.")

(defconst clos-def-form-match "\([^ \t\n\r]+[ \t\n\r]+")

(defconst clos-feature-tag-regexp
  (concat "\\(" clos-type-identifier "\\)"
	  clos-type-tag-separator
	  clos-def-form-match "['\(]?"
	  "\\((setf[^\)]+)\\|[^\(;,]+\\)\\( *(.*)\\)?")
  "Regexp matching a fully qualified, normalized clos feature tag.
Class name is grouping 1.  Feature name is grouping 2.  Optional
argument list (aliased features don't have one) is grouping 3.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun clos-add-default-classes ()
  ;; Add to 'system' class table.
  (let ((classes (set:create (mapcar 'cdr clos-element-type-alist))))
    ;; Methods are broken out into individual classes, so don't add "method"
    ;; as a default class.
    (setq classes (set:remove "method" classes))
    (mapcar
     (function (lambda (class)
		 (br-add-class (concat "[" class "]")
			       br-null-path nil)))
     classes)))

(defun clos-class-routine-to-regexp (class routine-name args)
  "Return regexp matching definition of CLASS's ROUTINE-NAME with ARGS.
ARGs should be a string or nil if routine definition had no argument list,
i.e. an alias."
  (setq class (regexp-quote class)
	routine-name (regexp-quote routine-name)
	args (if (stringp args) (regexp-quote args) args))
  ;; Search for CLOS method definition based on first typed argument.
  (concat "(defmethod[ \t\n\r]+"
	  routine-name "[ \t\n\r]"
	  ;; Alias defmethods don't have an argument list, so don't
	  ;; try to find one unless signature had an argument list.
	  (if (not args)
	      "+"
	    (concat "*[^\)]*[ \t\n\r]" class "[ \t\n\r]*\)"))
	  "\\|"
	  ;; Search for BWlib routine definition where class name is
	  ;; prepended with a colon to the routine name.
	  (concat "(defmethod[ \t\n\r]+" class ":" routine-name
		  "[ \t\n\r]"
		  ;; BWlib alias defmethods don't have an argument list,
		  ;; so don't try to find one unless signature had an
		  ;; argument list.
		  (if (not args) "+" "*\("))))

(defun clos-feature-implementors (ftr-name)
  "Return unsorted list of clos feature tags which implement FTR-NAME."
  (if (string-match "[ \t]+$" ftr-name)
      (setq ftr-name (substring ftr-name 0 (match-beginning 0))))
  (clos-feature-matches (concat "^" (regexp-quote ftr-name) "$")))

(defun clos-feature-locate-p (feature-tag)
  (let (start)
    (if (not (re-search-forward
	      (clos-feature-signature-to-regexp feature-tag) nil t))
	nil
      (setq start (match-beginning 0))
      (goto-char start)
      (skip-chars-forward " \t\n")
      (clos-to-comments-begin)
      (recenter 0)
      (goto-char start)
      t)))

(defun clos-feature-name-to-regexp (name)
  "Converts feature NAME into a regular expression matching the feature's name tag."
  (if (string-match (concat "^" br-feature-type-regexp " ") name)
      (setq name (substring name (match-end 0))))
  (format "%s%s\(\\(%s\\) %s[ \n]"
	  clos-type-identifier clos-type-tag-separator clos-def-form-regexp
	  (regexp-quote name)))

(defun clos-feature-signature-to-name (signature &optional with-class for-display)
  "Extracts the feature name from SIGNATURE.
The feature's class name is dropped from signature unless optional WITH-CLASS
is non-nil.  If optional FOR-DISPLAY is non-nil, a \"- \" is prepended to
the name for display in a browser listing."
  (concat (if for-display "- ")
	  (clos-feature-partial-name signature with-class)))

(defun clos-feature-signature-to-regexp (signature)
  "Given a clos element SIGNATURE, return regexp to match its definition."
  (cond ((string-match (concat "\\`[^ \t\n\r;]+" clos-type-tag-separator)
		       signature)
	 (clos-element-def-to-regexp
	  (substring signature (match-end 0))))
	((string-match (concat "\\(" clos-arg-identifier "\\):\\("
			       clos-element-identifier
			       "\\)[ \t\n\r]*\\(\(\\)?")
		       signature)
	 (clos-class-routine-to-regexp
	  (substring signature (match-beginning 1) (match-end 1))
	  (substring signature (match-beginning 2) (match-end 2))
	  (if (= ?\( (elt signature (match-end 0)))
	      (substring signature (match-beginning 3)))))))

(defun clos-feature-tree-command-p (class-or-signature)
  "Display definition of CLASS-OR-SIGNATURE if a signature and return t, else return nil."
  (if (br-in-browser) (br-to-view-window))
  (br-feature-found-p (br-feature-file class-or-signature)
		      class-or-signature))

(defun clos-list-features (class &optional indent)
  "Return sorted list of clos feature names lexically defined in CLASS."
  (let ((obuf (current-buffer))
	(class-tag (concat "\n" class clos-type-tag-separator))
	(features))
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    ;; Feature defs (methods) for a single class could occur in any file,
    ;; according to Common Lisp rules.
    (while (search-forward class-tag nil t)
      (setq features (cons (br-feature-current) features)))
    (set-buffer obuf)
    (clos-sort-features (nreverse features))))

(defun clos-scan-features ()
  "Return reverse ordered list of clos feature definitions in current buffer.
Assume point is at the beginning of a widened buffer."
  (save-excursion
    (let ((features) (tag-list)
	  ;; t if current file is an Emacs Lisp file and therefore may
	  ;; contain BWlib method definitions.  BWlib is a simple CLOS-like
	  ;; object system for Emacs Lisp written by the author of the
	  ;; OO-Browser for use in InfoDock, but not yet released.
	  (bwlib-flag (and buffer-file-name
			   (string-match "\\.el$" buffer-file-name)
			   t))
	  def-form)
      (while (re-search-forward clos-element-def nil t)
	(setq tag-list (mapcar
			'clos-feature-normalize
			(clos-element-tag-list
			 (setq def-form
			       (buffer-substring
				(match-beginning clos-def-form-grpn)
				(match-end clos-def-form-grpn)))
			 (buffer-substring (match-beginning clos-feature-grpn)
					   (match-end clos-feature-grpn))
			 (if (string-match clos-def-form-with-args-regexp
					   def-form)
			     (clos-scan-routine-arglist))
			 bwlib-flag))
	      features (nconc features tag-list)))
      features)))

(defun clos-scan-routine-arglist ()
  "Return list of routine's formal parameters.  Leaves point after arglist.
Requires that caller has left point in front of arglist.
If routine is an alias, get argument list from the routine aliased, if
defined, else return nil."
  (skip-chars-forward " \t\n\r")
  (if (= (following-char) ?\()
      (buffer-substring (point) (progn (progn (forward-list) (point))))
    ;; No arglist, treat as an alias.
    (let ((aliased-function (read (current-buffer)))
	  arg-list)
      (setq aliased-function
	    (condition-case ()
		(cond ((fboundp 'indirect-function)
		       (indirect-function aliased-function))
		      ((fboundp 'hypb:indirect-function)
		       (indirect-function aliased-function))
		      (t aliased-function))
	      (void-function nil)))
      (if (null aliased-function)
	  nil
	(setq arg-list
	      (cond ((fboundp 'action:params)
		     (action:params aliased-function))
		    ((listp aliased-function)
		     (if (eq (car aliased-function) 'autoload)
			 (error "(clos-scan-routine-arglist): Arglist unknown for autoload functions: %s" aliased-function)
		       (car (cdr aliased-function))))
		    ((funcall (if (fboundp 'compiled-function-p)
				  'compiled-function-p
				'byte-code-function-p)
			      aliased-function)
		     ;; Turn into a list for extraction
		     (car (cdr (cons nil (append aliased-function nil)))))))
	(if arg-list (prin1-to-string arg-list))))))

(defun clos-sort-features (feature-list)
  (sort feature-list 'clos-feature-lessp))

;; !! Need to write clos-to-definition function.
;;    Move from an identifier to its definition as best as possible.
;;    Use the following temporarily.
(fset 'clos-to-definition 'smart-lisp)

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun clos-element-def-to-regexp (element-def)
  "Convert a normalized clos element definition to a regular expression that will match to its definition in the source code."
  (setq element-def (regexp-quote element-def))
  (mapconcat (function (lambda (c)
			 (if (= c ?\ )
			     "[ \t\n\r]+\\(;.*[ \t\n\r]+\\)?"
			   (char-to-string c))))
	     element-def nil))

(defun clos-feature-def-p ()
  "Return nil unless point is within a feature definition.
If point is within a comment, return nil.
Leaves point at start of the definition for visual clarity."
  (if (clos-skip-to-statement)
      (looking-at "\(def")))

(defun clos-feature-partial-name (signature &optional with-class)
  "Extract the feature name without its class name from feature SIGNATURE.
If optional WITH-CLASS is non-nil, class name and 'clos-type-tag-separator'
are prepended to the name returned."
  (if (string-match clos-feature-tag-regexp signature)
      (let ((class (substring signature
			      (match-beginning 1) (match-end 1)))
	    (name (substring signature (match-beginning 2)
			     (match-end 2))))
	(setq name (br-delete-space name))
	(if (string-match (concat "\\`" class ":") name)
	    (setq name (substring name (match-end 0))))
	(if with-class
	    (concat class clos-type-tag-separator name)
	  name))
    signature))

(defun clos-feature-lessp (routine1 routine2)
  (string-lessp (clos-feature-partial-name routine1)
		(clos-feature-partial-name routine2)))
	
(defun clos-feature-matches (regexp)
  "Return an unsorted list of feature tags whose names match in part or whole to REGEXP."
  ;; Ensure match to feature names only; also handle "^" and "$" meta-chars
  (setq regexp
	(concat "^\\(" clos-type-identifier "\\)"
		clos-type-tag-separator
		clos-def-form-match "['\(]?"
		(if (equal (substring regexp 0 1) "^")
		    (progn (setq regexp (substring regexp 1)) nil)
		  (concat "[" clos-identifier-chars "]*"))
		(if (equal (substring regexp -1) "$")
		    (substring regexp 0 -1)
		  (concat regexp "[" clos-identifier-chars "]*"))
		"[ \t\n\r]"))
  (save-excursion
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    (let ((features))
      (while (re-search-forward regexp nil t)
	(backward-char) ;; Might have moved past newline.
	(setq features (cons (br-feature-current) features)))
      features)))

(defun clos-feature-normalize (routine)
  (let* ((len (length routine))
	 (normal-feature (make-string len ?\ ))
	 (n 0) (i 0)
	 (space-list '(?\  ?\t ?\n ?\r))
	 (space-regexp "[ \t\n\r]+")
	 chr)
    (while (< i len)
      (setq chr (aref routine i)) 
      (cond
       ;; Convert sequences of space characters to a single space.
       ((memq chr space-list)
	(aset normal-feature n ?\ )
	(if (string-match space-regexp routine i)
	    (setq i (match-end 0)
		  n (1+ n))
	  (setq i (1+ i)
		n (1+ n))))
       ;;
       ;; Remove ; style comments
       ((= chr ?\;)
	(setq i (1+ i))
	(while (and (< i len) (/= (aref routine i) ?\n))
	  (setq i (1+ i))))
       (t ;; Normal character
	(aset normal-feature n chr)
	(setq i (1+ i)
	      n (1+ n)))))
    (substring normal-feature 0 n)))

(defun clos-element-tag-list (element-type element arglist-string
			      &optional bwlib-flag)
  "Return list of tags (strings) of ELEMENT-TYPE, ELEMENT and its ARGLIST-STRING.
All three arguments should be strings.
Optional BWLIB-FLAG non-nil means check for BWlib expressions of the form:
\(defmethod class:method-name (args)...)."
  (let* ((element-category (downcase element-type))
	 (element-tag-function
	  (intern-soft (concat "clos-" element-category "-tag-list")))
	 (args (if (or (null arglist-string)
		       (string-equal arglist-string ""))
		   ""
		 (concat " " arglist-string)))
	 element-def-and-type)
    (cond ((fboundp element-tag-function)
	   ;; If any such function is defined, it must return a list of
	   ;; element-tags generated from the defining form, even if it
	   ;; generates only 1 tag.
	   (funcall element-tag-function element-type element arglist-string))
	  ((and bwlib-flag
		(string-match clos-def-form-with-args-regexp element-category)
		(string-match "\\`['\(]?\\([^ \t\n\r]+\\):" element))
	   ;; BWlib element definition support
	   (list
	    (format "%s%s\(%s %s%s"
		    (substring element (match-beginning 1) (match-end 1))
		    clos-type-tag-separator
		    element-type element args)))
	  ((equal element-category "defmethod")
	   ;; CLOS defmethod
	   (let ((arglist (if (string-equal args "")
			      t
			    (read arglist-string)))
		 (class)
		 (tags))
	     (if (nlistp arglist)
		 ;; Add to CLOS default 't' class.
		 (list (format "t%s\(defmethod %s" 
			       clos-type-tag-separator element))
	       ;; If any argument in arglist is itself a list, then this is a
	       ;; CLOS method definition with one or more (<arg-name>
	       ;; <type-name>) arguments.  We generate one tag for each arg
	       ;; list, with the tag's class = <type-name>.  We stop looking
	       ;; for specialized arguments if we encounter a keyword
	       ;; beginning with '&'.
	       (setq tags
		     (delq
		      nil
		      (mapcar
		       (function
			(lambda (arg)
			  (cond ((null arglist)
				 ;; Encountered &keyword, so ignore rest of
				 ;; args.
				 nil)
				((null arg) nil)
				((nlistp arg)
				 (and (symbolp arg)
				      (= ?& (aref (symbol-name arg) 0))
				      ;; Encountered &keyword, set up to
				      ;; ignore rest of args.
				      (setq arglist nil)))
				(t
				 ;; Typed argument
				 (setq class (car (cdr arg)))
				 ;; Type may be of the form: (eql <form>)
				 ;; which is used to compute the type.  We
				 ;; can't compute this here, however, so
				 ;; ignore such types.
				 (if (listp class)
				     nil
				   (setq class (symbol-name class))
				   (format "%s%s\(defmethod %s%s"
					   class clos-type-tag-separator
					   element args))))))
		       arglist)))
	       (or tags
		   ;; Add this method to CLOS default 't' class since none of
		   ;; its parameters were specialized.
		   (list (format "t%s\(defmethod %s%s" 
				 clos-type-tag-separator element args))))))
	  ((setq element-def-and-type (assoc element-category
					     clos-element-type-alist))
	   (list (format "[%s]%s\(%s %s%s"
			 (cdr element-def-and-type)
			 clos-type-tag-separator
			 element-type element args)))
	  (t (beep)
	     (message
	      "(clos-element-tag): '%s' is an unknown definition type"
	      element-type)
	     (sit-for 3)))))

(defun clos-feature-tag-class (element-tag)
  "Extract the class name from ELEMENT-TAG."
  (if (string-match (format "\\([^ \t%s]+\\)%s"
			    clos-type-tag-separator
			    clos-type-tag-separator)
		    element-tag)
      (substring element-tag (match-beginning 1) (match-end 1))
    ""))

(defun clos-files-with-source (class)
  "Use CLASS to compute set of files that match to a clos source file regexp.
Return as a list."
  (let ((file (if class (br-class-path class) buffer-file-name)))
    (and file
	 (let* ((src-file-regexp (concat "^" (br-filename-head file)
					 clos-src-file-regexp))
		(dir (file-name-directory file))
		(files (directory-files dir nil src-file-regexp)))
	   (mapcar (function (lambda (f) (concat dir f)))
		   files)))))

(defun clos-find-class-name ()
  "Return current word as a potential class name."
  (save-excursion
    (let* ((start)
	   (ignore " \t\n\r ;,\(\){}")
	   (pat (concat "^" ignore)))
      (forward-char 1)
      (skip-chars-backward ignore)
      (skip-chars-backward pat)
      (setq start (point))
      (skip-chars-forward (concat pat ":"))
      (buffer-substring start (point)))))

(defun clos-get-class-name-from-source ()
  "Return class name from closest class definition preceding point or nil."
  (save-excursion
    (if (re-search-backward clos-class-def-regexp nil t)
	(buffer-substring (match-beginning 1) (match-end 1)))))

(defun clos-get-feature-tags (feature-file &optional feature-list)
  "Scan clos FEATURE-FILE and hold feature tags in 'br-feature-tags-file'.
Assume FEATURE-FILE has already been read into a buffer and that
'br-feature-tags-init' has been called.  Optional FEATURE-LIST can be
provided so that a non-standard scan function can be used before calling
this function."
  (interactive)
  (let ((obuf (current-buffer)))
    (or feature-list
	(setq feature-list (clos-sort-features
			    (nreverse (clos-scan-features)))))
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    ;; Delete any prior feature tags associated with feature-file
    (if (search-forward feature-file nil 'end)
	(progn (forward-line -1)
	       (let ((start (point)))
		 (search-forward "\^L" nil 'end 2)
		 (backward-char 1)
		 (delete-region start (point))
		 )))
    (if feature-list
	(progn (insert "\^L\n" feature-file "\n")
	       (mapcar (function (lambda (tag) (insert tag "\n")))
		       feature-list)))
    (set-buffer obuf)))

(defun clos-skip-past-comments ()
  "Skip over comments immediately following point."
  (skip-chars-forward " \t\n")
  (while
      (cond ((looking-at "//")
	     (equal (forward-line 1) 0))
	    ((looking-at "/\\*")
	     (re-search-forward "\\*/" nil t))
	    (t nil))))

(defun clos-skip-to-statement ()
  (let ((bol (save-excursion (beginning-of-line) (point))))
    (if (save-excursion (search-backward ";" bol t))
	nil  ;; In a comment
      ;; Find definition beginning.
      (re-search-backward "^\(\\|" nil t))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst clos-element-identifier
  (let ((identifier "[^][ \t\n\r;,`'{}()]+"))
    ;; Initial optional paren is for defstructs of the form:
    ;; (defstruct (identifier options))
    (concat "['\(]?\\(" identifier
	    "\\|(setf[ \t\n\r]+" identifier "[ \t\n\r]*)\\)"
	    "\\([ \t\n\r]+'?:" identifier "\\)?"))
  "Regular expression matching a clos element name.
If a method, this includes any method qualifier.  Optional method qualifier
is of the form: :before, :after or :around.  \(setf <slot>) names the writer
method for <slot>.")

(defconst clos-comment-regexp "\\([ \t\n\r]*;.*[\n\r]\\)*[ \t\n\r]*")

(defvar   clos-element-type-alist
  '(("defconstant"  . "constant")
    ("defconst"     . "constant")
    ("defun"        . "function")
    ("defgeneric"   . "generic")
    ("defmacro"     . "macro")
    ("defmethod"    . "method")
    ("defpackage"   . "package")
    ("defparameter" . "parameter")
    ("defsetf"      . "setfunction")
    ("defstruct"    . "structure")
    ("deftype"      . "type")
    ("defvar"       . "variable")
    ("fset"         . "function"))
  "*Alist of (<element-definition-function-string> . <element-type-string>) elements.

Reread the definition of 'clos-def-form-regexp' if you change this variable,
as its value depends on this variable.  You may also need to add to the
definition of 'clos-def-form-with-args-regexp'.")

(defconst clos-def-form-regexp
  (mapconcat 'identity (mapcar 'car clos-element-type-alist) "\\|")
  "*Regexp of Common Lisp/Clos form names that define new element types.
Defclass is omitted since the OO-Browser handles that separately.")

(defconst clos-def-form-with-args-regexp
  "defun\\|defgeneric\\|defmacro\\|defmethod\\|defsetf\\|fset"
  "*Regexp of Common Lisp/Clos defining forms whose signature includes arguments.")

(defconst clos-feature-def-regexp
  (concat "(\\(" clos-def-form-regexp "\\)[ \t\n\r]+\\(\\('?"
	  clos-type-identifier ":\\)?"
	  "\\(" clos-element-identifier "\\)\\)"
	  clos-comment-regexp)
  "Regexp matching a clos element definition.
Defining form, e.g. defun, is group 'clos-def-form-grpn'.
Class plus element name is group 'clos-feature-grpn'.
Class name is group 'clos-feature-type-grpn.
Element name, with optional qualifier but without class, is group
'clos-feature-name-grpn'.")

(defconst clos-def-form-grpn 1)
(defconst clos-feature-grpn 2)
(defconst clos-feature-type-grpn 3)
(defconst clos-feature-name-grpn 4)

(defconst clos-element-def (concat "^[ \t]*" clos-feature-def-regexp)
  "Regexp matching a clos element definition.
See 'clos-feature-def-regexp' for grouping definitions.")

(defconst clos-arg-identifier (concat "[" clos-identifier-chars "]+")
  "Regular expression matching a clos function argument identifier.")

(provide 'br-clos-ft)
