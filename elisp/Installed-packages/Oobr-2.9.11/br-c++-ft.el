;;!emacs
;;
;; FILE:         br-c++-ft.el
;; SUMMARY:      C++ OO-Browser class and member functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    03-Oct-90
;; LAST-MOD:      5-May-95 at 15:59:53 by Bob Weiner
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

(require 'br-c++)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar c++-cpp-include-dirs '("/usr/include/")
  "*Ordered list of include directories by default searched by C preprocessor.
Each directory must end with a directory separator.  See also
'c++-include-dirs'.")

(defvar c++-include-dirs nil
  "*Ordered list of directories to search for C++ include files.
Each directory must end with a directory separator.  Directories normally
searched by the C++ pre-processor should be set instead in
'c++-cpp-include-dirs'.")

;; Modified on 3/28/95 to handle C++ names with multiple template
;; parameters, e.g. class<T1,T2,T3>.  Unfortunately for our pattern matcher,
;; we also have to allow spaces within the template parameters section.  We
;; consciously do not allow newlines within the parameters section to avoid
;; grabbing too much of the expression that follows.
(defconst c++-return-type-identifier
  (concat "[\[<a-zA-Z]"
	  "[]" c++-template-identifier-chars "]*"
	  "\\(::[]" c++-template-identifier-chars "]+\\)?"
	  "[ \t\n\^M]*<[" c++-return-type-chars " ,]+>[ \t\n\^M]*[*&]*"
	  "\\|[\[<a-zA-Z][]" c++-return-type-chars "]*"
	  "\\(::[\[<a-zA-Z][]" c++-return-type-chars "]+\\)?"
	  "[ \t\n\^M]*[*&]*"))

(defconst c++-type-identifier
  (concat "\\(::\\|[\[<a-zA-Z][]" c++-template-identifier-chars "]*"
	  "[ \t\n\^M]*<[^>;{}]+>[ \t\n\^M]*[*&]*::"
	  "\\|[\[<a-zA-Z][]" c++-identifier-chars "]*[ \t\n\^M]*[*&]*::\\)"))

(defconst c++-type-tag-separator "@"
  "String that separates a tag's type from its normalized definition form.
This should be a single character which is unchanged when quoted for use as a
literal in a regular expression.")

(defconst c++-tag-fields-regexp
  ;; The \\\\? below is necessary because we sometimes use this expression to
  ;; test against a string that has ben regexp-quoted and some of the
  ;; characters in br-feature-type-regexp will then be preceded by \\.
  (format "\\`\\([^%s \n]+\\)%s\\\\?\\(%s \\)\\([^%s\n]+\\)%s"
	  c++-type-tag-separator c++-type-tag-separator br-feature-type-regexp
	  c++-type-tag-separator c++-type-tag-separator)
 "Regexp matching the fields of a C++ feature tag line.
Group 1 is the class of the feature.  Group 2 is the prefix preceding the
feature when displayed within a listing buffer.  Group 3 is the feature name.
The feature definition signature begins at the end of the regexp match,
i.e. (match-end 0), and goes to the end of the string or line.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun c++-add-default-classes ()
  ;; Add to 'system' class table.
  (if br-c-tags-flag
      (c-add-default-classes)
    ;; Even if this flag is nil, the Environment still contains C function
    ;; tag entries, so add this default class.
    (br-add-class "[function]" br-null-path nil)))

(defun c++-member-p ()
  "Prints whether entity at point is a C++ member definition or declaration."
  (interactive)
  (let ((name))
    (save-excursion
      (message
       (concat
	"Is " (if (c++-feature-def-p)
		  (progn (setq name
			       (buffer-substring (match-beginning
						  c++-feature-name-grpn)
						 (match-end
						  c++-feature-name-grpn)))
			 "")
		"not ")
	"a def.  "
	"Is " (if (and (c++-skip-to-statement) (c++-feature-decl))
		  (progn (setq name
			       (buffer-substring (match-beginning
						  c++-feature-name-grpn)
						 (match-end
						  c++-feature-name-grpn)))
			 "")
		"not ")
	"a member decl.  "
	(if name (concat "  Name = " name)))))))

(defun c++-feature-implementors (name)
  "Return unsorted list of C++ feature tags which implement feature NAME.
This includes classes which define the interface for NAME as a pure virtual
function."
  (c++-feature-matches (concat "^" (regexp-quote name) "$")))

(defun c++-feature-locate-p (feature-tag &optional regexp-flag)
  "Leaves point at the start of FEATURE-TAG's definition in the current buffer.
Assumes caller has moved point to the beginning of the buffer or to the point
of desired search start.
Optional REGEXP-FLAG means FEATURE-TAG is a regular expression."
  ;; Match to function definitions, not declarations, except for pure virtual
  ;; functions and friends which are declared, not defined, and so end with a
  ;; ';'.
  ;;
  ;; First move to the proper class implementation if feature-tag does not
  ;; include a <class>:: part and this is not a [default-class], so that if
  ;; two classes in the same file have the same feature signature, we still
  ;; end up at the right one.
  (if (string-match c++-tag-fields-regexp feature-tag)
      (let ((class (substring feature-tag (match-beginning 1) (match-end 1))))
	(setq feature-tag (substring feature-tag (match-end 0)))
	(if regexp-flag
	    (if (not (string-match "\\`\\\\\\[\\|::" feature-tag))
		(re-search-forward (c++-class-definition-regexp class t)
				   nil t))
	  (if (not (string-match "\\`\\[\\|::" feature-tag))
	      (re-search-forward (c++-class-definition-regexp class)
				 nil t)))))
  (let ((found) (start))
    ;; Now look for feature expression.
    (or regexp-flag (setq feature-tag
			  (c++-feature-signature-to-regexp feature-tag)))
    (while (and (re-search-forward feature-tag nil t)
		(setq start (match-beginning 0))
		(not (setq found (not 
				  (if (c-within-comment-p)
				      (progn (search-forward "*/" nil t)
					     t)))))))
    (if found
	(progn (goto-char start)
	       (skip-chars-forward " \t\n")
	       (c++-to-comments-begin)
	       (recenter 0)
	       (goto-char start)
	       t))))

(defun c++-feature-name-to-regexp (name)
  "Converts routine NAME into a regular expression matching the routine's name tag."
  (setq name (c++-feature-signature-to-regexp name))
  (aset name (1- (length name)) ?\()  ;; Match only to functions
  name)

(defun c++-feature-signature-to-name (signature &optional with-class for-display)
  "Extracts the feature name from SIGNATURE.
The feature's class name is dropped from signature unless optional WITH-CLASS
is non-nil.  If optional FOR-DISPLAY is non-nil, a feature type character is
prepended to the name for display in a browser listing."
  (let ((name))
    (cond
     ;; member
     ((string-match c++-tag-fields-regexp signature)
      (setq name (substring signature (match-beginning (if for-display 2 3))
			    (match-end 3)))
      (if with-class
	  (setq name (concat
		      (substring signature (match-beginning 1) (match-end 1))
		      "::" name)))
      ;; Remove any trailing whitespace.
      (br-delete-space name))
     ;;
     ;; unknown
     (t ;; Remove any trailing whitespace and add display prefix.
      (setq name (br-delete-space signature))
      (if for-display (c++-feature-add-prefix name "" signature) name)))))

(defun c++-feature-signature-to-regexp (signature)
  "Given a C++ SIGNATURE, return regexp used to match to its definition."
  (setq signature (regexp-quote signature))
  (let ((prefix-info
	 (if (string-match c++-tag-fields-regexp signature)
	     (prog1 (substring signature (match-beginning 0) (match-end 0))
	       (setq signature (substring signature (match-end 0)))))))
    (if (string-match "[^<>*&  \t]+\\(<[^>]+>\\)::" signature)
	;; Method from a template class.  Match to a method with the same
	;; number of template parameters, regardless of parameter names.
	(let ((pre (substring signature 0 (match-beginning 1)))
	      (mid (substring signature (match-beginning 1) (match-end 1)))
	      (post (substring signature (match-end 1))))
	  (setq signature (concat pre (c++-template-args-regexp mid) post))))
    (let ((pat) (i 0) (c) (len (length signature)))
      (while (< i len)
	(setq c (aref signature i)
	      pat (cond ((= c ? )
			 ;; Allow for possible single line comment
			 ;; following any whitespace, e.g. following
			 ;; each routine argument.
			 (concat pat "[ \t\n\^M]*\\(//.*\\)?"))
			(t
			 (concat pat (char-to-string c))))
	      i (1+ i)))
      (setq pat (concat prefix-info pat)))))

(defun c++-feature-tree-command-p (class-or-signature)
  "Display definition of CLASS-OR-SIGNATURE if a signature and return t, else return nil."
  (if (c++-routine-p class-or-signature)
      (progn
	(if (br-in-browser) (br-to-view-window))
	(br-feature-found-p (br-feature-file class-or-signature)
			    class-or-signature))))

(defun c++-list-features (class &optional indent)
  "Return sorted list of C++ feature tags lexically defined in CLASS."
  (let ((obuf (current-buffer))
	(features)
	(class-tag (concat "\n" class c++-type-tag-separator))
	feature)
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    (if (or (null indent) (<= indent 2))
	;; Include all features.
	(while (search-forward class-tag nil t)
	  (setq features (cons (br-feature-current) features)))
      ;; Omit friend features which are not inherited since indent > 2.
      (let ((friend-regexp (format "%s%% " c++-type-tag-separator)))
	(while (search-forward class-tag nil t)
	  (setq feature (br-feature-current))
	  (or (string-match friend-regexp feature)
	      (setq features (cons feature features))))))
    (set-buffer obuf)
    (c++-sort-features (nreverse features))))

(defun c++-routine-p (str)
  (string-match "([^\)]*)" str))

(defun c++-scan-features ()
  "Return reverse ordered list of C++ routine definitions in current buffer.
Assume point is at beginning of widened buffer."
  (save-excursion
    (let ((routines) class name rout)
      (while (re-search-forward c++-routine-def nil t)
	(setq class ""
	      name (buffer-substring (match-beginning c++-feature-name-grpn)
				     (match-end c++-feature-name-grpn)))
	;;
	;; This code is very subtle but it does the right thing so don't
	;; change it unless you know exactly what you are doing.
	(if (not (match-beginning c++-feature-scope-grpn))
	    ;; This is a non-class function since we did not find a ::
	    ;; scoping operator.
	    (setq class "[function]")
	  ;; Is a member function, but this may set class = "" since prior
	  ;; regexp grouping may have grabbed the type.  Be careful to
	  ;; handle this.
	  (setq class (buffer-substring
		       (match-beginning c++-feature-scope-grpn)
		       (- (match-end c++-feature-scope-grpn) 2)))
	  (if (and (string-equal class "")
		   (match-beginning c++-feature-type-grpn))
	      (setq class (buffer-substring
			   (match-beginning c++-feature-type-grpn)
			   (match-end c++-feature-type-grpn)))))

	(setq rout (buffer-substring (match-beginning 0) (match-end 0)))
	(if (c-within-comment-p)
	    (search-forward "*/" nil t)
	  ;; Move point to precede feature opening brace or pure virtual
	  ;; function declaration semicolon.
	  (backward-char) 
	  (if (= (following-char) ?\{)
	      (condition-case ()
		  ;; Move to end of feature but ignore any error if braces are
		  ;; unbalanced.  Let the compiler tell the user about this.
		  (forward-sexp)
		(error nil)))
	  ;; Ignore matches to class constructs.
	  (if (string-match c++-class-name-before rout)
	      nil
	    (setq rout (c++-feature-normalize rout class name)
		  routines (cons rout routines)))))
      routines)))

(defun c++-sort-features (routine-list)
  (sort routine-list 'c++-feature-lessp))

(defun c++-to-definition (&optional other-win)
  "If point is within a declaration, try to move to its definition.
With OTHER-WIN non-nil, show it in another window."
  (interactive)
  (let ((opoint (point)))
    (cond
     ((c++-include-file other-win))
     ((br-check-for-class (c++-class-decl-p) other-win))
     ((c++-feature other-win))
     ((and (goto-char opoint)
 	   (br-check-for-class (c++-find-class-name) other-win)))
     (t	(beep)
	(message
	 "(OO-Browser):  Select a C++ declaration to move to its definition.")
	nil))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun c++-class-decl-p ()
  "Return nil unless point is within a class declaration, referenced by another
class.  Commented declarations also return nil.  When value is non-nil, it is
the class name from the declaration.  Leave point at start of statement for
visual clarity."
  (c++-skip-to-statement)
  (save-excursion
    (let ((class))
      (and (looking-at c++-class-decl)
	   (setq class (buffer-substring
			(match-beginning c++-class-name-grpn)
			(match-end c++-class-name-grpn)))
	   (if (match-beginning c++-decl-template-grpn)
	       (setq class
		     (c++-get-class-name
		      class (buffer-substring
			     (match-beginning c++-decl-template-grpn)
			     (match-end c++-decl-template-grpn))
		      t))
	     t)
	   (not (c-within-comment-p))
	   (progn (beginning-of-line)
		  (not (looking-at "[ \t]*//")))
	   class))))

(defun c++-feature (&optional other-win)
  "Move point to definition of member given by declaration at point.
Return nil if point is not within a member declaration."
  ;; If '{' follows the feature declaration, then feature is defined right
  ;; here, within the class definition.
  (interactive)
  (let ((feature-def) (ftr) (class) (ftr-pat))
    (cond ((c++-feature-def-p)
	   (recenter 0)
	   t)
	  ((c++-view-friend other-win t))
	  ;; Now look for feature definition in code (non-header) files.
	  ((progn (setq feature-def (c++-feature-def-pat)
			ftr (car (cdr (cdr feature-def)))
			class (car (cdr feature-def))
			ftr-pat (car feature-def))
		  (c++-locate-feature ftr class ftr-pat other-win)))
	  ((c++-feature-decl)
	   (beep)
	   (message "(OO-Browser):  '%s' feature definition not found." ftr)
	   t))))

(defun c++-view-friend (&optional other-win sig-at-point-flag)
  "With point on a friend listing entry, view its source code definition.
With optional OTHER-WIN non-nil, display in another window.
With optional SIG-AT-POINT-FLAG non-nil, assume point is within a friend
signature in a source buffer."
  (interactive)
  (let ((tag
	 (if (not sig-at-point-flag)
	     (br-feature-get-signature)
	   (beginning-of-line)
	   (and (looking-at c++-friend-regexp)
		(looking-at c++-friend-in-class)
		(let ((friend (buffer-substring (match-beginning 0)
						(match-end 0)))
		      (class (c++-get-class-name-from-source)))
		  (c++-feature-normalize
		   friend class
		   (c++-feature-signature-to-name friend) t))))))
    (cond ((or (null tag)
	       ;; Not a friend entry.
	       (/= ?% (aref (c++-feature-signature-to-name tag nil t) 0)))
	   nil)
	  ((= ?\{ (aref tag (1- (length tag))))
	   ;; Friend is defined where declared.
	   (br-feature nil 'view tag)
	   t)
	  ((string-match (format " class \\(%s\\) ?;$"
				 c++-return-type-identifier) tag)
	   ;; friend class
	   (br-view nil nil
		    (c++-normalize-template-arguments
		     (substring tag (match-beginning 1) (match-end 1))))
	   t)
	  (t
	   (if sig-at-point-flag
	       nil ;; Other feature location code will handle this.
	     (br-feature nil t tag) ;; Move point to friend declaration.
	     (c++-feature other-win))))))

(defun c++-feature-add-prefix (feature-name class signature &optional friend-flag)
  "Add a browser listing display prefix to FEATURE-NAME from CLASS based on feature's SIGNATURE."
  (concat (cond (friend-flag "% ")
		((string-match c++-pure-virtual-function-regexp signature)
		 "> ")
		((or (= ?~ (aref feature-name 0))
		     (equal feature-name (c++-class-non-template-name class))
		     (br-member feature-name
				'("operator new" "operator delete")))
		 "+ ")
		(t "- "))
	  feature-name))

(defun c++-feature-decl ()
  (if (looking-at c++-class-decl)
      nil
    (looking-at c++-feature-decl)))

(defun c++-feature-def-p ()
  "Return nil unless point is within a member definition.
Commented member definitions also return nil.
Leaves point at start of statement for visual clarity."
  (c++-skip-to-statement)
  (save-excursion
    (and (not (c-within-comment-p))
	 (save-excursion (beginning-of-line)
			 (not (looking-at "[ \t]*//")))
	 (not (looking-at c++-class-decl))
	 (looking-at (concat c++-at-feature-regexp "[{;,]"))
	 (let ((end-punct))
	   (or (= ?{ 
		  (setq end-punct (save-excursion (goto-char (match-end 0))
						  (preceding-char))))
	       ;; If ends with a '[;,]' then must not have func parens
	       ;; nor simply be a scoped name in order to be a def.
	       ;; If it begins with 'virtual', ends with "= 0" and has
	       ;; parens, then is a deferred virtual function declaration.
	       (if (match-end c++-feature-parens-grpn)
		   (save-restriction
		     (narrow-to-region (match-beginning 0) (match-end 0))
		     (if (looking-at
			  "\\(^\\|[ \t]+\\)virtual[ \t].*=[ \t]*0[ \t]*[,;]")
			 (progn (message "(OO-Browser):  Pure virtual function, definition deferred to descendants.")
				t)))
		 (or (null (match-end c++-feature-scope-grpn))
		     (not (equal (concat
				  (buffer-substring
				   (match-beginning c++-feature-scope-grpn)
				   (match-end c++-feature-name-grpn))
				  (char-to-string end-punct))
				 (buffer-substring (match-beginning 0)
						   (match-end 0)))))))))))

(defun c++-feature-def-pat ()
  "Return (list <feature-def-pat> <feature-class> <feature-name>) associated with declaration at point."
  (and (c++-skip-to-statement)
       (c++-feature-decl)
       ;; Don't regexp-quote member-name yet
       (let* ((member-name (buffer-substring
			    (match-beginning c++-feature-name-grpn)
			    (match-end c++-feature-name-grpn)))
	      (member-modifiers (if (match-end c++-feature-mod-grpn)
				    (br-quote-match c++-feature-mod-grpn)))
	      (scoped-name)
	      (class)
	      (member-type
	       (concat (and (match-end c++-feature-type-grpn)
			    ;; Handle possible regexp bug
			    (not
			     (equal 
			      (match-beginning c++-feature-type-grpn)
			      (match-beginning c++-feature-name-grpn)))
			    (concat (br-quote-match
				     c++-feature-type-grpn)))
		       (if (match-end c++-feature-scope-grpn)
			   (progn (setq scoped-name t
					class (buffer-substring
					       (match-beginning
						c++-feature-scope-grpn)
					       (- (match-end
						   c++-feature-scope-grpn)
						  2)))
				  (if (equal class "")
				      (setq class nil))
				  nil))))
	      (func-args (if (match-end c++-feature-parens-grpn)
			     (cons (match-beginning c++-feature-parens-grpn)
				   (match-end c++-feature-parens-grpn))))
	      (base-cl-args (match-end c++-feature-parens-grpn))
	      (friend)
	      )
	 (and member-type (string-match "[ \t]+$" member-type)
	      (setq member-type (substring member-type 0
					   (match-beginning 0))))
	 ;;
	 ;; Allow for different whitespace between declaration and definition
	 ;; when * or & is part of name and/or type, e.g. "char* id" and "char
	 ;; *id".
	 (if (and (stringp member-type)
		  (string-match "[*&]+$" member-type))
	     (setq member-type
		   (concat (substring member-type 0 (match-beginning 0))
			   "[ \t\n]*" (regexp-quote
				       (substring member-type
						  (match-beginning 0))))))
	 (if (string-match "^[*&]+" member-name)
	     (setq member-name (substring member-name (match-end 0))
		   member-type (concat member-type "[ \t\n]*"
				       (regexp-quote
					(substring member-name 0
						   (match-end 0)))
				       "[ \t\n]*"))
	   (and (stringp member-type)
		(not (equal member-type ""))
		(setq member-type (concat member-type "[ \t\n]*"))))

	 (let ((pre-member-regexp
		(concat
		 c++-template-prefix
		 "\\(\\(auto\\|inline\\|overload\\|static\\|virtual\\)[ \t\n]+\\)?"
		 (if member-modifiers
		     (let ((def-mods "") (mod))
		       (while (string-match "\\([a-z]+\\)[ \t\n]+"
					    member-modifiers)
			 (setq mod (substring member-modifiers
					      (match-beginning 1)
					      (match-end 1))
			       member-modifiers (substring member-modifiers
							   (match-end 0)))
			 (if (not friend)
			     (setq friend (string-equal mod "friend")))
			 (if (equal (string-match
				     c++-type-def-modifier mod) 0)
			     (setq def-mods (concat def-mods "\\(" mod
						    "[ \t\n]+\\)?"))))
		       def-mods))
		 (if (equal (string-match "virtual" member-type) 0)
		     nil member-type)))
	       (post-member-regexp
		(concat
		 ;; Point at beginning of line may imply a non-member func.
		 (if (or scoped-name (not (bolp))) "::")
		 (progn (cond (scoped-name)
			      (friend
			       (progn
				 (if member-type
				     (progn
				       (setq class 
					     (string-match c++-identifier
							   member-type))
				       (if class (setq class
						       (substring
							member-type
							class
							(match-end 0))))))))
			      ;; Class name is not part of declaration
			      ;; nor a 'friend' declaration, so look
			      ;; for declaration within a class
			      ;; definition and locate the class name.
			      ;; If not within a class, assume
			      ;; declaration is global.
			      (t
			       (setq class (c++-get-class-name-from-source))))
			(br-regexp-quote member-name))
		 "[ \t\n]*"
		 (if func-args
		     (concat "\\(" (c++-func-args-regexp func-args)
			     "\\|" (c++-func-args-string func-args)
			     "\\)"))
		 ;; If is a constructor member function, then can have some
		 ;; arguments for base class constructors after a ':'
		 ;; but preceding the '{'.
		 "[ \t\n]*"
		 (and base-cl-args
		      (equal member-name class)
		      "\\(:[^;{}]*\\)?")
		 c++-comment-regexp)))
	   (list
	    (` (lambda (class)
		 (concat "^" (br-regexp-quote class)
			 (, (concat
			     c++-type-tag-separator
			     br-feature-type-regexp " "
			     (br-regexp-quote member-name)
			     c++-type-tag-separator
			     pre-member-regexp))
			 (br-regexp-quote class)
			 (, post-member-regexp))))
	    class member-name)))))

(defun c++-feature-lessp (routine1 routine2)
  (string-lessp (c++-feature-signature-to-name routine1)
		(c++-feature-signature-to-name routine2)))

(defun c++-feature-matches (regexp)
  "Return an unsorted list of feature tags whose names match in part or whole to REGEXP."
  ;; Ensure match to feature names only; also handle "^" and "$" meta-chars
  (setq regexp
	(concat (format "^[^%s \n]+%s%s "
			c++-type-tag-separator c++-type-tag-separator
			br-feature-type-regexp)
		(if (equal (substring regexp 0 1) "^")
		    (progn (setq regexp (substring regexp 1)) nil)
		  c++-identifier-chars)
		(if (equal (substring regexp -1) "$")
		    (substring regexp 0 -1)
		  (concat regexp c++-identifier-chars))
		c++-type-tag-separator))
  (save-excursion
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    (let ((features))
      (while (re-search-forward regexp nil t)
	(setq features (cons (br-feature-current) features)))
      features)))

(defun c++-feature-normalize (routine class name &optional friend-flag)
  (setq class (br-delete-space class)
	name (c++-feature-add-prefix name class routine friend-flag)
	routine (concat class c++-type-tag-separator 
			name c++-type-tag-separator 
			(br-delete-space routine)))
  (let* ((len (length routine))
	 (normal-feature (make-string len ?\ ))
	 (n 0) (i 0)
	 (space-list '(?\  ?\t ?\n ?\^M))
	 (space-regexp "[ \t\n\^M]+")
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
       ;; Remove // style comments
       ((and (= chr ?/)
	     (< (1+ i) len)
	     (= (aref routine (1+ i)) ?/))
	(setq i (+ i 2))
	(while (and (< i len) (/= (aref routine i) ?\n))
	  (setq i (1+ i))))
       (t ;; Normal character
	(aset normal-feature n chr)
	(setq i (1+ i)
	      n (1+ n)))))
    (substring normal-feature 0 n)))

(defun c++-feature-tag-class (signature)
  "Extract the class name from SIGNATURE."
  (cond ((string-match c++-type-tag-separator signature)
	 (substring signature 0 (match-beginning 0)))
	((string-match "\\([^ \t]+\\)::" signature)
	 (substring signature (match-beginning 1) (match-end 1)))
	(t "")))

(defun c++-feature-tags-lookup (class-list ftr-pat &optional other-win)
  "Display routine definition derived from CLASS-LIST, matching FTR-PAT.
Use routine tags table to locate a match.  Caller must use 'set-buffer'
to restore prior buffer when a match is not found."
  (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
  (let  ((classes class-list)
	 (found-ftr)
	 (ftr-regexp)
	 (class)
	 (ftr-path))
    (if (or (null class-list) (equal class-list '(nil)))
	nil
      (while (and (not found-ftr) classes)
	(setq class (car classes)
	      ftr-regexp (funcall ftr-pat class)
	      ftr-path (br-feature-def-file ftr-regexp)
	      found-ftr (if ftr-path
			    (br-edit-feature (br-feature-current)
					     ftr-path other-win))
	      classes (if found-ftr nil (cdr classes))))
      (if found-ftr
	  (or class t)
	(c++-feature-tags-lookup
	 (apply 'append (mapcar (function (lambda (cl) (br-get-parents cl)))
				class-list))
	 ftr-pat)))))

(defun c++-files-with-source (class)
  "Use CLASS to compute set of files that match to a C++ source file regexp.
Return as a list."
  (let ((file (if class (br-class-path class) buffer-file-name)))
    (and file
	 (let* ((src-file-regexp (concat "^" (br-filename-head file)
					 c++-code-file-regexp))
		(dir (file-name-directory file))
		(files (directory-files dir nil src-file-regexp)))
	   (mapcar (function (lambda (f) (concat dir f)))
		   files)))))

(defun c++-find-ancestors-feature (class-list ftr-pat &optional other-win)
  "Scan ancestors of CLASS-LIST and show routine definition matching FTR-PAT."
  ;; If no class, search for non-member function.
  (or class-list (setq class-list '(nil)))
  (let ((obuf (current-buffer)))
    (prog1
	(if (and br-feature-tags-file
		 (file-exists-p br-feature-tags-file)
		 (file-readable-p br-feature-tags-file))
	    (c++-feature-tags-lookup class-list ftr-pat other-win)
	  ;; Only works if features are in same directory as class def.
	  (c++-scan-ancestors-feature class-list ftr-pat other-win))
      (set-buffer obuf))))

(defun c++-find-class-name ()
  "Return current word as a potential class name."
  (save-excursion
    (let* ((start)
	   (ignore "\]\[ \t\n;,.\(\){}*&-")
	   (pat (concat "^" ignore)))
      (forward-char 1)
      (skip-chars-backward ignore)
      (skip-chars-backward pat)
      (setq start (point))
      (skip-chars-forward (concat pat ":"))
      (buffer-substring start (point)))))

(defun c++-func-args-regexp (func-args)
  (let* ((space "\\\\\\s-*")
	 (obuf (current-buffer))
	 (tmp-buf-nm "*br-c++-tmp*")
	 (tmp-buf (progn (if (get-buffer tmp-buf-nm)
			     (kill-buffer tmp-buf-nm))
			 (get-buffer-create tmp-buf-nm))))
    (or tmp-buf (error "OO-Browser: (c++-func-args-regexp) - Can't create tmp-buf."))
    ;; Fill tmp-buffer with all func-args, including parens.
    (copy-to-buffer tmp-buf (car func-args) (cdr func-args))
    
    (set-buffer tmp-buf)
    (let ((quoted-args (br-regexp-quote (buffer-substring
					 (point-min) (point-max)))))
      (erase-buffer)
      (insert quoted-args))

    (goto-char (point-min))
    (if (looking-at "(\\s-*)")
	(replace-match "(\\\\s-*)" t)

      ;; Replace all "\( +" with "\(" temporarily
      (br-buffer-replace "\\(^\\|[^\\]\\)\([ \t\n]+" "\\1\(")
    
      ;; Replace all "+ \)" with "\)" temporarily
      (br-buffer-replace "[ \t\n]+\)" "\)")
    
      ;; Replace all "...\)" with "...@" temporarily
      (br-buffer-replace "\\\\\\.\\\\\\.\\\\\\.\)" "@@@")
    
      ;; Optionalize right hand side of argument assignments.
      (br-buffer-replace "\\([^=,\( \t\n]+\\)\\([ \t\n]*=[^,\)]+\\)"
			 (concat "\\1\\\\( "
				 (br-regexp-quote c++-arg-identifier)
				 "\\\\)? \\\\(\\2\\\\)?"))

      ;; Replace all "\)" with "optional <c++-identifier> \)"
      (br-buffer-replace
       "\\([\(,][^=\)]+\\)\)"
       (concat "\\1\\\\( " (br-regexp-quote c++-arg-identifier)
	       "\\\\)?\)"))

      ;; Replace all  "," with "optional <c++-identifier>,"
      (br-buffer-replace
       "\\([\(,][^=,]+\\),"
       (concat "\\1\\\\( " (br-regexp-quote c++-arg-identifier) "\\\\)?,"))

      ;; Replace all  " *, *" with "<spc>,<spc>"
      (br-buffer-replace "[ \t\n]*,[ \t\n]*" (concat space "," space))
    
      ;; Replace all " +" with "<spc>"
      (br-buffer-replace "[ \t\n]+" space)

      ;; Replace all "\(" with "\(<spc>"
      (br-buffer-replace "\\(^\\|[^\\]\\)\(" (concat "\\1\(" space))
    
      ;; Replace all "\)" with "<spc>\)"
      (br-buffer-replace "\\([^\\]\\)\)" (concat "\\1" space "\)"))

      ;; Replace all & and quoted \\* with "<spc>[*&]+<spc>"
      (br-buffer-replace "\\(&\\|\\\\\\*\\)+" (concat space "\\1" space))

      ;; Replace all "<spc>" with "[ \t\n]*"
      (br-buffer-replace "\\\\s-\\*" "[ \t\n]*")

      ;; Replace all "@@@" with any # of args
      (br-buffer-replace "@@@" "[^\)]*\)")
      )

    ;; Return final buffer as a string.
    (prog1 (buffer-substring (point-min) (point-max))
      (kill-buffer tmp-buf-nm)
      (set-buffer obuf))))

(defun c++-func-args-string (func-args)
  (let* ((space "\\\\\\s-*")
	 (obuf (current-buffer))
	 (tmp-buf-nm "*br-c++-tmp*")
	 (tmp-buf (progn (if (get-buffer tmp-buf-nm)
			     (kill-buffer tmp-buf-nm))
			 (get-buffer-create tmp-buf-nm))))
    (or tmp-buf (error "OO-Browser: (c++-func-args-string) - Can't create tmp-buf."))
    ;; Fill tmp-buffer with all func-args, including parens.
    (copy-to-buffer tmp-buf (car func-args) (cdr func-args))
    
    (set-buffer tmp-buf)
    (let ((quoted-args (br-regexp-quote (buffer-substring
					 (point-min) (point-max)))))
      (erase-buffer)
      (insert quoted-args))

    (goto-char (point-min))
    (if (looking-at "(\\s-*)")
	(replace-match "(\\\\s-*)" t)

      ;; Replace all "\( +" with "\(" temporarily
      (br-buffer-replace "\\(^\\|[^\\]\\)\([ \t\n]+" "\\1\(")
    
      ;; Replace all "+ \)" with "\)" temporarily
      (br-buffer-replace "[ \t\n]+\)" "\)")
    
      ;; Replace all "...\)" with "@@@" temporarily
      (br-buffer-replace "\\\\\\.\\\\\\.\\\\\\.\)" "@@@")

      ;; Optionalize right hand side of argument assignments.
      (br-buffer-replace "\\([^=,\( \t\n]+\\)\\([ \t\n]+=[^,\)]+\\)"
			 (concat "\\1\\\\(\\2\\\\)?"))

      ;; If an arg consists of 2 or more words, replace last with <identifier>
      (br-buffer-replace
       "\\([\(,][^=,\)]*[^ \t\n=,\)]+[ \t\n]+\\)[^ \t\n=,\)]+\\([ \t\n]*[,\)]\\)"
       (concat "\\1" (br-regexp-quote c++-arg-identifier) "\\2"))

      ;; If an arg consists of only 1 word, add a second
      (br-buffer-replace
       "\\([\(,][ \t\n]*\\)\\([^ \t\n=,\)]+\\)\\([ \t\n]*[,\)]\\)"
       (concat "\\1\\2 " (br-regexp-quote c++-arg-identifier) "\\3"))

      ;; Replace all  " *, *" with "<spc>,<spc>"
      (br-buffer-replace "[ \t\n]*,[ \t\n]*" (concat space "," space))
    
      ;; Replace all " +" with "<spc>"
      (br-buffer-replace "[ \t\n]+" space)

      ;; Replace all "\(" with "\(<spc>"
      (br-buffer-replace "\\(^\\|[^\\]\\)\(" (concat "\\1\(" space))
    
      ;; Replace all "\)" with "<spc>\)"
      (br-buffer-replace "\\([^\\]\\)\)" (concat "\\1" space "\)"))

      ;; Replace all & and quoted \\* with "<spc>[*&]+<spc>"
      (br-buffer-replace "\\(&\\|\\\\\\*\\)+" (concat space "\\1" space))

      ;; Replace all "<spc>" with "[ \t\n]*"
      (br-buffer-replace "\\\\s-\\*" "[ \t\n]*")

      ;; Replace all "@@@" with any # of args
      (br-buffer-replace "@@@" "[^\)]*\)")
      )

    ;; Return final buffer as a string.
    (prog1 (buffer-substring (point-min) (point-max))
      (kill-buffer tmp-buf-nm)
      (set-buffer obuf))))

(defun c++-get-class-name-from-source ()
  "Return class name from closest class definition preceding point or nil."
  (let ((opoint (point))
	(class))
    (save-excursion
      (if (re-search-backward c++-class-def-regexp nil t)
	  (progn (goto-char (match-beginning c++-class-def-derived-grpn))
		 (setq class (c++-normalize-class-match nil))
		 ;; Ensure that declaration occurs within class definition.
		 (forward-list)
		 (and (> (point) opoint)
		      class))))))

(defun c++-get-feature-tags (routine-file &optional routine-list)
  "Scan C++ ROUTINE-FILE and hold routine tags in 'br-feature-tags-file'.
Assume ROUTINE-FILE has already been read into a buffer and that
'br-feature-tags-init' has been called.  Optional ROUTINE-LIST can be
provided so that a non-standard scan function can be used before calling
this function."
  (interactive)
  (let ((obuf (current-buffer)))
    (or routine-list
	(setq routine-list (c++-sort-features (nreverse
						  (c++-scan-features)))))
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    ;; Delete any prior routine tags associated with routine-file
    (if (search-forward routine-file nil 'end)
	(progn (forward-line -1)
	       (let ((start (point)))
		 (search-forward "\^L" nil 'end 2)
		 (backward-char 1)
		 (delete-region start (point))
		 )))
    (if routine-list
	(progn (insert "\^L\n" routine-file "\n")
	       (mapcar (function (lambda (tag) (insert tag "\n")))
		       routine-list)
	       ))
    (set-buffer obuf)))

(defun c++-include-file (&optional other-win)
  "If point is on an include file line, try to display file.
Return non-nil iff an include file line, even if file is not found.
Look for include file in 'c++-cpp-include-dirs' and in directory list
'c++-include-dirs'."
  (let ((opoint (point)))
    (beginning-of-line)
    (if (looking-at c++-include-regexp)
	(let ((incl-type (string-to-char
			  (buffer-substring (match-beginning 1)
					    (1+ (match-beginning 1)))))
	      (file (buffer-substring (match-beginning 2) (match-end 2)))
	      (path)
	      (dir-list c++-include-dirs)
	      (found))
	  (goto-char opoint)
	  (setq dir-list (if (= incl-type ?<)
			     (append dir-list c++-cpp-include-dirs)
			   (cons (file-name-directory buffer-file-name)
				 dir-list)))
	  (while dir-list
	    (setq path (concat (car dir-list) file)
		  dir-list (if (setq found (file-exists-p path))
			       nil
			     (cdr dir-list))))
	  ;;
	  ;; If not found in normal include dirs, check all Env paths also.
	  ;;
	  (if (not found)
	      (let ((paths (delq nil (hash-map 'cdr br-paths-htable))))
		(while paths
		  (setq path (car paths))
		  (if (string-equal (file-name-nondirectory path) file)
		      (setq found t paths nil)
		    (setq paths (cdr paths))))))
	  ;;
	  ;; If found, display file
	  ;;
	  (if found
	      (if (file-readable-p path)
		  (progn
		    (funcall br-edit-file-function path other-win)
		    (if (not (fboundp 'br-lang-mode))
			(c++-mode-setup))
		    (br-major-mode))
		(beep)
		(message "(OO-Browser):  Include file '%s' unreadable." path))
	    (beep)
	    (message "(OO-Browser):  Include file '%s' not found." file))
	  path)
      (goto-char opoint)
      nil)))

(defun c++-locate-feature (ftr class ftr-pat &optional other-win)
  ;; 'class' may = nil, implying non-member function
  (or class (setq class "[function]"))
  (let ((def-class))
    (if (and ftr-pat
	     (setq def-class
		   (c++-find-ancestors-feature (list class)
					       ftr-pat other-win)))
	(progn (if (and class (not (equal class def-class)))
		   (message
		     "Member `%s` of class '%s' inherited from class '%s'."
		     ftr class def-class))
	       t))))

(defun c++-scan-ancestors-feature (class-list ftr-pat &optional other-win)
  "Display routine definition derived from CLASS-LIST, matching FTR-PAT.
Scan files with same base name as class file."
  (let  ((classes class-list)
	 (found-ftr)
	 (code-def-files)
	 (file)
	 (ftr-regexp)
	 (class))
    (if (null class-list)
	nil
      (while (and (not found-ftr) classes)
	(setq class (car classes)
	      code-def-files (c++-files-with-source class)
	      ftr-regexp (funcall ftr-pat class))
	(while (and (setq file (car code-def-files))
		    (not (setq found-ftr
			       (br-feature-found-p file ftr-regexp
						   nil other-win t))))
	  (setq code-def-files (cdr code-def-files)))
	(setq classes (if found-ftr nil (cdr classes))))
      (if found-ftr
	  (or class t)
	(c++-scan-ancestors-feature
	 (apply 'append (mapcar (function (lambda (cl) (br-get-parents cl)))
				class-list))
	 ftr-pat)))))

(defun c++-scan-features-in-class (class start end)
  "Return reverse ordered list of C++ routine definitions within CLASS def.
START and END give buffer region to search."
  (setq class (br-delete-space class))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let ((routines) rout name type)
	;;
	;; Get member definitions and pure virtual declarations.
	;;
	(while (re-search-forward c++-routine-def-in-class nil t)
	  (setq start (match-beginning 0)
		name (buffer-substring
		      (match-beginning c++-feature-name-grpn)
		      (match-end c++-feature-name-grpn))
		type (if (match-beginning c++-feature-type-grpn)
			 (buffer-substring
			  (match-beginning c++-feature-type-grpn)
			  (match-end c++-feature-type-grpn)))
		rout (buffer-substring (match-beginning 0) (match-end 0))
		;; Do this after done getting groupings from the search.
		type (if type (br-delete-space type)))
	  ;; This is necessary to remove a possible double expression match
	  ;; where there is a blank line within the match.
	  (if (string-match "[\n\^M]\\([ \t]*[\n\^M]\\)+" rout)
	      (progn (setq rout (substring rout (match-end 0)))
		     (goto-char (+ start (match-end 0))))
	    (if (c-within-comment-p)
		(search-forward "*/" nil t)
	      ;; Move point to precede feature opening brace or pure virtual
	      ;; function declaration semicolon.
	      (backward-char)
	      (if (= (following-char) ?\{)
		  (condition-case ()
		      ;; Move to end of feature but ignore any error if braces
		      ;; are unbalanced.  Let the compiler tell the user about
		      ;; this.
		      (forward-sexp)
		    (error nil)))
	      (if (string-match c++-friend-regexp rout)
		  ;; skip friends until later
		  nil
		;; Handle type conversion ops:  operator int() { return i; }
		(if (equal type "operator") (setq name (concat type " " name)
						  type nil))
		(setq rout (c++-feature-normalize rout class name)
		      routines (cons rout routines))))))
	;;
	;; Get friend declarations.
	;;
	(goto-char (point-min))
	(while (re-search-forward c++-friend-regexp nil t)
	  (beginning-of-line)
	  (if (not (looking-at c++-friend-in-class))
	      nil
	    (setq start (match-beginning 0)
		  name (buffer-substring
			(match-beginning c++-feature-name-grpn)
			(match-end c++-feature-name-grpn))
		  type (if (match-beginning c++-feature-type-grpn)
			   (buffer-substring
			    (match-beginning c++-feature-type-grpn)
			    (match-end c++-feature-type-grpn)))
		  rout (buffer-substring (match-beginning 0) (match-end 0))
		  ;; Do this after done getting groupings from the search.
		  type (if type (br-delete-space type)))
	    ;; This is necessary to remove a possible double expression match
	    ;; where there is a blank line within the match.
	    (if (string-match "[\n\^M]\\([ \t]*[\n\^M]\\)+" rout)
		(progn (setq rout (substring rout (match-end 0)))
		       (goto-char (+ start (match-end 0))))
	      (if (c-within-comment-p)
		  (search-forward "*/" nil t)
		;; Handle type conversion ops: operator int() { return i; }
		(if (equal type "operator") (setq name (concat type " " name)
						  type nil))
		(setq rout (c++-feature-normalize rout class name t)
		      routines (cons rout routines)))))
	  ;; Move to next entry.
	  (or (= (forward-line 1) 0) (end-of-line)))
	routines))))

(defun c++-skip-past-comments ()
  "Skip over comments immediately following point."
  (skip-chars-forward " \t\n")
  (while
      (cond ((looking-at "//")
	     (equal (forward-line 1) 0))
	    ((looking-at "/\\*")
	     (re-search-forward "\\*/" nil t))
	    (t nil))))

(defun c++-skip-to-statement ()
  (if (re-search-backward "\\(^\\|[;{}]\\)[ \t]*" nil t)
      (progn (goto-char (match-end 0))
	     (skip-chars-forward " \t")
	     t)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst c++-code-file-regexp "\\.\\(cxx\\|[cC][cC]?P?\\)$"
  "Regular expression matching a unique part of C++ source (non-header) file name and no others.")

(defconst c++-include-regexp
  "[ \t/*]*#[ \t]*include[ \t]+\\([\"<]\\)\\([^\">]+\\)[\">]"
  "Regexp to match to C++ include file lines.  File name is grouping 2.  Type
of include, user-specified via double quote, or system-related starting with
'<' is given by grouping 1.")

(defconst c++-type-def-modifier
  "\\(auto\\|const\\|inline\\|mutable\\|register\\|static\\|typedef\\)")

(defconst c++-type-modifier-keyword
  (concat "\\(\\(auto\\|const\\|explicit\\|extern[ \t\n\^M]+\"[^\"]+\"\\|"
	  "extern\\|friend\\|inline\\|mutable\\|overload\\|"
	  "register\\|static\\|typedef\\|virtual\\)[ \t\n\^M]+\\)"))

(defconst c++-member-modifier-keyword
  "\\(\\([ \t\n\^M]+const\\|[ \t\n\^M]+mutable\\)?\\([ \t\n\^M]*[=:][^;{]+\\)?\\)")

(defconst c++-type-identifier-group
  ;; It is critical that the final part of this expression, [*& \t\n\^M]+,
  ;; stay exactly as it is or certain feature definitions may be missed or
  ;; segmented improperly.
  ;; If you remove the '*&', "Int &operator=(int j) {}", will not be found
  ;; because the & will be missed.  If you change the '+' to a '*', "main()"
  ;; will show up as "- n" in listing buffers.
  (concat "\\(\\(" c++-return-type-identifier "\\)[*& \t\n\^M]+\\)"))

(defconst c++-function-identifier (concat
			       "[_~<a-zA-Z][^][ \t:;.,~{}()]*")
  "Regular expression matching a C++ or G++ function name.")

;; Old def = "operator[ \t]*[^]) \t:;.,?~{}][^[( \t:;.,~^!|?{}]?[=*]?"
;; Final optional expression is to handle new C++ array operators, e.g.
;;    void operator delete [] (void*);
(defconst c++-operator-identifier "operator[ \t\n\^M]*[^ \t\n\^M:;.,?~{}]+\\([ \t\n\^M]*\\[\\]\\)?"
  "Regular expression matching a C++ or G++ operator name.")

(defconst c++-feature-decl-or-def
  (concat c++-template-prefix
	  "\\(" c++-type-modifier-keyword "*"
	  c++-type-identifier-group "\\)?"
	  "\\(" c++-type-identifier "[ \t\n\^M]*\\)?"
	  "\\(" c++-operator-identifier "\\|" c++-function-identifier "\\|"
	  "[*&]?" c++-identifier "\\)"
	  ;; This old version of the next line matched to things such as:
	  ;;   enum name {};.  Since we don't deal with attributes yet and
	  ;;   since such matches improperly end up in the [function] default
	  ;;   class, we only accept matches with () in them.
	  ;;   "[ \t\n\^M]*\\(\\[[^{;]+\\|([^{;]*)"
	  "[ \t\n\^M]*\\(([^{;]*)"
	  c++-member-modifier-keyword "?\\)")
  "Regexp matching a C++ member declaration or definition.
Member modifier keywords are grouped expression 'c++-feature-mode-grpn'.
Member type is grouped expression 'c++-feature-type-grpn', unless scoping
type name, grouped expression 'c++-feature-scope-grpn' is non-nil, in which
case, grouping 'c++-feature-scope-grpn' is the type plus \"::\". 
Member name is group 'c++-feature-name-grpn'.  Function parentheses, if any,
are group 'c++-feature-parens-grpn'.")

(defconst c++-feature-mod-grpn 3)
(defconst c++-feature-type-grpn 6)
(defconst c++-feature-scope-grpn 10)
(defconst c++-feature-name-grpn 11)
(defconst c++-feature-parens-grpn 14)

(defconst c++-at-feature-regexp
  (concat c++-feature-decl-or-def c++-comment-regexp)
  "See documentation of 'c++-feature-decl-or-def' for grouping expressions.")

(defconst c++-feature-decl
  (concat c++-at-feature-regexp ";")
  "See documentation of 'c++-feature-decl-or-def' for grouping expressions.")

(defconst c++-friend-in-class
  (concat "[ \t]*" c++-at-feature-regexp "[{;]")
  "See documentation of 'c++-feature-decl-or-def' for grouping expressions.
This doesn't limit matches to friends.  See 'c++-friend-regexp' for that.")

(defconst c++-friend-regexp "^[^(){}\n\^M]*[ \t]friend[ \t\n\^M]"
  "Regexp matching a C++ friend declaration or definition at the start of a line or the start of a string.")

(defconst c++-routine-def-terminator-regexp
  ;; Also matches to pure virtual function declarations.
  "\\({\\|[ \t\n\^M]*=[ \t]*0[ \t]*;\\)")

(defconst c++-routine-def
  (concat "^" c++-at-feature-regexp c++-routine-def-terminator-regexp)
  "See documentation of 'c++-feature-decl-or-def' for grouping expressions.")

(defconst c++-routine-def-in-class
  (concat "^[ \t]*" c++-at-feature-regexp c++-routine-def-terminator-regexp)
  "See documentation of 'c++-feature-decl-or-def' for grouping expressions.")

(defconst c++-class-modifier-keyword
  "\\(\\(friend\\|public\\|protected\\)[ \t\n\^M]+\\)")

(defconst c++-class-decl
  (concat c++-class-modifier-keyword "?"
	  c++-template-prefix
	  c++-class-keyword c++-identifier "[ \t]*[;,]")
  "Regexp matching a C++ class declaration.
Template match, if any, is grouping 'c++-decl-template-grpn'.
Class name is grouping 'c++-class-name-grpn'.")

(defconst c++-decl-template-grpn 3)
(defconst c++-class-name-grpn 5)

(defconst c++-arg-identifier
  (concat "[_a-zA-Z][" c++-identifier-chars "]*")
  "Regular expression matching a C++ or G++ function argument identifier.")

(defconst c++-pure-virtual-function-regexp "\)[^=]*=[ \t]*0[ \t]*;\\'"
  "Regexp matching the trailing part of a C++ pure virtual function signature.")

(provide 'br-c++-ft)
