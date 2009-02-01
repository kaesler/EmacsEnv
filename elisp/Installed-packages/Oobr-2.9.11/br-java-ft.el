;;!emacs
;;
;; FILE:         br-java-ft.el
;; SUMMARY:      Java OO-Browser class and member functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     java, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    01-Aug-95
;; LAST-MOD:      4-Oct-95 at 13:41:34 by Bob Weiner
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

(mapcar 'require '(br-c-ft br-java))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar java-cpp-include-dirs '("/usr/include/")
  "*Ordered list of include directories by default searched by C preprocessor.
Each directory must end with a directory separator.  See also
'java-include-dirs'.")

(defvar java-include-dirs nil
  "*Ordered list of directories to search for java include files.
Each directory must end with a directory separator.  Directories normally
searched by the java pre-processor should be set instead in
'java-cpp-include-dirs'.")

(defconst java-return-type-identifier
  (concat "\\([\[a-zA-Z]"
	  "\\|[\[a-zA-Z][]" java-return-type-chars "]*"
	  "[]" java-return-type-chars "]+\\)"
	  "[ \t\n\^M]*"))

(defconst java-type-identifier
  (concat "\\([\[a-zA-Z][]" java-identifier-chars "]*[ \t\n\^M]+\\)"))

(defconst java-type-tag-separator "@"
  "String that separates a tag's type from its normalized definition form.
This should be a single character which is unchanged when quoted for use as a
literal in a regular expression.")

(defconst java-tag-fields-regexp
  ;; The \\\\? below is necessary because we sometimes use this expression to
  ;; test against a string that has been regexp-quoted and some of the
  ;; characters in br-feature-type-regexp will then be preceded by \\.
  (format "\\`\\([^%s \n]+\\)%s\\\\?\\(%s \\)\\([^%s\n]+\\)%s"
	  java-type-tag-separator java-type-tag-separator
	  br-feature-type-regexp java-type-tag-separator
	  java-type-tag-separator)
 "Regexp matching the fields of a java feature tag line.
Group 1 is the class of the feature.  Group 2 is the prefix preceding the
feature when displayed within a listing buffer.  Group 3 is the feature name.
The feature definition signature begins at the end of the regexp match,
i.e. (match-end 0), and goes to the end of the string or line.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun java-member-p ()
  "Prints whether entity at point is a java member definition or declaration."
  (interactive)
  (let ((name))
    (save-excursion
      (message
       (concat
	"Is " (if (java-feature-def-p)
		  (progn (setq name
			       (buffer-substring (match-beginning
						  java-feature-name-grpn)
						 (match-end
						  java-feature-name-grpn)))
			 "")
		"not ")
	"a def.  "
	"Is " (if (and (java-skip-to-statement) (java-feature-decl))
		  (progn (setq name
			       (buffer-substring (match-beginning
						  java-feature-name-grpn)
						 (match-end
						  java-feature-name-grpn)))
			 "")
		"not ")
	"a member decl.  "
	(if name (concat "  Name = " name)))))))

(defun java-feature-implementors (name)
  "Return unsorted list of java feature tags which implement feature NAME.
This includes classes which declare abstract functions with NAME."
  (java-feature-matches (concat "^" (regexp-quote name) "$")))

(defun java-feature-locate-p (feature-tag &optional regexp-flag)
  "Leaves point at the start of FEATURE-TAG's definition in the current buffer.
Assumes caller has moved point to the beginning of the buffer or to the point
of desired search start.
Optional REGEXP-FLAG means FEATURE-TAG is a regular expression."
  ;; Match to function definitions, not declarations, except for abstract
  ;; methods which are declared, not defined, and so end with a ';'.
  ;;
  ;; First move to the proper class implementation if feature-tag does not
  ;; include a <class>:: part and this is not a [default-class], so that if
  ;; two classes in the same file have the same feature signature, we still
  ;; end up at the right one.
  (if (string-match java-tag-fields-regexp feature-tag)
      (let ((class (substring feature-tag (match-beginning 1) (match-end 1))))
	(setq feature-tag (substring feature-tag (match-end 0)))
	(if regexp-flag
	    (if (not (string-match "\\`\\\\\\[\\|::" feature-tag))
		(re-search-forward (java-class-definition-regexp class t)
				   nil t))
	  (if (not (string-match "\\`\\[\\|::" feature-tag))
	      (re-search-forward (java-class-definition-regexp class)
				 nil t)))))
  ;; Now look for feature expression.
  (let ((found) (start))
    (or regexp-flag (setq feature-tag
			  (java-feature-signature-to-regexp feature-tag)))
    (while (and (re-search-forward feature-tag nil t)
		(setq start (match-beginning 0))
		(not (setq found (not 
				  (if (c-within-comment-p)
				      (progn (search-forward "*/" nil t)
					     t)))))))
    (if found
	(progn (goto-char start)
	       (skip-chars-forward " \t\n")
	       (java-to-comments-begin)
	       (recenter 0)
	       (goto-char start)
	       t))))

(defun java-feature-name-to-regexp (name)
  "Converts routine NAME into a regular expression matching the routine's name tag."
  (setq name (java-feature-signature-to-regexp name))
  (aset name (1- (length name)) ?\()  ;; Match only to functions
  name)

(defun java-feature-signature-to-name (signature &optional with-class for-display)
  "Extracts the feature name from SIGNATURE.
The feature's class name is dropped from signature unless optional WITH-CLASS
is non-nil.  If optional FOR-DISPLAY is non-nil, a feature type character is
prepended to the name for display in a browser listing."
  (let ((name))
    (cond
     ;; member
     ((string-match java-tag-fields-regexp signature)
      (setq name (substring signature (match-beginning (if for-display 2 3))
			    (match-end 3)))
      (if with-class
	  (setq name (concat
		      (substring signature (match-beginning 1) (match-end 1))
		      "." name)))
      ;; Remove any trailing whitespace.
      (br-delete-space name))
     ;;
     ;; unknown
     (t ;; Remove any trailing whitespace and add display prefix.
      (setq name (br-delete-space signature))
      (if for-display (java-feature-add-prefix name "" signature) name)))))

(defun java-feature-signature-to-regexp (signature)
  "Given a java SIGNATURE, return regexp used to match to its definition."
  (setq signature (regexp-quote signature))
  (let ((prefix-info
	 (if (string-match java-tag-fields-regexp signature)
	     (prog1 (substring signature (match-beginning 0) (match-end 0))
	       (setq signature (substring signature (match-end 0)))))))
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

(defun java-feature-tree-command-p (class-or-signature)
  "Display definition of CLASS-OR-SIGNATURE if a signature and return t, else return nil."
  (if (java-routine-p class-or-signature)
      (progn
	(if (br-in-browser) (br-to-view-window))
	(br-feature-found-p (br-feature-file class-or-signature)
			    class-or-signature))))

(defun java-list-features (class &optional indent)
  "Return sorted list of java feature tags lexically defined in CLASS."
  (let ((obuf (current-buffer))
	(features)
	(class-tag (concat "\n" class java-type-tag-separator))
	feature)
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    (if (or (null indent) (<= indent 2))
	;; Include all features.
	(while (search-forward class-tag nil t)
	  (setq features (cons (br-feature-current) features)))
      (while (search-forward class-tag nil t)
	(setq feature (br-feature-current))
	(setq features (cons feature features))))
    (set-buffer obuf)
    (java-sort-features (nreverse features))))

(defun java-routine-p (str)
  (string-match "([^\)]*)" str))

(defun java-sort-features (routine-list)
  (sort routine-list 'java-feature-lessp))

(defun java-to-definition (&optional other-win)
  "If point is within a declaration, try to move to its definition.
With OTHER-WIN non-nil, show it in another window."
  (interactive)
  (let ((opoint (point)))
    (cond
     ((java-include-file other-win))
     ((br-check-for-class (java-class-decl-p) other-win))
     ((java-feature other-win))
     ((and (goto-char opoint)
 	   (br-check-for-class (java-find-class-name) other-win)))
     (t	(beep)
	(message
	 "(OO-Browser):  Select a java declaration to move to its definition.")
	nil))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun java-class-decl-p ()
  "Return nil unless point is within a class declaration, referenced by another
class.  Commented declarations also return nil.  When value is non-nil, it is
the class name from the declaration.  Leave point at start of statement for
visual clarity."
  (java-skip-to-statement)
  (save-excursion
    (let ((class))
      (and (looking-at java-class-decl)
	   (setq class (buffer-substring
			(match-beginning java-class-name-grpn)
			(match-end java-class-name-grpn)))
	   (not (c-within-comment-p))
	   (progn (beginning-of-line)
		  (not (looking-at "[ \t]*//")))
	   class))))

(defun java-feature (&optional other-win)
  "Move point to definition of member given by declaration at point.
Return nil if point is not within a member declaration."
  ;; If '{' follows the feature declaration, then feature is defined right
  ;; here, within the class definition.
  (interactive)
  (let ((feature-def) (ftr) (class) (ftr-pat))
    (cond ((java-feature-def-p)
	   (recenter 0)
	   t)
	  ;; Now look for feature definition in code (non-header) files.
	  ((progn (setq feature-def (java-feature-def-pat)
			ftr (car (cdr (cdr feature-def)))
			class (car (cdr feature-def))
			ftr-pat (car feature-def))
		  (java-locate-feature ftr class ftr-pat other-win)))
	  ((java-feature-decl)
	   (beep)
	   (message "(OO-Browser):  '%s' feature definition not found." ftr)
	   t))))

(defun java-feature-add-prefix (feature-name class signature)
  "Add a browser listing display prefix to FEATURE-NAME from CLASS based on feature's SIGNATURE."
  (concat (cond ((string-match java-native-method-regexp signature)
		 "/ ")
		((string-match java-abstract-method-regexp signature)
		 "> ")
		((or (string-equal feature-name class)
		     (string-equal feature-name "finalize"))
		 "+ ")
		(t "- "))
	  feature-name))

(defun java-feature-decl ()
  (if (looking-at java-class-decl)
      nil
    (looking-at java-feature-decl)))

(defun java-feature-def-p ()
  "Return nil unless point is within a member definition.
Commented member definitions also return nil.
Leaves point at start of statement for visual clarity."
  (java-skip-to-statement)
  (save-excursion
    (and (not (c-within-comment-p))
	 (save-excursion (beginning-of-line)
			 (not (looking-at "[ \t]*//")))
	 (not (looking-at java-class-decl))
	 (looking-at (concat java-at-feature-regexp "[{;,]"))
	 (or (= ?\{
		(save-excursion (goto-char (match-end 0))
				(preceding-char)))
	     ;; If ends with a '[;,]' then it must be a native or abstract
	     ;; method to be a definition.
	     (if (match-end java-feature-parens-grpn)
		 (save-restriction
		   (narrow-to-region (match-beginning 0) (match-end 0))
		   (if (looking-at
			"\\(^\\|[ \t\n\^M]\\)\\(abstract\\|native\\)[ \t\n\^M][^;{}]+;")
		       (progn (message "(OO-Browser):  Abstract function, definition deferred to descendants.")
			      t))))))))

(defun java-feature-def-pat ()
  "Return (list <feature-def-pat> <feature-class> <feature-name>) associated with declaration at point."
  (and (java-skip-to-statement)
       (java-feature-decl)
       ;; Don't regexp-quote member-name yet
       (let* ((member-name (buffer-substring
			    (match-beginning java-feature-name-grpn)
			    (match-end java-feature-name-grpn)))
	      (member-modifiers (if (match-end java-feature-mod-grpn)
				    (br-quote-match java-feature-mod-grpn)))
	      (class)
	      (member-type
	       (concat (and (match-end java-feature-type-grpn)
			    ;; Handle possible regexp bug
			    (not
			     (equal 
			      (match-beginning java-feature-type-grpn)
			      (match-beginning java-feature-name-grpn)))
			    (concat (br-quote-match
				     java-feature-type-grpn)))))
	      (func-args (if (match-end java-feature-parens-grpn)
			     (cons (match-beginning java-feature-parens-grpn)
				   (match-end java-feature-parens-grpn))))
	      (base-cl-args (match-end java-feature-parens-grpn)))

	 (and member-type (string-match "[ \t]+$" member-type)
	      (setq member-type (substring member-type 0
					   (match-beginning 0))))
	 (and (stringp member-type)
	      (not (equal member-type ""))
	      (setq member-type (concat member-type "[ \t\n]*")))

	 (let ((pre-member-regexp
		(concat
		 java-type-modifier-keyword
		 (if member-modifiers
		     (let ((def-mods "") (mod))
		       (while (string-match "\\([a-z]+\\)[ \t\n]+"
					    member-modifiers)
			 (setq mod (substring member-modifiers
					      (match-beginning 1)
					      (match-end 1))
			       member-modifiers (substring member-modifiers
							   (match-end 0)))
			 (if (equal (string-match
				     java-type-def-modifier mod) 0)
			     (setq def-mods (concat def-mods "\\(" mod
						    "[ \t\n]+\\)?"))))
		       def-mods))
		 ))
	       (post-member-regexp
		(concat
		 ;; Point at beginning of line may imply a non-member func.
		 (progn
		   ;; Class name is not part of declaration
		   ;; so look for declaration within a
		   ;; class definition and locate the class
		   ;; name.  If not within a class, assume
		   ;; declaration is global.
		   (setq class (java-get-class-name-from-source))
		   (br-regexp-quote member-name))
		 "[ \t\n]*"
		 (if func-args
		     (concat "\\(" (java-func-args-regexp func-args)
			     "\\|" (java-func-args-string func-args)
			     "\\)"))
		 ;; If is a constructor member function, then can have some
		 ;; arguments for base class constructors after a ':'
		 ;; but preceding the '{'.
		 "[ \t\n]*"
		 (and base-cl-args
		      (equal member-name class)
		      "\\(:[^;{}]*\\)?"))))
	   (list
	    (` (lambda (class)
		 (concat "^" (br-regexp-quote class)
			 (, (concat
			     java-type-tag-separator
			     br-feature-type-regexp " "
			     (br-regexp-quote member-name)
			     java-type-tag-separator
			     pre-member-regexp))
			 (br-regexp-quote class)
			 (, post-member-regexp))))
	    class member-name)))))

(defun java-feature-lessp (routine1 routine2)
  (string-lessp (java-feature-signature-to-name routine1)
		(java-feature-signature-to-name routine2)))

(defun java-feature-matches (regexp)
  "Return an unsorted list of feature tags whose names match in part or whole to REGEXP."
  ;; Ensure match to feature names only; also handle "^" and "$" meta-chars
  (setq regexp
	(concat (format "^[^%s \n]+%s%s "
			java-type-tag-separator java-type-tag-separator
			br-feature-type-regexp)
		(if (equal (substring regexp 0 1) "^")
		    (progn (setq regexp (substring regexp 1)) nil)
		  java-identifier-chars)
		(if (equal (substring regexp -1) "$")
		    (substring regexp 0 -1)
		  (concat regexp java-identifier-chars))
		java-type-tag-separator))
  (save-excursion
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    (let ((features))
      (while (re-search-forward regexp nil t)
	(setq features (cons (br-feature-current) features)))
      features)))

(defun java-feature-normalize (routine class name)
  (setq class (br-delete-space class)
	name (java-feature-add-prefix name class routine)
	routine (concat class java-type-tag-separator 
			name java-type-tag-separator 
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

(defun java-feature-tag-class (signature)
  "Extract the class name from SIGNATURE."
  (cond ((string-match java-type-tag-separator signature)
	 (substring signature 0 (match-beginning 0)))
	((string-match "\\([^ \t]+\\)\." signature)
	 (substring signature (match-beginning 1) (match-end 1)))
	(t "")))

(defun java-feature-tags-lookup (class-list ftr-pat &optional other-win)
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
	(java-feature-tags-lookup
	 (apply 'append (mapcar (function (lambda (cl) (br-get-parents cl)))
				class-list))
	 ftr-pat)))))

(defun java-files-with-source (class)
  "Use CLASS to compute set of files that match to a java source file regexp.
Return as a list."
  (let ((file (if class (br-class-path class) buffer-file-name)))
    (and file
	 (let* ((src-file-regexp (concat "^" (br-filename-head file)
					 java-code-file-regexp))
		(dir (file-name-directory file))
		(files (directory-files dir nil src-file-regexp)))
	   (mapcar (function (lambda (f) (concat dir f)))
		   files)))))

(defun java-find-ancestors-feature (class-list ftr-pat &optional other-win)
  "Scan ancestors of CLASS-LIST and show routine definition matching FTR-PAT."
  ;; If no class, search for non-member function.
  (or class-list (setq class-list '(nil)))
  (let ((obuf (current-buffer)))
    (prog1
	(if (and br-feature-tags-file
		 (file-exists-p br-feature-tags-file)
		 (file-readable-p br-feature-tags-file))
	    (java-feature-tags-lookup class-list ftr-pat other-win)
	  ;; Only works if features are in same directory as class def.
	  (java-scan-ancestors-feature class-list ftr-pat other-win))
      (set-buffer obuf))))

(defun java-find-class-name ()
  "Return current word as a potential class name."
  (save-excursion
    (let* ((start)
	   (ignore "\]\[ \t\n;,.\(\){}-")
	   (pat (concat "^" ignore)))
      (forward-char 1)
      (skip-chars-backward ignore)
      (skip-chars-backward pat)
      (setq start (point))
      (skip-chars-forward (concat pat ":"))
      (buffer-substring start (point)))))

(defun java-func-args-regexp (func-args)
  (let* ((space "\\\\\\s-*")
	 (obuf (current-buffer))
	 (tmp-buf-nm "*br-java-tmp*")
	 (tmp-buf (progn (if (get-buffer tmp-buf-nm)
			     (kill-buffer tmp-buf-nm))
			 (get-buffer-create tmp-buf-nm))))
    (or tmp-buf (error "OO-Browser: (java-func-args-regexp) - Can't create tmp-buf."))
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
				 (br-regexp-quote java-arg-identifier)
				 "\\\\)? \\\\(\\2\\\\)?"))

      ;; Replace all "\)" with "optional <java-identifier> \)"
      (br-buffer-replace
       "\\([\(,][^=\)]+\\)\)"
       (concat "\\1\\\\( " (br-regexp-quote java-arg-identifier)
	       "\\\\)?\)"))

      ;; Replace all  "," with "optional <java-identifier>,"
      (br-buffer-replace
       "\\([\(,][^=,]+\\),"
       (concat "\\1\\\\( " (br-regexp-quote java-arg-identifier) "\\\\)?,"))

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

(defun java-func-args-string (func-args)
  (let* ((space "\\\\\\s-*")
	 (obuf (current-buffer))
	 (tmp-buf-nm "*br-java-tmp*")
	 (tmp-buf (progn (if (get-buffer tmp-buf-nm)
			     (kill-buffer tmp-buf-nm))
			 (get-buffer-create tmp-buf-nm))))
    (or tmp-buf (error "OO-Browser: (java-func-args-string) - Can't create tmp-buf."))
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
       (concat "\\1" (br-regexp-quote java-arg-identifier) "\\2"))

      ;; If an arg consists of only 1 word, add a second
      (br-buffer-replace
       "\\([\(,][ \t\n]*\\)\\([^ \t\n=,\)]+\\)\\([ \t\n]*[,\)]\\)"
       (concat "\\1\\2 " (br-regexp-quote java-arg-identifier) "\\3"))

      ;; Replace all  " *, *" with "<spc>,<spc>"
      (br-buffer-replace "[ \t\n]*,[ \t\n]*" (concat space "," space))
    
      ;; Replace all " +" with "<spc>"
      (br-buffer-replace "[ \t\n]+" space)

      ;; Replace all "\(" with "\(<spc>"
      (br-buffer-replace "\\(^\\|[^\\]\\)\(" (concat "\\1\(" space))
    
      ;; Replace all "\)" with "<spc>\)"
      (br-buffer-replace "\\([^\\]\\)\)" (concat "\\1" space "\)"))

      ;; Replace all "<spc>" with "[ \t\n]*"
      (br-buffer-replace "\\\\s-\\*" "[ \t\n]*")

      ;; Replace all "@@@" with any # of args
      (br-buffer-replace "@@@" "[^\)]*\)")
      )

    ;; Return final buffer as a string.
    (prog1 (buffer-substring (point-min) (point-max))
      (kill-buffer tmp-buf-nm)
      (set-buffer obuf))))

(defun java-get-class-name-from-source ()
  "Return class name from closest class definition preceding point or nil."
  (let ((opoint (point))
	(class))
    (save-excursion
      (if (re-search-backward java-class-def-regexp nil t)
	  (progn (goto-char (match-beginning java-class-def-derived-grpn))
		 (setq class (java-normalize-class-match))
		 ;; Ensure that declaration occurs within class definition.
		 (forward-list)
		 (and (> (point) opoint)
		      class))))))

(defun java-get-feature-tags (routine-file &optional routine-list)
  "Scan java ROUTINE-FILE and hold routine tags in 'br-feature-tags-file'.
Assume ROUTINE-FILE has already been read into a buffer and that
'br-feature-tags-init' has been called.  Optional ROUTINE-LIST can be
provided so that a non-standard scan function can be used before calling
this function."
  (interactive)
  (let ((obuf (current-buffer)))
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

(defun java-include-file (&optional other-win)
  "If point is on an include file line, try to display file.
Return non-nil iff an include file line, even if file is not found.
Look for include file in 'java-cpp-include-dirs' and in directory list
'java-include-dirs'."
  (let ((opoint (point)))
    (beginning-of-line)
    (if (looking-at java-include-regexp)
	(let ((incl-type (string-to-char
			  (buffer-substring (match-beginning 1)
					    (1+ (match-beginning 1)))))
	      (file (buffer-substring (match-beginning 2) (match-end 2)))
	      (path)
	      (dir-list java-include-dirs)
	      (found))
	  (goto-char opoint)
	  (setq dir-list (if (= incl-type ?<)
			     (append dir-list java-cpp-include-dirs)
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
			(java-mode-setup))
		    (br-major-mode))
		(beep)
		(message "(OO-Browser):  Include file '%s' unreadable." path))
	    (beep)
	    (message "(OO-Browser):  Include file '%s' not found." file))
	  path)
      (goto-char opoint)
      nil)))

(defun java-locate-feature (ftr class ftr-pat &optional other-win)
  ;; 'class' may = nil, implying non-member function
  (or class (setq class "[function]"))
  (let ((def-class))
    (if (and ftr-pat
	     (setq def-class
		   (java-find-ancestors-feature (list class)
					       ftr-pat other-win)))
	(progn (if (and class (not (equal class def-class)))
		   (message
		     "Member `%s` of class '%s' inherited from class '%s'."
		     ftr class def-class))
	       t))))

(defun java-scan-ancestors-feature (class-list ftr-pat &optional other-win)
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
	      code-def-files (java-files-with-source class)
	      ftr-regexp (funcall ftr-pat class))
	(while (and (setq file (car code-def-files))
		    (not (setq found-ftr
			       (br-feature-found-p file ftr-regexp
						   nil other-win t))))
	  (setq code-def-files (cdr code-def-files)))
	(setq classes (if found-ftr nil (cdr classes))))
      (if found-ftr
	  (or class t)
	(java-scan-ancestors-feature
	 (apply 'append (mapcar (function (lambda (cl) (br-get-parents cl)))
				class-list))
	 ftr-pat)))))

(defun java-scan-features (class start end)
  "Return reverse ordered list of java routine definitions within CLASS def.
START and END give buffer region to search."
  (setq class (br-delete-space class))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let ((routines) rout name)
	;;
	;; Get member definitions and abstract method declarations.
	;;
	(while (re-search-forward java-routine-def nil t)
	  (setq start (match-beginning 0)
		name  (buffer-substring
		       (match-beginning java-feature-name-grpn)
		       (match-end java-feature-name-grpn))
		rout  (buffer-substring (match-beginning 0) (match-end 0)))
	  ;; This is necessary to remove a possible double expression match
	  ;; where there is a blank line within the match.
	  (if (string-match "[\n\^M]\\([ \t]*[\n\^M]\\)+" rout)
	      (progn (setq rout (substring rout (match-end 0)))
		     (goto-char (+ start (match-end 0))))
	    (if (c-within-comment-p)
		(search-forward "*/" nil t)
	      ;; Move point to precede feature opening brace or abstract
	      ;; method declaration semicolon.
	      (backward-char)
	      (if (= (following-char) ?\{)
		  (condition-case ()
		      ;; Move to end of feature but ignore any error if braces
		      ;; are unbalanced.  Let the compiler tell the user about
		      ;; this.
		      (forward-sexp)
		    (error nil)))
	      (setq rout (java-feature-normalize rout class name)
		    routines (cons rout routines)))))
	routines))))

(defun java-skip-to-statement ()
  (if (re-search-backward "\\(^\\|[;{}]\\)[ \t]*" nil t)
      (progn (goto-char (match-end 0))
	     (skip-chars-forward " \t")
	     t)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst java-code-file-regexp "\\.java$"
  "Regular expression matching a unique part of java source (non-header) file name and no others.")

(defconst java-include-regexp
  "[ \t/*]*import[ \t]+\\([^;]+\\)"
  "Regexp to match to java include file lines.  File name is grouping 2.  Type
of include, user-specified via double quote, or system-related starting with
'<' is given by grouping 1.")

(defconst java-type-def-modifier
  "\\(const\\|final\\|static\\|abstract\\|public\\|protected\\|private\\)")

(defconst java-type-modifier-keyword
  (concat "\\(\\(public\\|protected\\|private\\|const\\|abstract\\|"
	  "synchronized\\|final\\|static\\|threadsafe\\|transient\\|"
	  "native\\)[ \t\n\^M]+\\)"))

(defconst java-type-identifier-group
  (concat "\\(\\(" java-return-type-identifier "\\)[ \t\n\^M]+\\)"))

(defconst java-function-identifier (concat
			       "[_a-zA-Z][^][ \t:;.,{}()=]*")
  "Regular expression matching a Java function name.")

(defconst java-arg-identifier
  (concat "[_a-zA-Z][" java-identifier-chars "]*")
  "Regular expression matching a Java function argument identifier.")

(defconst java-feature-decl-or-def
  (concat "^[ \t]*\\(" java-type-modifier-keyword "*"
	  java-type-identifier-group "\\)?"
	  "\\(" java-type-identifier "[ \t\n\^M]*\\)?"
	  "\\(" java-function-identifier "\\|" java-identifier "\\)"
	  ;; It's hard to tell arguments from parenthesized initializing
	  ;; expressions.
	  "[ \t\n\^M]*(\\([^);{}]*\\))\\([][]*\\)"
	  ;; Optional exceptions that a feature can throw.
	  "\\([ \t\n\^M]+throws[ \t\n\^M]+\\("
	  java-identifier  "[, \t\n\^M]*\\)+\\)?"
	  )
  "Regexp matching a java member declaration or definition.
Member modifier keywords are grouped expression 'java-feature-mode-grpn'.
Member type is grouped expression 'java-feature-type-grpn'.  Member name is
group 'java-feature-name-grpn'.  Function parentheses, if any, are group
'java-feature-parens-grpn'.  Comma separated list of exceptions that can be
thrown by a function are group 'java-feature-exceptions-grpn'.")

(defconst java-feature-mod-grpn 2)
(defconst java-feature-type-grpn 5)
(defconst java-feature-name-grpn 9)
(defconst java-feature-parens-grpn 11)
(defconst java-feature-exceptions-grpn 14)

(defconst java-at-feature-regexp
  (concat java-feature-decl-or-def "[ \t\n]*")
  "See documentation of 'java-feature-decl-or-def' for grouping expressions.")

(defconst java-feature-decl
  (concat java-at-feature-regexp "[;{]")
  "See documentation of 'java-feature-decl-or-def' for grouping expressions.")

(defconst java-routine-def-terminator-regexp
  ;; Also matches to abstract method declarations.
  ;; Only native and abstract functions end with a semi-colon.
  "\\({\\|;\\)")

(defconst java-routine-def
  (concat java-at-feature-regexp java-routine-def-terminator-regexp)
  "See documentation of 'java-feature-decl-or-def' for grouping expressions.")

(defconst java-class-decl
  (concat java-class-modifier-keyword 
	  java-class-keyword java-identifier "[ \t]*[;,]")
  "Regexp matching a java class declaration.
Class name is grouping 'java-class-name-grpn'.")

(defconst java-class-name-grpn 4)

;; Old def was: "\\(^\\|[ \t\n\^M]\\)abstract[ \t\n\^M][^;{}]+;"
(defconst java-abstract-method-regexp ";\\'"
  "Regexp matching a Java abstract method signature.")

(defconst java-native-method-regexp "\\(^\\|[ \t\n\^M]\\)native[ \t\n\^M][^;{}]+;"
  "Regexp matching a Java native method signature, one implemented in another language.")

(provide 'br-java-ft)
