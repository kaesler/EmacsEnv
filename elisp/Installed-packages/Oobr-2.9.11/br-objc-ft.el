;;!emacs
;;
;; FILE:         br-objc-ft.el
;; SUMMARY:      Objective-C OO-Browser class and feature functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    03-Oct-90
;; LAST-MOD:      5-May-95 at 15:57:14 by Bob Weiner
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

(require 'br-objc)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst objc-default-category-class "[category]"
  "Name of the default class whose instances are Objective-C categories.")

(defconst objc-default-protocol-class "[protocol]"
  "Name of the default class whose instances are Objective-C protocols.")

(defconst objc-type-identifier
  (concat "[a-zA-Z][" objc-identifier-chars "]*[ \t\n]*[*&]*"))

(defconst objc-type-tag-separator "@"
  "String that separates a tag's type from its normalized definition form.")

(defconst objc-tag-fields-regexp
  ;; The \\\\? below is necessary because we sometimes use this expression to
  ;; test against a string that has ben regexp-quoted and some of the
  ;; characters in br-feature-type-regexp will then be preceded by \\.
  (format "\\`\\([^*& \n]+\\)%s\\\\?\\(%s \\)\\([^%s\n]+\\)\\(%s\\|\\'\\)"
	  objc-type-tag-separator br-feature-type-regexp
	  objc-type-tag-separator objc-type-tag-separator)
 "Regexp matching the fields of an Objective-C feature tag line.
Group 1 is the class of the feature.  Group 2 is the prefix preceding the
feature when displayed within a listing buffer.  Group 3 is the feature name.
The feature definition signature begins at the end of the regexp match,
i.e. (match-end 0), and goes to the end of the string or line.")

(defvar objc-cpp-include-dirs '("/usr/include/")
  "*Ordered list of include directories by default searched by C preprocessor.
Each directory must end with a directory separator.  See also
'objc-include-dirs'.")

(defvar objc-include-dirs nil
  "*Ordered list of directories to search for Objective-C include files.
Each directory must end with a directory separator.  Directories normally
searched by the Objective-C pre-processor should be set instead in
'objc-cpp-include-dirs'.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun objc-add-default-classes ()
  ;; Add to 'system' class table.
  (mapcar (function (lambda (class) (br-add-class class br-null-path nil)))
	  (list objc-default-category-class objc-default-protocol-class))
  (if br-c-tags-flag (c-add-default-classes)))

(defun objc-class-definition-name (class)
  "Convert CLASS name to the way it appears in its source code definition.
Returns a regular expression."
  (cond ((string-match "^<.+>$" class)
	 ;; Remove <> delimiters from protocol class.
	 (regexp-quote (substring class 1 -1)))
	((string-match "^\\([^ \(]+\\) *(\\([^\)]*\\)) *$" class)
	 ;; Allow for whitespace within class(category)
	 (format "%s[ \t\n\r]*([ \t\n\r]*%s[ \t\n\r]*)"
		 (regexp-quote
		  (substring class (match-beginning 1) (match-end 1)))
		 (regexp-quote
		  (substring class (match-beginning 2) (match-end 2)))))
	((string-match "^(\\([^\)]*\\)) *\\([^ ]+\\) *$" class)
	 ;; Allow for whitespace within (category)class
	 (format "%s[ \t\n\r]*([ \t\n\r]*%s[ \t\n\r]*)"
		 (regexp-quote
		  (substring class (match-beginning 2) (match-end 2)))
		 (regexp-quote
		  (substring class (match-beginning 1) (match-end 1)))))
	(t (regexp-quote class))))

(defun objc-class-definition-regexp (class)
  "Return regexp to uniquely match the definition of CLASS name."
  (concat objc-class-name-before (objc-class-definition-name class)
	  objc-class-name-after))

(defun objc-feature-implementors (ftr-name)
  "Return unsorted list of Objective-C feature tags which implement FTR-NAME."
  (objc-feature-matches ftr-name))

(defun objc-feature-locate-p (feature-tag &optional regexp-flag)
  "Leaves point at the start of FEATURE-TAG's definition in the current buffer.
Assumes caller has moved point to the beginning of the buffer or to the point
of desired search start.
Optional REGEXP-FLAG means FEATURE-TAG is a regular expression."
  ;; Match to function definitions, not declarations.
  (let ((found)	(start))
    ;; First move to the proper class implementation if this is not a
    ;; [default-class], so that if two classes in the same file have the same
    ;; feature signature, we still end up at the right one.
    (if (and (not (string-match (if regexp-flag "\\`\\\\\\[" "\\`\\[")
				feature-tag))
	     (string-match objc-tag-fields-regexp feature-tag))
	(let ((class
	       (substring feature-tag (match-beginning 1) (match-end 1))))
	  ;; Protocols don't define methods, they only declare them, so we
	  ;; know we can't be searching for a protocol method definition
	  ;; here, and so there is no special case handling.
	  (re-search-forward (concat objc-implementation-before
				     ;; Assume regexp-quoted class is the
				     ;; same as non-regexp-quoted version
				     ;; since this call will regexp-quote it
				     ;; again; we have no way of
				     ;; un-regexp-quoting it.
				     (objc-class-definition-name class))
			     nil t)))
    ;;
    ;; Now search for feature.
    (or regexp-flag (setq feature-tag
			  (objc-feature-signature-to-regexp feature-tag)))
    (while (and (re-search-forward feature-tag nil t)
		(setq start (match-beginning 0))
		(not (setq found (not (if (c-within-comment-p)
					  (progn (search-forward "*/" nil t)
						 t)))))))
    (if found
	(progn (goto-char start)
	       (skip-chars-forward " \t\n")
	       (objc-to-comments-begin)
	       (recenter 0)
	       (goto-char start)
	       t))))

(defun objc-feature-name-to-regexp (name)
  "Converts feature NAME into a regular expression matching the feature's name tag."
  (cond
   ;;
   ;; protocol name
   ((= (aref name 0) ?\<) (regexp-quote name))
   ;;
   ;; category name
   ((= (aref name 0) ?\()
    ;; Drop any class name following the category.
    (regexp-quote
     (substring name 0 (1+ (string-match "\)" name)))))
   ;;
   ;; feature tag
   ((string-match objc-tag-fields-regexp name)
    (concat
     "^" (regexp-quote (substring name (match-beginning 0) (match-end 0)))))
   ;;
   ;; unrecognized name format
   (t (error "(objc-feature-name-to-regexp): Invalid name, '%s'" name))))

(defun objc-feature-signature-to-name (signature &optional with-class for-display)
  "Extracts the feature name from SIGNATURE.
SIGNATURE may be a feature tag line or a signature extracted from source code.
The feature's class name is dropped from signature unless optional WITH-CLASS
is non-nil.  The feature's type (class feature = +, instance feature = -)
is dropped unless FOR-DISPLAY is non-nil."

  ;; feature tag
  (if (string-match objc-tag-fields-regexp signature)
      (cond ((and with-class for-display)
	     (substring signature (match-beginning 1) (match-end 3)))
	    (for-display
	     (substring signature (match-beginning 2) (match-end 3)))
	    (with-class
	     (concat 
	      (substring signature (match-beginning 1) (1+ (match-end 1)))
	      (substring signature (match-beginning 3) (match-end 3))))
	    (t (substring signature (match-beginning 3) (match-end 3))))
    ;;
    ;; source code signature
    (let ((loop-p t)
	  (name-part (concat "\\`" objc-name-part objc-name-sep))
	  (name-type (concat "\\`" objc-name-part objc-type-sep))
	  (name))
      (cond ((or (= (aref signature 0) ?\<) (= (aref signature 0) ?\())
	     ;; protocol or category tags
	     (setq name
		   (if for-display
		       (concat "@ " signature)
		     signature)))
	    ((string-match (concat "\\`" br-feature-type-regexp) signature)
	     ;; regular feature signature
	     (if for-display 
		 (setq name (concat (substring signature (match-beginning 0)
					       (match-end 0))
				    " ")))
	     (setq signature (concat (substring signature (match-end 0)) ";"))
	     (while (and loop-p (string-match name-part signature))
	       ;; Handles named or unnamed parameters.
	       (if (match-beginning objc-name-part-id)
		   (setq name (concat name
				      (substring
				       signature
				       (match-beginning objc-name-part-id)
				       (match-end objc-name-part-id)))))
	       (if (/= (aref signature (1- (match-end 0))) ?:)
		   (setq loop-p nil
			 signature (substring signature (match-end 0)))
		 (setq name (concat name ":")
		       signature (substring signature (match-end 0)))
		 (if (string-match name-type signature)
		     (setq signature (substring signature (match-end 0)))))))
	    (t (error
		"(objc-feature-signature-to-name): Invalid signature, '%s'"
		signature)))
      name)))

(defun objc-feature-signature-to-regexp (signature)
  "Return regexp to match the definition of an Objective-C element SIGNATURE.
SIGNATURE may be a feature tag line or a signature extracted from source code."
  (cond ((string-match
	  (format
	   "^\\([^*& \n]+\\)%s%s [^%s\n]+%s\\([\<\(][^\>\)\n]+[\>\)]\\(%s\\)?\\)"
	   objc-type-tag-separator br-feature-type-regexp
	   objc-type-tag-separator objc-type-tag-separator
	   objc-identifier)
	  signature)
	 ;; protocol or category signature
	 (let ((class (substring signature (match-beginning 1) (match-end 1)))
	       (element (substring signature (match-beginning 2)
				  (match-end 2))))
	   (if (= (aref element 0) ?\<)
	       (if (string-equal class objc-default-protocol-class)
		   ;; find def of protocol
		   (concat objc-protocol-before
			   (objc-class-definition-name element)
			   "[\< \t\n\r]")
		 ;; find def of class which conforms to protocol
		 (concat objc-interface-before
			 (objc-class-definition-name class)
			 objc-class-name-after
			 "[^\>\{]*[\<,][ \t\n\r]*"
			 (objc-class-definition-name element)
			 "[ \t\n\r]*[\>,]"))
	     (if (string-equal class objc-default-category-class)
		 ;; find def of '[category]@(category-name)class-name'
		 (concat objc-interface-before
			 (objc-class-definition-name element))
	       ;; find def of 'class-name@(category-name)'
	       (concat objc-interface-before
		       (objc-class-definition-name (concat class element)))))))
	;;
	(t
	 ;; regulare feature tag
	 (setq signature (regexp-quote signature))
	 (if (string-match objc-tag-fields-regexp signature)
	     ;;
	     ;; We must leave the class name as an optional component at the
	     ;; start of the signature so that functions that lookup feature
	     ;; definitions can use it to ensure that the definition is found
	     ;; within the right class.
	     (setq signature
		   (concat
		    "\\(" (substring signature (match-beginning 1)
				     (match-end 1))
		    objc-type-tag-separator "\\)?"
		    (substring signature (match-end 0)))))
	 (let ((pat) (i 0) (c) (len (length signature)))
	   (while (< i len)
	     (setq c (aref signature i)
		   pat (cond ((= c ? )
			      (concat pat "[ \t\n\^M]*"))
			     (t
			      (concat pat (char-to-string c))))
		   i (1+ i)))
	   (if (= ?{ (aref pat (1- (length pat))))
	       (setq pat (concat (substring pat 0 -1)
				 "\\([ \t\n]*//.*[\n]\\)*[ \t\n]*{"))
	     pat)))))

(defun objc-feature-tree-command-p (class-or-signature)
  "Display definition of CLASS-OR-SIGNATURE if a signature and return t, else return nil."
  (if (br-in-browser) (br-to-view-window))
  (br-feature-found-p (br-feature-file class-or-signature)
		      class-or-signature))

(defun objc-list-categories (class)
  "Return sorted list of Objective-C CLASS categories."
  (let ((obuf (current-buffer))
	(categories)
	class-tag)
    (cond ((string-equal class objc-default-category-class)
	   (objc-list-features class))
	  ((= (aref class 0) ?\[)
	   ;; Any other default classes belong to no categories.
	   nil)
	  (t
	   (setq class-tag (concat "\n" class objc-type-tag-separator "\("))
	   (set-buffer
	    (funcall br-find-file-noselect-function br-feature-tags-file))
	   (goto-char 1)
	   ;; Use a string match for speed.
	   (while (search-forward class-tag nil t)
	     (setq categories (cons (br-feature-current) categories)))
	   (set-buffer obuf)
	   (objc-sort-features (nreverse categories))))))

(defun objc-list-features (class &optional indent)
  "Return sorted list of Objective-C features lexically defined in CLASS."
  (let ((obuf (current-buffer))
	(features)
	class-tag
	search-function)
    (if (= (aref class 0) ?\[)
	;; Default class of protocols or categories.  Use a string match
	;; for speed.
	(setq search-function 'search-forward
	      class-tag (concat "\n" class objc-type-tag-separator))
      (setq search-function 're-search-forward
	    class-tag
	    ;; Include methods defined in any of the class' categories.
	    (concat "^" class "\\(([^\)]*)\\)?" objc-type-tag-separator)))
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    (while (funcall search-function class-tag nil t)
      (setq features (cons (br-feature-current) features)))
    (set-buffer obuf)
    (objc-sort-features (nreverse features))))

(defun objc-list-protocols (class)
  "Return sorted list of Objective-C CLASS protocols."
  (let ((obuf (current-buffer))
	(protocols)
	class-tag)
    (cond ((string-equal class objc-default-protocol-class)
	   (objc-list-features class))
	  ((= (aref class 0) ?\[)
	   ;; Any other default classes conform to no formal protocols.
	   nil)
	  (t
	   (setq class-tag (concat "\n" class objc-type-tag-separator "\<"))
	   (set-buffer
	    (funcall br-find-file-noselect-function br-feature-tags-file))
	   (goto-char 1)
	   ;; Use a string match for speed.
	   (while (search-forward class-tag nil t)
	     (setq protocols (cons (br-feature-current) protocols)))
	   (set-buffer obuf)
	   (objc-sort-features (nreverse protocols))))))

(defun objc-routine-at-point-p ()
  "Returns name of Objective-C routine signature at point or nil.
If called interactively, it prints the name in the minibuffer."
  (interactive)
  (save-excursion
    (if (and (re-search-backward "[-+\n\^M]\\|\\`" nil t)
	     (looking-at "[ \t\n\^M]*[-+]"))
	(let ((name "") (loop-p t)
	      (name-part (concat objc-name-part objc-name-sep))
	      (name-type (concat objc-name-part objc-type-sep)))
	  (goto-char (match-end 0))
	  (while (and loop-p (looking-at name-part))
	    ;; Handles named or unamed parameters.
	    (if (match-beginning objc-name-part-id)
		(setq name (concat name
				   (buffer-substring
				     (match-beginning objc-name-part-id)
				     (match-end objc-name-part-id)))))
	    (goto-char (match-end 0))
	    (if (/= (preceding-char) ?:)
		(setq loop-p nil)
	      (setq name (concat name ":"))
	      (if (looking-at name-type) (goto-char (match-end 0)))
	      ))
	  (if (interactive-p)
	      (message name)
	    name)))))

(defun objc-scan-features ()
  "Return reverse ordered list of Objective-C routine definitions in current buffer.
Assume point is at beginning of widened buffer."
  (save-excursion
    (let ((routines) (rout) (class-end)
	  class category)
      (while (re-search-forward
	      (concat "^@implementation[ \t\n\r]+" objc-identifier
		      "\\([ \t\n\r]*([ \t\n\r]*" objc-identifier
		      "[ \t\n\r]*)\\)?")
	      nil t)
	(setq category (if (match-beginning 3)
			   (buffer-substring (match-beginning 3)
					     (match-end 3)))
	      class (buffer-substring (match-beginning 1) (match-end 1))
	      class (if category (format "%s(%s)" class category) class))
	(save-excursion
	  (if (search-forward "\n@end" nil t)
	      (setq class-end (point))
	    (error "(objc-scan-features): %s, at char %d, @implementation without @end.")))
	(while (re-search-forward objc-routine-def class-end t)
	  (setq rout (buffer-substring (match-beginning 0)
				       (match-end 0)))
	  (if (c-within-comment-p)
	      (search-forward "*/" nil t)
	    (backward-char) ;; Move point to precede feature opening brace.
	    (condition-case ()
		;; Move to end of feature but ignore any error if braces are
		;; unbalanced.  Let the compiler tell the user about this.
		(forward-sexp)
	      (error nil))
	    (setq rout (objc-feature-normalize rout class)
		  routines (cons rout routines)))))
      routines)))

(defun objc-scan-protocol-list ()
  "Return a list of protocol names following point, delimited by <> and separated by commas.
Point may be immediately before or after the '<' which begins the protocol
list.  Leaves point afer the closing delimiter of the protocol list."
  (cond ((= (preceding-char) ?\<))
	((= (following-char) ?\<)
	 (forward-char 1))
	(t
	 (error "(objc-scan-protocol-list): Point must precede or follow a '<' delimiter.")))
      (let ((end (save-excursion (search-forward "\>")))
	    (protocols))
	(while (re-search-forward objc-identifier end t)
	  (setq protocols (cons (concat "<"
					(buffer-substring (match-beginning 1)
							  (match-end 1))
					">")
				protocols)))
	(goto-char end)
	(nreverse protocols)))

(defun objc-sort-features (routine-list)
  (sort routine-list 'objc-feature-lessp))

(defun objc-to-definition (&optional other-win)
  "If point is within a declaration, try to move to its definition.
With OTHER-WIN non-nil, show it in another window."
  (interactive)
  (let ((opoint (point)))
    (cond
     ((objc-include-file other-win))
     ((br-check-for-class (objc-class-decl-p) other-win))
     ((objc-feature other-win))
     ((and (goto-char opoint)
	   (br-check-for-class (objc-find-class-name) other-win)))
     (t	(beep)
	(message
	 "(OO-Browser):  Select an Objective-C declaration to move to its definition.")
	nil))))

(defun objc-view-protocol (protocol-name)
  "Display definition of PROTOCOL-NAME for viewing.
PROTOCOL-NAME must be a string."
  (or (string-match "<.*>" protocol-name)
      (setq protocol-name (concat "<" protocol-name ">")))
  (let* ((sig (concat objc-default-protocol-class objc-type-tag-separator
		      protocol-name))
	 (feature-path (br-feature-file sig)))
    (br-to-view-window)
    (if (br-feature-found-p feature-path sig)
	(progn (br-major-mode)
	       (setq buffer-read-only t)
	       ;; Force mode-line redisplay
	       (set-buffer-modified-p (buffer-modified-p))
	       (br-to-from-viewer))
      ;; Protocol not found.  Return to original window and signal an error.
      (br-to-from-viewer)
      (error "(OO-Browser):  No '%s' protocol defined in Environment."
	     protocol-name))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun objc-class-decl-p ()
  "Return nil unless point is within a class declaration, referenced by another
class.  Commented declarations also return nil.  When value is non-nil, it is
the class name from the declaration.  Leave point at start of statement for
visual clarity."
  (objc-skip-to-statement)
  (save-excursion
    (let ((class))
      (and (looking-at objc-class-decl)
	   (setq class (buffer-substring (match-beginning objc-class-name-grpn)
					 (match-end objc-class-name-grpn)))
	   (not (c-within-comment-p))
	   (progn (beginning-of-line)
		  (not (looking-at "[ \t]*//")))
	   class))))

(defun objc-feature (&optional other-win)
  "Move point to definition of the element given by declaration at point.
Return nil if point is not within an element declaration."
  ;; If '{' follows the feature declaration, then feature is defined right
  ;; here, within the class definition.
  (interactive)
  (cond ((objc-feature-def-p)
	 (recenter 0)
	 t)
	;; Now look for feature definition in code (non-header) files.
	((objc-feature-decl-p)
	 (let ((class) feature-name signature)
	   (setq signature (buffer-substring (match-beginning 1)
					     (match-end 1)))
	   (save-excursion
	     (if (re-search-backward objc-class-def-regexp nil t)
		 (setq class (buffer-substring
			      (match-beginning objc-class-name-grpn) 
			      (match-end objc-class-name-grpn)))))
	   (setq signature (objc-feature-normalize signature class)
		 feature-name (objc-feature-signature-to-name signature))
	   (if (objc-locate-feature feature-name class signature other-win)
	       t
	     (beep)
	     (message "(OO-Browser):  No definition for '%s' in '%s'."
		      feature-name (or class "UNKNOWN-CLASS"))
	     t)))))

(defun objc-feature-decl-p ()
  "Return t if point is within an Objective-C feature declaration."
  (save-excursion
    (beginning-of-line)
    (looking-at objc-feature-declaration)))

(defun objc-feature-def-p ()
  "Return nil unless point is within an element definition.
Commented element definitions also return nil."
  (save-excursion
    (objc-skip-to-statement)
    (and (not (c-within-comment-p))
	 (save-excursion (beginning-of-line)
			 (not (looking-at "[ \t]*//")))
	 (not (looking-at objc-class-decl))
	 (looking-at (concat objc-feature-decl-or-def
			     objc-comment-regexp "[{;,]"))
	 (= ?\{ (save-excursion (goto-char (match-end 0))
				(preceding-char))))))

(defun objc-feature-partial-name (feature-tag)
  "Extract the feature name without its class name from FEATURE-TAG."
  (objc-feature-signature-to-name feature-tag))

(defun objc-feature-lessp (tag1 tag2)
  (string-lessp (objc-feature-partial-name tag1)
		(objc-feature-partial-name tag2)))

(defun objc-feature-matches (name)
  "Return an unsorted list of feature tags whose names match in whole to NAME."
  ;; Ensure match to feature names only.
  (let ((regexp (format "^[^%s \n]+%s%s %s%s" objc-type-tag-separator
			objc-type-tag-separator br-feature-type-regexp
			(regexp-quote name) objc-type-tag-separator))
	(features))
    (save-excursion
      (set-buffer
       (funcall br-find-file-noselect-function br-feature-tags-file))
      (goto-char 1)
      (while (re-search-forward regexp nil t)
	(save-excursion
	  (setq features (cons (br-feature-current) features))))
      features)))

(defun objc-feature-normalize (routine class)
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
    (setq normal-feature (substring normal-feature 0 n))
    (concat class objc-type-tag-separator
	    (objc-feature-signature-to-name normal-feature nil t)
	    objc-type-tag-separator
	    normal-feature)))

(defun objc-feature-tag-class (feature-signature)
  "Extract the class name from FEATURE-SIGNATURE."
    (if (string-match objc-type-tag-separator feature-signature)
	(substring feature-signature 0 (match-beginning 0))
      ""))

(defun objc-feature-tags-lookup (class-list signature ftr-regexp &optional other-win)
  "Display routine definition derived from CLASS-LIST, matching SIGNATURE (string) and FTR-REGEXP (regexp matching SIGNATURE).
Use routine tags table to locate a match.  Caller must use 'set-buffer'
to restore prior buffer when a match is not found."
  (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
  (let  ((classes class-list)
	 (found-ftr)
	 (class))
    (if (null class-list)
	nil
      (while (and (not found-ftr) classes)
	(setq class (car classes)
	      found-ftr (br-feature-found-p
			 (br-feature-file signature)
			 ftr-regexp nil other-win t)
	      classes (if found-ftr nil (cdr classes))))
      (if found-ftr
	  (or class t)
	(objc-feature-tags-lookup
	 (apply 'append (mapcar (function (lambda (cl) (br-get-parents cl)))
				class-list))
	 signature
	 ftr-regexp
	 other-win)))))

(defun objc-files-with-source (class)
  "Use CLASS to compute set of files that match to an Objective-C source file regexp.
Return as a list."
  (let ((file (if class (br-class-path class) buffer-file-name)))
    (and file
	 (let* ((src-file-regexp (concat "^" (br-filename-head file)
					 objc-code-file-regexp))
		(dir (file-name-directory file))
		(files (directory-files dir nil src-file-regexp)))
	   (mapcar (function (lambda (f) (concat dir f)))
		   files)))))

(defun objc-find-ancestors-feature (class-list signature &optional other-win)
  "Scan ancestors of CLASS-LIST and show routine definition matching SIGNATURE."
  ;; If no class, search for non-element function.
  (or class-list (setq class-list '(nil)))
  (let ((obuf (current-buffer))
	(ftr-regexp (objc-feature-signature-to-regexp signature)))
    (prog1
	(if (and br-feature-tags-file
		 (file-exists-p br-feature-tags-file)
		 (file-readable-p br-feature-tags-file))
	    (objc-feature-tags-lookup
	     class-list signature ftr-regexp other-win)
	  ;; Only works if features are in same directory as class def.
	  (objc-scan-ancestors-feature class-list ftr-regexp other-win))
      (set-buffer obuf))))

(defun objc-find-class-name ()
  "Return current word as a potential class name."
  (save-excursion
    (let* ((start)
	   (ignore "-+ \t\n;,.<>{}*&\(\)")
	   (pat (concat "^" ignore)))
      (forward-char 1)
      (skip-chars-backward ignore)
      (skip-chars-backward pat)
      (setq start (point))
      (skip-chars-forward (concat pat ":"))
      (buffer-substring start (point)))))

(defun objc-get-class-name-from-source ()
  "Return class name from closest class definition preceding point or nil."
  (let ((opoint (point))
	(class))
    (save-excursion
      (if (re-search-backward objc-class-def-regexp nil t)
	  (progn (setq class (buffer-substring
			      (match-beginning objc-class-name-grpn)
			      (match-end objc-class-name-grpn)))
		 ;; Ensure that declaration occurs within class definition.
		 (forward-list)
		 (and (> (point) opoint) class))))))

(defun objc-get-feature-tags (routine-file &optional routine-list)
  "Scan Objective-C ROUTINE-FILE and hold routine tags in 'br-feature-tags-file'.
Assume ROUTINE-FILE has already been read into a buffer and that
'br-feature-tags-init' has been called.  Optional ROUTINE-LIST can be
provided so that a non-standard scan function can be used before calling
this function."
  (interactive)
  (let ((obuf (current-buffer)))
    (setq routine-list
	  (objc-sort-features
	   (or routine-list (objc-scan-features))))
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
		       routine-list)))
    (set-buffer obuf)))

(defun objc-include-file (&optional other-win)
  "If point is on an include file line, try to display file.
Return non-nil iff an include file line, even if file is not found.
Look for include file in 'objc-cpp-include-dirs' and in directory list
'objc-include-dirs'."
  (let ((opoint (point)))
    (beginning-of-line)
    (if (looking-at objc-include-regexp)
	(let ((incl-type (string-to-char
			  (buffer-substring
			   (match-beginning objc-include-type-grpn)
			   (1+ (match-beginning objc-include-type-grpn)))))
	      (file (buffer-substring
		     (match-beginning objc-include-file-grpn)
		     (match-end objc-include-file-grpn)))
	      (path)
	      (dir-list objc-include-dirs)
	      (found))
	  (goto-char opoint)
	  (setq dir-list (if (= incl-type ?\<)
			     (append dir-list objc-cpp-include-dirs)
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
			(objc-mode-setup))
		    (br-major-mode))
		(beep)
		(message "(OO-Browser):  Include file '%s' unreadable." path))
	    (beep)
	    (message "(OO-Browser):  Include file '%s' not found." file))
	  path)
      (goto-char opoint)
      nil)))

(defun objc-locate-feature (ftr class signature &optional other-win)
  ;; 'class' may = nil, implying non-element function
  (let ((def-class))
    (if (and signature
	     (setq def-class
		   (objc-find-ancestors-feature (list class)
						signature other-win)))
	(progn (if (and class (not (equal class def-class)))
		   (message
		     "Element `%s` of class '%s' inherited from class '%s'."
		     ftr class def-class))
	       t))))

(defun objc-scan-ancestors-feature (class-list ftr-regexp &optional other-win)
  "Display routine definition derived from CLASS-LIST, matching FTR-REGEXP.
Scan files with same base name as class file."
  (let  ((classes class-list)
	 (found-ftr)
	 (code-def-files)
	 (file)
	 (class))
    (if (null class-list)
	nil
      (while (and (not found-ftr) classes)
	(setq class (car classes)
	      code-def-files (objc-files-with-source class))
	(while (and (setq file (car code-def-files))
		    (not (setq found-ftr
			       (br-feature-found-p file ftr-regexp
					       nil other-win t))))
	  (setq code-def-files (cdr code-def-files)))
	(setq classes (if found-ftr nil (cdr classes))))
      (if found-ftr
	  (or class t)
	(objc-scan-ancestors-feature
	 (apply 'append (mapcar (function (lambda (cl) (br-get-parents cl)))
				class-list))
	 ftr-regexp)))))

(defun objc-skip-past-comments ()
  "Skip over comments immediately following point."
  (skip-chars-forward " \t\n")
  (while
      (cond ((looking-at "//")
	     (equal (forward-line 1) 0))
	    ((looking-at "/\\*")
	     (re-search-forward "\\*/" nil t))
	    (t nil))))

(defun objc-skip-to-statement ()
  (if (re-search-backward "\\(^\\|[;{}]\\)[ \t]*" nil t)
      (progn (goto-char (match-end 0))
	     (skip-chars-forward " \t")
	     t)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst objc-code-file-regexp ".\\.[cmCM]$"
  "Regular expression matching a unique part of Objective-C source (non-header) file name and no others.")

(defconst objc-include-regexp
  "[ \t/*]*#[ \t]*\\(import\\|include\\)[ \t]+\\([\"<]\\)\\([^\">]+\\)[\">]"
  "Regexp to match to Objective-C include file lines.
File name is grouping 'objc-include-file-grpn'.  Type of include,
user-specified via double quote, or system-related starting with `<' is given
by grouping 'objc-include-type-grpn'.")

(defconst objc-include-type-grpn 2)
(defconst objc-include-file-grpn 3)

(defconst objc-type-def-modifier
  "\\(auto\\|const\\|inline\\|register\\|static\\|typedef\\)")

(defconst objc-func-identifier (concat
			       "[_a-zA-Z][^][ \t:;.,~{}()]*")
  "Regular expression matching an Objective-C function name.")

(defconst objc-feature-decl-or-def
  "[-+]\\([^\]\[{};`'\"/|?,!.#$%^=+-]+\\)"
  "Regexp matching an Objective-C feature declaration or definition.
Feature name is group 1.")

(defconst objc-feature-name-grpn 1)

(defconst objc-comment-regexp "\\([ \t\n]*//.*[\n]\\)*[ \t\n]*")

(defconst objc-routine-def (concat "^" objc-feature-decl-or-def
				   objc-comment-regexp "{"))

(defconst objc-feature-declaration
  (concat "^[ \t]*\\(" objc-feature-decl-or-def "\\)" objc-comment-regexp ";"))

(defconst objc-class-decl
  (concat objc-class-name-before objc-identifier "[ \t]*")
  "Regexp matching an Objective-C class declaration.
Class name is grouping 'objc-class-name-grpn'.")

(defconst objc-class-name-grpn 2)

(defconst objc-arg-identifier (concat
			      "[_a-zA-Z][" objc-identifier-chars "]*")
  "Regular expression matching an Objective-C function argument identifier.")

(defconst objc-name-part-prefix
  "[ \t\n]*\\(([^\)]+)[ \t\n]*\\)?")

(defconst objc-name-part
  (concat objc-name-part-prefix objc-identifier "?"))

(defconst objc-name-sep "[ \t\n]*\\([:;{]\\)")

(defconst objc-type-sep "\\([ \t\n;{]\\)")

(defconst objc-name-part-type 1)
(defconst objc-name-part-id 2)
(defconst objc-name-part-sep 3)

(provide 'br-objc-ft)
