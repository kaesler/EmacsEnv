;;!emacs
;;
;; FILE:         br-eif-ft.el
;; SUMMARY:      Eiffel OO-Browser class and feature functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    03-Oct-90
;; LAST-MOD:     11-May-95 at 11:24:33 by Bob Weiner
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;; ************************************************************************
;; Other required Elisp libraries
;; ************************************************************************

(require 'eif-calls)

;; ************************************************************************
;; Public variables
;; ************************************************************************

(defconst eif-type-tag-separator ","
  "String that separates a tags type from its normalized definition form.")

;; ************************************************************************
;; Public functions
;; ************************************************************************

(defun eif-feature-implementors (ftr-name)
  "Return unsorted list of Eiffel feature tags which implement FTR-NAME."
  (eif-feature-matches (concat "^" (regexp-quote ftr-name) "$")))

(defun eif-feature-name-to-regexp (name)
  "Converts feature NAME into a regular expression matching the feature's name tag."
  (if (string-match (concat "^" br-feature-type-regexp " ") name)
      (setq name (substring name (match-end 0))))
  (format "%s%s%s %s[ \n]"
	  eif-identifier eif-type-tag-separator br-feature-type-regexp
	  (regexp-quote name)))

(fset 'eif-feature-signature-to-name 'eif-feature-partial-name)

(defun eif-feature-signature-to-regexp (signature)
  "Given an Eiffel class or feature SIGNATURE, return regexp to match its definition."
  (let ((regexp) class name type)
    (setq regexp
	  (cond ((string-match (concat eif-type-tag-separator
				       "\\(" br-feature-type-regexp "\\) ")
			       signature)
		 (setq name (substring signature (match-end 0))
		       type (string-to-char
			     (substring
			      signature (match-beginning 1) (match-end 1))))
		 (cond ((memq type '(?- ?1 ?>))
			;; routine
			(eif-routine-to-regexp name))
		       ((= type ?=)
			;; attribute
			(eif-attribute-to-regexp name))))
		((equal 0 (string-match eif-identifier signature))
		 ;; Assume is a class name
		 (concat eif-class-name-before (regexp-quote signature)
			 eif-class-name-after))))
    (or regexp
	(error "(eif-feature-signature-to-regexp): Invalid format, '%s'"
	       signature))))

(defun eif-feature-tree-command-p (class-or-signature)
  "Display definition of CLASS-OR-SIGNATURE if a signature and return t, else return nil."
  (if (br-in-browser) (br-to-view-window))
  (br-feature-found-p (br-feature-file class-or-signature)
		      class-or-signature))

(defun eif-list-features (class &optional indent)
  "Return sorted list of Eiffel feature names lexically defined in CLASS."
  (let ((class-tag (concat "\n" class eif-type-tag-separator))
	(features) start end)
    (save-excursion
      (set-buffer
       (funcall br-find-file-noselect-function br-feature-tags-file))
      (goto-char 1)
      (if (not (search-forward class-tag nil t))
	  nil
	(setq start (match-beginning 0)
	      end (if (search-forward "\^L\n" nil t)
		      (match-beginning 0)
		    (point-max)))
	(goto-char start)
	;; Feature defs can occur only within a single file.
	(while (search-forward class-tag end t)
	  (setq features (cons (br-feature-current) features)))
	(eif-sort-features features)))))

(defun eif-get-feature-tags (feature-file feature-list)
  "Save Eiffel feature tags defined in FEATURE-FILE to 'br-feature-tags-file'.
Assume FEATURE-FILE has already been read into a buffer and that
'br-feature-tags-init' has been called.  FEATURE-LIST is the list
of tags to save."
  (interactive)
  (let ((obuf (current-buffer)))
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

(defun eif-scan-features-in-class (class start end)
  "Return unordered list of Eiffel feature definitions in CLASS.
START and END give buffer region to search."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let ((attributes-and-routines (eif-parse-features t)))
	(append
	 (mapcar
	  (function (lambda (routine)
		      (concat class eif-type-tag-separator routine)))
	  (cdr attributes-and-routines))
	 (mapcar
	  (function (lambda (attribute)
		      (concat class eif-type-tag-separator attribute)))
	  (car attributes-and-routines)))))))

(defun eif-sort-features (feature-list)
  (sort feature-list 'eif-feature-lessp))

(defun eif-to-definition (&optional identifier)
  "If point is within an Eiffel class or feature name, try to move to its definition.
With optional IDENTIFIER, do the same instead for it."
  (interactive)
  (let ((cl (or identifier (eif-find-class-name))))
    (cond
     ((eif-keyword-p) nil)
     ((br-check-for-class cl))
     ((eif-feature cl))
     ((progn
	(beep)
	(message
	 "(OO-Browser):  Select an Eiffel identifier to move to its definition.")
	nil))
     )))

;; ************************************************************************
;; Private functions
;; ************************************************************************

(defun eif-export-feature-p ()
  "Return nil unless point is within a class export clause."
  (save-excursion
    (let ((end (point)))
      (beginning-of-line)
      ;; If in a comment, return nil.
      (if (search-forward "--" end t)
	  nil
	(goto-char (point-min))
	(and (re-search-forward eif-export-key-regexp end t)
	     (not (re-search-forward "^\\(inherit\\|feature\\)\\([ \t]\\|$\\)" end t)))))))

(defun eif-feature (&optional ftr)
  "Return nil if definition is not found for optional FTR or feature declared at point."
  (interactive)
  (let ((class-deferred)
	(class)
	(deferred-p)
	(ftr-def-class))
    (cond ((or ftr (and (eif-export-feature-p)
			(setq ftr (eif-to-feature-decl))))
	   (if (and (setq class-deferred (eif-get-class-name-from-source))
		    (setq class (car class-deferred)
			  deferred-p (cdr class-deferred)
			  ftr-def-class (eif-find-ancestors-feature
					 (list class) deferred-p ftr)))
	       (cond ((equal (car ftr-def-class) class) t)
		     ((equal (cdr ftr-def-class) ftr)
		      ;; Feature inherited but not renamed.
		      (message
		       "Feature '%s' of class '%s' inherited from class '%s'."
		       ftr class (car ftr-def-class)))
		     ;; Feature inherited and renamed.
		     (t (message "Feature '%s', class '%s' from feature '%s', class '%s'."
				 ftr class (cdr ftr-def-class)
				 (car ftr-def-class))
			t))
	     (beep)
	     (message "(OO-Browser):  '%s' feature not found." ftr)
	     t))
	  ((and (not ftr) (eif-feature-def-p)))
	  ;;
	  ;; Later we might add the case of a feature invocation here.
	  ;;
	  )))

(defun eif-feature-def-p ()
  "If point is within a feature definition's name, display feature including leading comments."
  (let ((opoint (point)))
    (beginning-of-line)
    (if (or (looking-at eif-routine-regexp)
	    (looking-at eif-attribute-regexp))
	(progn (setq opoint (match-beginning eif-feature-name-grpn))
	       (eif-to-comments-begin)
	       (recenter 0)
	       (goto-char opoint)
	       t)
      (goto-char opoint)
      nil)))

(defun eif-feature-matches (regexp)
  "Return an unsorted list of feature tags whose names match in part or whole to REGEXP."
  ;; Ensure match to feature names only; also handle "^" and "$" meta-chars
  (setq regexp
	(concat "^\\(" eif-identifier "\\)"
		eif-type-tag-separator
		br-feature-type-regexp " "
		(if (equal (substring regexp 0 1) "^")
		    (progn (setq regexp (substring regexp 1)) nil)
		  (concat "[" eif-identifier-chars "]*"))
		(if (equal (substring regexp -1) "$")
		    (substring regexp 0 -1)
		  (concat regexp "[" eif-identifier-chars "]*"))
		"[ \t\n\r]"))
  (save-excursion
    (set-buffer (funcall br-find-file-noselect-function br-feature-tags-file))
    (goto-char 1)
    (let ((features) start end)
      (if (not (re-search-forward regexp nil t))
	  nil
	(setq start (match-beginning 0)
	      end (if (search-forward "\^L\n" nil t)
		      (match-beginning 0)
		    (point-max)))
	(goto-char start)
	;; Feature defs can occur only within a single file.
	(while (re-search-forward regexp end t)
	  (backward-char) ;; Might have moved past newline.
	  (setq features (cons (br-feature-current) features))))
      features)))

(defun eif-feature-lessp (feature1 feature2)
  (string-lessp (eif-feature-partial-name feature1)
		(eif-feature-partial-name feature2)))

(defun eif-feature-partial-name (signature &optional with-class for-display)
  "Extract the feature name without its class name from feature SIGNATURE.
If optional WITH-CLASS is non-nil, class name and 'eif-type-tag-separator'
are prepended to the name returned.  If optional FOR-DISPLAY is non-nil, a
feature type character is prepended to the name for display in a browser
listing."
  (if (string-match (concat eif-type-tag-separator
			    "\\(" br-feature-type-regexp " \\)")
		    signature)
      (let ((class (substring signature 0 (match-beginning 0)))
	    (name (substring signature (match-end 0))))
	(cond ((and with-class for-display)
	       signature)
	      (with-class
	       (concat class eif-type-tag-separator name))
	      (for-display
	       (substring signature (match-beginning 1)))
	      (t name)))
    signature))

(defun eif-feature-tag-class (element-tag)
  "Extract the class name from ELEMENT-TAG."
  (if (string-match eif-type-tag-separator element-tag)
      (substring element-tag 0 (match-beginning 0))
    ""))

(defun eif-find-ancestors-feature (class-list deferred-class ftr)
  (let* ((classes class-list)
	 (cl)
	 (file)
	 (found-ftr))
    (if (null class-list)
	nil
      (while (and (not found-ftr) classes)
	(setq cl (car classes)
	      file (br-class-path cl))
	(and file (setq found-ftr
			(br-feature-found-p file ftr deferred-class)))
	;; If found-ftr is a cons cell, then only one parent class need
	;; be searched to look for ftr.
	(if (consp found-ftr)
	    (setq class-list (list (car found-ftr))
		  ftr (cdr found-ftr)))
	(setq classes (cdr classes)))
      (cond ((consp found-ftr)
	     (eif-find-ancestors-feature class-list deferred-class ftr))
	    ((null found-ftr)
	     (eif-find-ancestors-feature 
	      (apply 'append (mapcar (function
				       (lambda (cl) (br-get-parents cl)))
				     class-list))
	      deferred-class
	      ftr))
	    (t (cons cl ftr))))))

;; Prefixed with 'eiffel' rather than 'eif' since works as a standalone
;; feature in buffers whose major mode is 'eiffel-mode'.  It is used by the
;; browser but may also be used standalone.
;;
(defun eiffel-find-feature (feature-name)
  "Move point to start of feature named FEATURE-NAME in current buffer.
Display feature including all preceding comments at the top of the window.
Move point and return non-nil iff FEATURE-NAME is found."
  (interactive "sFeature to find: ")
  (cond ((eif-locate-feature
	  feature-name (eif-routine-to-regexp feature-name)))
	((eif-to-attribute feature-name)
	 (let ((opoint (point)))
	   (eif-to-comments-begin)
	   (recenter 0)
	   (goto-char opoint)
	   (back-to-indentation)
	   t))))

(defun eif-find-class-name ()
  "Return class name that point is within, else nil."
  (if (= (point) (point-max)) (skip-chars-backward " \t\n"))
  (save-excursion
    (skip-chars-forward " \t")
    (skip-chars-backward eif-identifier-chars)
    (skip-chars-backward " \t\n")
    (backward-char 1)
    (and (looking-at eif-class-name-pat)
	 (eif-set-case
	  (buffer-substring (match-beginning 2)
			    (match-end 2))))))

(defun eif-find-feature (feature-name)
  "With point selecting a class in a listing buffer, move point to definition of FEATURE-NAME in viewer window.
Move point and return non-nil iff FEATURE-NAME is found."
  (interactive "sFeature to find: ")
  ;; If selected class is displayed, don't go to start of class
  (if (equal (br-class-path (br-find-class-name))
	     (progn
	       (br-to-from-viewer)
	       (expand-file-name buffer-file-name)))
      nil
    (br-edit))
  (if (eiffel-find-feature feature-name)
      (progn (recenter 0)
	     t)
    (br-to-from-viewer)
    (and (interactive-p)
	 (progn
	   (beep)
	   (message "(OO-Browser):  No '%s' feature found." feature-name)))))

(defun eif-feature-locate-p (feature-tag)
  (let (start class)
    (if (string-match (concat "\\`[^\]\[]+" eif-type-tag-separator)
		      feature-tag)
	;; First move to the proper class implementation, so that if two
	;; classes in the same file have the same feature signature, we still
	;; end up at the right one.
	(progn
	  (setq class (substring feature-tag 0 (1- (match-end 0))))
	  (re-search-forward
	   (concat eif-class-name-before (regexp-quote class)
		   eif-class-name-after)
	   nil t)))
    (if (not (re-search-forward
	      (eif-feature-signature-to-regexp feature-tag) nil t))
	nil
      (setq start (match-beginning 0))
      (goto-char start)
      (skip-chars-forward " \t\n")
      (eif-to-comments-begin)
      (recenter 0)
      (goto-char start)
      t)))

(defun eif-keyword-p ()
  "Return t if point is within an Eiffel keyword, else nil."
  (if (= (point) (point-max)) (skip-chars-backward " \t\n"))
  (save-excursion
    (skip-chars-forward " \t")
    (skip-chars-backward eif-identifier-chars)
    (and (looking-at eif-identifier)
	 (br-member-sorted-strings (buffer-substring (match-beginning 0)
						     (match-end 0))
				   eif-reserved-words))))

(defun eif-locate-feature (ftr ftr-pat)
  (let ((opoint (point)))
    (goto-char (point-min))
    (if (and (re-search-forward "^feature\\([ \t]\\|$\\)" nil t)
	     (re-search-forward ftr-pat nil t))
	(progn (goto-char (match-beginning eif-feature-name-grpn))
	       (setq opoint (point))
	       (eif-to-comments-begin)
	       (recenter 0)
	       (goto-char opoint)
	       t)
      (goto-char opoint)
      (and (interactive-p) (error (format "Feature '%s' not found."
					  ftr))))))

(defun eif-renamed-feature-p (ftr)
  (goto-char (point-min))
  (let ((rename-regexp "[ \t\n]+rename[ \t\n]")
	(rename-match
	 (concat eif-identifier "[ \t\n]+as[ \t\n]+" ftr "[,; \t\n]"))
	(prev-feature-nm)
	(prev-class)
	(parents))
    (while (and (setq prev-feature-nm
		      (and (re-search-forward rename-regexp nil t)
			   (re-search-forward rename-match nil t)))
		(setq prev-feature-nm
		      (buffer-substring (match-beginning 1) (match-end 1))
		      prev-class (match-beginning 0))
		(progn (backward-char 1)
		       (eif-in-comment-p))))
    (if prev-feature-nm
	(progn (goto-char prev-class)
	       (setq parents (eif-get-parents-from-source buffer-file-name))
	       (if (re-search-backward (concat
					"[^[][ \t\n]+\\("
					(mapconcat
					  (function (lambda (cl)
						      (eif-set-case-type cl)))
					  parents
					  "\\|")
					"\\)")
				       nil t)
		   (progn (setq prev-class (eif-set-case (buffer-substring
							  (match-beginning 1)
							  (match-end 1))))
			  (cons prev-class prev-feature-nm))
		 (beep)
		 (message
		  "(OO-Browser):  Internal error - no class associated with rename clause."))))))

(defun eif-to-feature-decl ()
  (let ((end))
    (while (and (progn (skip-chars-backward "^, \t\n")
		       (and (not (= (preceding-char) ?,))
			    (not (looking-at "export[ \t\n]+"))))
		(progn (skip-chars-backward " \t\n")
		       (setq end (point))
		       (beginning-of-line)
		       (if (search-forward "--" end t)
			   (progn (goto-char end)
				  (skip-chars-forward " \t\n")
				  nil)
			 (goto-char end)
			 t)))))
  (if (looking-at "export[ \t\n]+")
      (goto-char (match-end 0))
    (skip-chars-forward " \t\n"))
  (if (looking-at eif-feature-name)
      (buffer-substring (match-beginning 0) (match-end 0))))


;; ************************************************************************
;; Private variables
;; ************************************************************************

(defconst eif-feature-name
  (concat 
   "\\("
   "\\(prefix[ \t]+\"\\(not\\|\\+\\|-\\)\"\\)"
   "\\|infix[ \t]+\"\\(div\\|mod\\|^\\|<=?\\|>=?\\|\+\\|-\\|\\*\\|/"
                   "\\|and then\\|and\\|or else\\|or\\|xor\\|implies\\)"
   "\\|" eif-identifier "\\)")
  "Regexp matching any Eiffel feature name.
Will also match class names and keywords, so tests for these should precede
use of this expression.")

(defconst eif-export-key-regexp
  "\\(^[ \t]*\\|[ \t]+\\)export[ \t\n]+"
  "Regexp matching the Eiffel export keyword in context.")

(defconst eif-class-repeat (concat "repeat[ \t]+" eif-identifier)
  "Match to an Eiffel 'repeat <class>' phrase.  Grouping 1 is class name.")

(defconst eif-exported-feature
  (concat "\\(,\\|export[ \t\n]+\\(--.*[ \t\n]+\\)*\\)"
	  eif-feature-name "\\([ \t]*{[^}]+}\\)?"
	  "\\([ \t]*[\n,]\\|[ \t]+--\\)")
  "Regexp to match to a feature declaration in an export clause.
  Exclude 'repeat <class>' phrases.  Feature name is grouping 3.")


(provide 'br-eif-ft)
