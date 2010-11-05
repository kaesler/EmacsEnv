;;!emacs
;;
;; FILE:         br-lib.el
;; SUMMARY:      OO-Browser support functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    22-Mar-90
;; LAST-MOD:     21-Sep-95 at 14:30:36 by Bob Weiner
;;
;; Copyright (C) 1990-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-env br-ftr br-compl set))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar br-null-path "<none>"
  "Pathname associated with OO-Browser entities which have no source file.
That is, virtual entities, such as categories.")

;;; ************************************************************************
;;; General public functions
;;; ************************************************************************

(defun br-buffer-replace (regexp to-str)
  "In current buffer, replace all occurrences of REGEXP with TO-STR."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-str 'fixedcase nil)
    (backward-char 1)))

(defun br-delete-space (string)
  "Delete any leading and trailing space from STRING and return the STRING. "
  (if (string-match "\\`\\s *\\(\\(.\\|\n\\)*\\S \\)\\s *\\'" string)
      (setq string (substring string (match-beginning 1)
			      (match-end 1)))
    string))

(defun br-first-match (regexp list)
  "Return non-nil if REGEXP matches to an element of LIST.
All elements of LIST must be strings.
The value returned is the first matched element."
  (while (and list (not (string-match regexp (car list))))
    (setq list (cdr list)))
  (car list))

(defun br-filename-head (path)
  (setq path (file-name-nondirectory path))
  (if (string-match "\\(.+\\)\\." path)
      (substring path 0 (match-end 1))
    path))

(defun br-duplicate-and-unique-strings (sorted-strings)
  "Return SORTED-STRINGS list with a list of duplicate entries consed onto the front of the list."
  (let ((elt1) (elt2) (lst sorted-strings)
	(count 0) (repeat) (duplicates))
    (while (setq elt1 (car lst) elt2 (car (cdr lst)))
      (cond ((not (string-equal elt1 elt2))
	     (setq lst (cdr lst)))
	    ((equal elt1 repeat)
	    ;; Already recorded this duplicate.
	     (setcdr lst (cdr (cdr lst))))
	    (t ;; new duplicate
	     (setq count (1+ count)
		   duplicates (cons elt1 duplicates)
		   repeat elt1)
	     (setcdr lst (cdr (cdr lst))))))
    (cons (sort duplicates 'string-lessp) sorted-strings)))

(defun br-set-of-strings (sorted-strings &optional count)
  "Return SORTED-STRINGS list with any duplicate entries removed.
Optional COUNT conses number of duplicates on to front of list before return."
  (and count (setq count 0))
  (let ((elt1) (elt2) (lst sorted-strings)
	(test (if count
		  (function
		    (lambda (a b) (if (string-equal a b)
				      (setq count (1+ count)))))
		(function (lambda (a b) (string-equal a b))))))
    (while (setq elt1 (car lst) elt2 (car (cdr lst)))
      (if (funcall test elt1 elt2)
	  (setcdr lst (cdr (cdr lst)))
	(setq lst (cdr lst)))))
  (if count (cons count sorted-strings) sorted-strings))

(defun br-member-sorted-strings (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with 'string-equal'.
All ELTs must be strings and the list must be sorted in ascending order.
The value returned is actually the tail of LIST whose car is ELT."
  (while (and list (not (string-equal (car list) elt)))
    (setq list (and (string-lessp (car list) elt)
		    (cdr list))))
  list)

(defun br-pathname-head (path)
  (if (string-match "\\(.+\\)\\." path)
      (substring path 0 (match-end 1))
    path))

(defun br-quote-match (match-num)
  "Quote special symbols in last matched expression MATCH-NUM."
  (br-regexp-quote (buffer-substring (match-beginning match-num)
				     (match-end match-num))))

(defun br-rassoc (elt list)
  "Return non-nil if ELT is the cdr of an element of LIST.
Comparison done with 'equal'.  The value is actually the tail of LIST
starting at the element whose cdr is ELT."
  (while (and list (not (equal (cdr (car list)) elt)))
    (setq list (cdr list)))
  list)

(defun br-regexp-quote (obj)
  "If OBJ is a string, quote and return it for use in a regular expression."
  ;; Don't use (stringp obj) here since we want to signal an error if some
  ;; caller ever passes in a non-nil, non-string object, to aid in debugging.
  (if obj (regexp-quote obj)))

(defun br-relative-path (filename &optional directory)
  "Convert FILENAME to be relative to DIRECTORY or default-directory.
The shorter of the absolute and relative paths is returned."
  (let ((relative-path (file-relative-name filename directory)))
    (if (< (length relative-path) (length filename))
	relative-path
      filename)))

(defmacro br-set-cons (set elt)
  "Add to SET element ELT.  Returns nil iff ELT is already in SET.
Uses 'equal' for comparison."
  (` (if (br-member (, elt) (, set))
	 nil
       (setq (, set) (cons (, elt) (, set))))))


(defun br-wind-line-at-point ()
  "Return window relative line number that point is on."
  (max 0 (1- (- (count-lines 1 (1+ (point)))
		(count-lines 1 (window-start))))))

;;; ************************************************************************
;;; Browser public functions
;;; ************************************************************************

(defun br-add-class (class-name &optional class-path lib-table-p save-file)
  "Add or replace CLASS-NAME in current Environment.
  Find class source in optional CLASS-PATH.  Interactively or when optional
CLASS-PATH is nil, defaults to current buffer file as CLASS-PATH.  If
optional LIB-TABLE-P is non-nil, add to Library Environment, otherwise add to
System Environment.  If optional SAVE-FILE is t, the Environment is then
stored to filename given by 'br-env-file'.  If SAVE-FILE is non-nil and
not t, its string value is used as the file to which to save the Environment.
Does not update children lookup table."
  (interactive
    (list (read-string "Class name to add: ")
	  (read-file-name (concat "Class file name"
				  (if buffer-file-name
				      " (default <current file>)")
				  ": ")
			  nil buffer-file-name t)
	  (y-or-n-p "Add to Library, rather than System tables? ")
	  (y-or-n-p
	    (concat "Save tables after addition to " br-env-file "? "))))
  ;; 
  ;; Pseudo code:
  ;; 
  ;;    If class-name is in table
  ;;       If function called interactively
  ;;          Query whether should overwrite class-name in tables
  ;;          If yes
  ;;             Replace entry
  ;;          else
  ;;             Don't add class; do nothing
  ;;          end
  ;;       else
  ;;          Store class in all necessary tables
  ;;       end
  ;;    else
  ;;       Store class under key in all necessary tables
  ;;    end
  ;;
  (or class-path (setq class-path buffer-file-name)
      (error "No class pathname specified."))
  (if (or (string-equal class-name "")
	  (not (or (equal class-path br-null-path)
		   (file-exists-p class-path))))
      (error (format "Invalid class specified, '%s', in: %s" class-name class-path)))
  ;; Is class already in Environment?
  (if (hash-key-p class-name (br-get-htable
			       (if lib-table-p "lib-parents" "sys-parents")))
      (if (interactive-p)
	  (if (y-or-n-p (format "Overwrite existing '%s' entry? " class-name))
	      (br-real-add-class lib-table-p class-name class-path 'replace)
	    (setq save-file nil))
	(br-real-add-class lib-table-p class-name class-path))
    (br-real-add-class lib-table-p class-name class-path))
  (cond ((eq save-file nil))
	((eq save-file t) (br-env-save))
	((br-env-save save-file))))

(defun br-build-lib-htable ()
  "Build Library dependent Environment."
  (interactive)
  (cond	((and (interactive-p)
	       (not (y-or-n-p "Rebuild Library Environment? ")))
	 nil)
	(t
	 (message "Building Library Environment...")
	 (sit-for 2)
	 (br-real-build-alists br-lib-search-dirs)
	 (setq br-lib-paths-htable (hash-make br-paths-alist)
	       br-lib-parents-htable (hash-make br-parents-alist))
	 (run-hooks 'br-after-build-lib-hook)
	 (br-env-set-htables)
	 (br-build-children-htable)
	 ;; Set prev-search-dirs so table rebuilds are not triggered.
	 (setq br-lib-prev-search-dirs br-lib-search-dirs)
	 (if (interactive-p) (br-env-save))
	 (message "Building Library Environment...Done")
	 t)))

(defun br-build-sys-htable ()
  "Build System dependent class Environment."
  (interactive)
  (cond	((and (interactive-p)
	      (not (y-or-n-p "Rebuild System Environment? ")))
	 nil)
	(t
	 (message "Building System Environment...")
	 (sit-for 2)
	 (br-real-build-alists br-sys-search-dirs)
	 (setq br-sys-paths-htable (hash-make br-paths-alist)
	       br-sys-parents-htable (hash-make br-parents-alist))
	 (run-hooks 'br-after-build-sys-hook)
	 (br-env-set-htables)
	 (br-build-children-htable)
	 ;; Set prev-search-dirs so table rebuilds are not triggered.
	 (setq br-sys-prev-search-dirs br-sys-search-dirs)
	 (if (interactive-p) (br-env-save))
	 (message "Building System Environment...Done")
	 t)))

(defun br-class-in-table-p (class-name)
  "Return t iff CLASS-NAME is found in current Environment."
  (interactive (list (br-complete-class-name)))
  (if class-name (hash-key-p class-name (br-get-parents-htable))))

(defun br-class-path (class-name &optional insert)
  "Return full path, if any, to CLASS-NAME.
With optional prefix argument INSERT non-nil, insert path at point.
Only the first matching class is returned, so each CLASS-NAME should be
unique. Set 'br-lib/sys-search-dirs' properly before use."
  (interactive (list (br-complete-class-name)))
  (setq class-name (if class-name (br-set-case class-name)))
  (let* ((class-path)
	 (class-htable (br-get-paths-htable)))
    (hash-map
      (function (lambda (val-key-cons)
		  (and (null class-path)
		       (br-member-sorted-strings class-name (car val-key-cons))
		       (setq class-path (br-select-path val-key-cons nil)))))
      class-htable)
    (if (equal class-path br-null-path)
	(setq class-path nil))
    (and (interactive-p) (setq insert current-prefix-arg))
    (if (and insert class-path)
	(insert class-path)
      (if (interactive-p)
	  (message
	   (or class-path
	       (format
		"(OO-Browser): No '%s' class found in 'br-lib/sys-search-dirs'."
		class-name)))))
    class-path))

(defun br-find-class (&optional class-name view-only other-win)
  "Display file of class text matching CLASS-NAME in VIEW-ONLY mode if non-nil.
Return t if class is successfully displayed, nil otherwise.  Can also
signal an error when called interactively."
  (interactive)
  (and (interactive-p) (setq view-only current-prefix-arg))
  (let ((class-path)
	(info (equal br-lang-prefix "info-"))
	(err))
    (setq class-name
	  (or class-name (br-complete-class-name))
	  class-path (br-class-path class-name))
    (cond 
     (info (info-find-nd class-path class-name (not view-only)))
     (class-path
      (if (file-readable-p class-path)
	  (progn (if view-only 
		     (funcall br-view-file-function class-path other-win)
		   (funcall br-edit-file-function class-path other-win)
		   ;; Handle case of already existing buffer in
		   ;; read only mode.
		   (and buffer-read-only
			(file-writable-p class-path)
			(progn (setq buffer-read-only nil)
			       ;; Force mode-line redisplay
			       (set-buffer-modified-p
				(buffer-modified-p)))))
		 (br-major-mode)
		 (let ((opoint (point))
		       (start)
		       (pmin (point-min))
		       (pmax (point-max))
		       (class-def (br-class-definition-regexp class-name)))
		   (widen)
		   (goto-char (point-min))
		   (if br-narrow-view-to-class
		       ;; Display file narrowed to definition of
		       ;; 'class-name'.
		       (if (re-search-forward class-def nil t)
			   ;; Narrow display to this class
			   (progn (narrow-to-region
				   (progn (setq opoint
						(goto-char
						 (match-beginning 0)))
					  (br-to-comments-begin)
					  (setq start (point))
					  (goto-char opoint)
					  start)
				   (progn (br-to-class-end)
					  (point)))
				  (goto-char (point-min)))
			 (goto-char opoint)
			 (narrow-to-region pmin pmax)
			 (setq err (format "(OO-Browser):  No '%s' in %s" class-name
					   class-path))
			 )
		     (if (re-search-forward class-def nil t)
			 (progn (setq opoint (goto-char (match-beginning 0)))
				(br-to-comments-begin)
				(recenter 0))
		       (goto-char opoint)
		       (narrow-to-region pmin pmax)
		       (setq err (format "(OO-Browser):  No '%s' in %s" class-name
					class-path))
		       )))
		 (setq class-path t))
	(setq err (format "(OO-Browser):  '%s' - src file not found or not readable, %s"
			  class-name class-path)
	      class-path nil)
	)
      (if (interactive-p)
	  (setq err
		(format "(OO-Browser):  No '%s' class defined in Environment."
			class-name))
	)))
    (if err (error err))
    class-path))

(defun br-major-mode ()
  "Invoke language-specific major mode on current buffer if not already set."
  (or (eq major-mode (symbol-function 'br-lang-mode))
      (br-lang-mode)))

(defun br-show-children (class-name)
  "Return children of CLASS-NAME from current Environment."
  (interactive (list (br-complete-class-name t)))
  (and class-name
       (br-get-children class-name)))

(defun br-show-parents (class-name)
  "Return parents of CLASS-NAME from Environment or scan of current buffer's source."
  (interactive (list (br-complete-class-name t)))
  (if class-name
      (if (br-class-in-table-p class-name)
	  (br-get-parents class-name)
	(if (and buffer-file-name (file-readable-p buffer-file-name))
	    (let ((br-view-file-function 'br-insert-file-contents))
	      (br-get-parents-from-source buffer-file-name class-name))))))

(defun br-undefined-classes ()
  "Return a list of the classes referenced but not defined within the current Environment."
  (let ((classes (hash-get br-null-path (br-get-paths-htable))))
    (delq nil (mapcar (function (lambda (class)
				  ;; Remove default classes
				  (if (/= (aref class 0) ?\[)
				      class)))
		      classes))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun br-add-to-paths-htable (class-name paths-key htable)
  "Add CLASS-NAME under PATHS-KEY in paths lookup HTABLE, keeping the classes sorted."
  (let ((other-classes (hash-get paths-key htable)))
    (if (and other-classes (br-member-sorted-strings class-name other-classes))
	nil
      (hash-add (sort (cons class-name other-classes) 'string-lessp)
		paths-key htable))))

(defun br-build-lib-parents-htable ()
  (interactive)
  (if (not br-lib-search-dirs)
      nil
    (message "Building Library parent...")
    (sit-for 2)
    (setq br-lib-parents-htable
	  (hash-make
	    (if br-lib-paths-htable
		(br-real-build-parents-alist br-lib-paths-htable)
	      (br-real-build-alists br-lib-search-dirs)
	      br-parents-alist)))
    (if (interactive-p) (br-env-save))
    (message "Building Library parent...Done")))

(defun br-build-lib-paths-htable ()
  (interactive)
  (if (not br-lib-search-dirs)
      nil
    (message "Building Library paths...")
    (sit-for 2)
    (br-real-build-alists br-lib-search-dirs)
    (setq br-lib-paths-htable (hash-make br-paths-alist))
    (if (interactive-p) (br-env-save))
    (message "Building Library paths...Done")))

(defun br-build-sys-parents-htable ()
  (interactive)
  (if (not br-sys-search-dirs)
      nil
    (message "Building System parents...")
    (sit-for 2)
    (setq br-sys-parents-htable
	  (hash-make
	    (if br-sys-paths-htable
		(br-real-build-parents-alist br-sys-paths-htable)
	      (br-real-build-alists br-sys-search-dirs)
	      br-parents-alist)))
    (if (interactive-p) (br-env-save))
    (message "Building System parents...Done")))

(defun br-build-sys-paths-htable ()
  (interactive)
  (if (not br-sys-search-dirs)
      nil
    (message "Building System paths...")
    (sit-for 2)
    (br-real-build-alists br-sys-search-dirs)
    (setq br-sys-paths-htable (hash-make br-paths-alist))
    (if (interactive-p) (br-env-save))
    (message "Building System paths...Done")))

(defun br-build-children-htable ()
  (interactive)
  (setq br-children-htable (br-real-build-children-htable))
  (if (interactive-p) (br-env-save)))

(defun br-build-parents-htable ()
  (interactive)
  (br-build-sys-parents-htable)
  (br-build-lib-parents-htable)
  ;; Make System entries override Library entries which they duplicate, since
  ;; this is generally more desireable than merging the two.
  (let ((hash-merge-values-function (function (lambda (val1 val2) val1))))
    (setq br-parents-htable (hash-merge br-sys-parents-htable
					br-lib-parents-htable)))
  (if (interactive-p) (br-env-save)))

(defun br-build-paths-htable ()
  (interactive)
  (br-build-sys-paths-htable)
  (br-build-lib-paths-htable)
  (setq br-paths-htable (hash-merge br-sys-paths-htable br-lib-paths-htable))
  (if (interactive-p) (br-env-save)))

(defun br-class-defined-p (class)
  "Return path for CLASS if defined in current Environment.
Otherwise, display error and return nil."
  (or (br-class-path class)
      (progn
	(beep)
	(message
	 (if (br-class-in-table-p class)
	     (format "(OO-Browser):  Class '%s' referenced but not defined in Environment."
		     class)
	   (format "(OO-Browser):  Class '%s' not defined in Environment."
		   class)))
	nil)))

(defun br-check-for-class (cl &optional other-win)
  "Try to display class CL.
Display message and return nil if unsucessful."
  (if (br-class-in-table-p cl)
      (or (br-find-class cl nil other-win)
	  (progn
	    (beep)
	    (message
	     (format "(OO-Browser):  Class '%s' referenced but not defined in Environment."
		     cl))
	    t))))

(defun br-get-children (class-name)
  "Return list of children of CLASS-NAME from child lookup table.
Those which directly inherit from CLASS-NAME."
  (setq class-name (and class-name (br-set-case class-name)))
  (br-set-of-strings (hash-get class-name (br-get-children-htable))))

(defun br-get-parents (class-name)
  "Return list of parents of CLASS-NAME from parent lookup table.
Those from which CLASS-NAME directly inherits."
  (setq class-name (and class-name (br-set-case class-name)))
  (br-set-of-strings (hash-get class-name (br-get-parents-htable))))

(defun br-get-children-htable ()
  "Loads or builds 'br-children-htable' if necessary and returns value."
  (br-get-htable "children"))

(defun br-get-paths-htable ()
  "Loads or builds 'br-paths-htable' if necessary and returns value."
  (br-get-htable "paths"))

(defun br-get-parents-htable ()
  "Loads or builds 'br-parents-htable' if necessary and returns value."
  (br-get-htable "parents"))

(defun br-get-children-from-parents-htable (class-name)
  "Return list of children of CLASS-NAME.
Those that directly inherit from CLASS-NAME.  Use parent lookup table to
compute children."
  (setq class-name (and class-name (br-set-case class-name)))
  (delq nil (hash-map (function (lambda (cns)
				  (if (and (consp cns)
					   (br-member class-name (car cns)))
				      (cdr cns))))
		      (br-get-parents-htable))))

(defun br-get-htable (htable-type)
  "Return hash table corresponding to string, HTABLE-TYPE.  When necessary,
load the hash table from a file or build it."
  (let* ((htable-symbol (intern-soft (concat "br-" htable-type "-htable")))
	 (htable-specific (if (string-match "sys\\|lib" htable-type)
			      (substring htable-type (match-beginning 0)
					 (match-end 0))))
	 changed-types non-matched-types)
    (if (equal htable-type "children")
	nil
      (if (and (or (not htable-specific) (equal htable-specific "lib"))
	       (or (null (symbol-value htable-symbol))
		   (not (equal br-lib-prev-search-dirs br-lib-search-dirs))))
	  (setq changed-types '("lib")))
      (if (and (or (not htable-specific) (equal htable-specific "sys"))
	       (or (null (symbol-value htable-symbol))
		   (not (equal br-sys-prev-search-dirs br-sys-search-dirs))))
	  (setq changed-types (cons "sys" changed-types))))
    (if (and (or br-lib-search-dirs br-sys-search-dirs)
	     (or changed-types (null (symbol-value htable-symbol)))
	     (not (boundp 'br-loaded)))
	;;
	;; Then need to load or rebuild htable.
	;;
	(progn (if (and br-env-file
			(file-exists-p br-env-file))
		   ;;
		   ;; Try to load from file.
		   ;;
		   (progn (setq non-matched-types
				(br-env-load-matching-htables changed-types))
			  (if non-matched-types
			      (setq changed-types
				    (delq nil (mapcar
					       (function
						(lambda (type)
						  (if (br-member type
								 changed-types)
						      type)))
					       non-matched-types)))
			    (and changed-types (br-env-set-htables))
			    (setq changed-types nil)
			    (cond (htable-specific)
				  ((equal htable-type "children")
				   (progn (goto-char (point-min))
					  (setq br-children-htable
						(cdr (br-env-file-sym-val
						      "br-children-htable")))))
				  ((let ((suffix
					  (concat "-" htable-type "-htable"))
					 (hash-merge-values-function
					  'hash-merge-values))
					 ;; Make System entries override
					 ;; Library entries which they
					 ;; duplicate, if this is the parents
					 ;; htable.
				     (if (equal htable-type "parents")
					 (setq hash-merge-values-function
					       (function
						(lambda (val1 val2) val1))))
				     (set htable-symbol
					  (hash-merge
					   (symbol-value
					    (intern-soft
					     (concat "br-sys" suffix)))
					   (symbol-value
					    (intern-soft
					     (concat
					      "br-lib" suffix)))
					   ))))))))
	       ;; Rebuild any lists that need to be changed.
	       (mapcar
		(function
		 (lambda (type-str)
		   (let ((suffix (concat "-" htable-type "-htable")))
		     (funcall (intern-soft
			       (concat "br-build-" type-str suffix)))
		     (and htable-specific
			  ;; Make System entries override Library entries
			  ;; which they duplicate, if this is the parents
			  ;; htable.
			  (let ((hash-merge-values-function
				 'hash-merge-values))
			    (if (equal htable-type "parents")
				(setq hash-merge-values-function
				      (function (lambda (val1 val2) val1))))
			    (set htable-symbol
				 (hash-merge (symbol-value
					      (intern-soft
					       (concat "br-sys" suffix)))
					     (symbol-value
					      (intern-soft
					       (concat "br-lib" suffix)))
					     )))))))
		changed-types)
	       (if (and changed-types br-env-file)
		   (br-env-save))
	       (let ((buf (get-file-buffer br-env-file)))
		 (and buf (kill-buffer buf)))
	       ))
    ;; Return non-nil hash table.
    (if (null (symbol-value htable-symbol))
	(set htable-symbol (hash-make 0))
      (symbol-value htable-symbol))))

(defun br-get-top-class-list (htable-type-str)
    "Returns unordered list of top-level classes.
Those that do not explicitly inherit from any other classes.  Obtains classes
from list denoted by HTABLE-TYPE-STR whose values may be:
\"parents\", \"sys-parents\", or \"lib-parents\"."
    (delq nil (hash-map (function
			  (lambda (cns)
			    (and (null (car cns)) (cdr cns))))
			(br-get-htable htable-type-str))))

(defun br-get-top-classes ()
  "Returns lexicographically ordered list of top-level classes.
Those that do not explicitly inherit from any other classes."
  (br-get-top-class-list "parents"))

(defun br-get-lib-top-classes ()
  "Returns lexicographically ordered list of top-level Library classes.
Those that do not explicitly inherit from any other classes."
  (br-get-top-class-list "lib-parents"))

(defun br-get-sys-top-classes ()
  "Returns lexicographically ordered list of top-level System classes.
Those that do not explicitly inherit from any other classes."
  (br-get-top-class-list "sys-parents"))

(defun br-has-children-p (class-name)
  "Return non-nil iff CLASS-NAME has at least one child.
That is a class that directly inherits from CLASS-NAME."
  (setq class-name (and class-name (br-set-case class-name)))
  (hash-get class-name (br-get-children-htable)))

(defun br-has-parents-p (class-name)
  "Return non-nil iff CLASS-NAME has at least one parent.
That is a class which is a direct ancestor of CLASS-NAME."
  (setq class-name (and class-name (br-set-case class-name)))
  (hash-get class-name (br-get-parents-htable)))

(defun br-get-process-group (group max)
  "Return list of all active processes in GROUP (a string).
MAX is max number of processes to check for."
  (let ((i 0)
	(proc-list))
    (while (<= i max)
      (setq i (1+ i)
	    proc-list (cons (get-process (concat group (int-to-string i)))
			    proc-list)))
    (delq nil proc-list)))


(defun br-kill-process-group (group max group-descrip)
  "Optionally question user, then kill all subprocesses in named GROUP.
Processes are numbered one to MAX, some of which may have been killed already.
User is prompted with a string containing GROUP-DESCRIP, only if non-nil.
Return list of processes killed."
  (let ((proc-list (br-get-process-group group max)))
    (if proc-list
	(if (or (null group-descrip)
		(y-or-n-p (concat "Terminate all " group-descrip "? ")))
	    (prog1 (mapcar 'delete-process proc-list)
	      (message ""))))))

(defun br-real-add-class (lib-table-p class-name class-path &optional replace)
  "Add or replace class in current Environment.
If LIB-TABLE-P is non-nil, add to Library Environment, otherwise add to
System Environment.  Add class CLASS-NAME located in CLASS-PATH to
Environment.  If CLASS-PATH is nil, use current buffer file as CLASS-PATH.
Optional REPLACE non-nil means replace already existing class.  Does not
update children lookup table."
  (or class-path (setq class-path buffer-file-name))
  (let ((par-list)
	(paths-key class-path)
	(func)
	(class class-name))
    (if replace
	(setq func 'hash-replace
	      class-name (br-first-match
			  (concat "^" (regexp-quote class-name) "$")
			  (hash-get paths-key
				    (if lib-table-p 
					(br-get-htable "lib-paths")
				      (br-get-htable "sys-paths"))))
	      par-list
	      (and (stringp class-path) (file-readable-p class-path)
		   (let ((br-view-file-function 'br-insert-file-contents))
		     (br-get-parents-from-source class-path class-name))))
      (setq func 'hash-add))
    ;; Signal error if class-name is invalid.
    (if (null class-name)
	(if replace
	    (error "(br-real-add-class): '%s' not found in %s classes, so cannot replace it."
		   class (if lib-table-p "Library" "System"))
	    (error
	     "(br-real-add-class): Attempt to add null class to %s classes."
	     (if lib-table-p "Library" "System"))))
    ;;
    (mapcar
      (function
	(lambda (type)
	 (let ((par-htable (br-get-htable (concat type "parents")))
	       (path-htable (br-get-htable (concat type "paths"))))
	   (funcall func par-list class-name par-htable)
	   (br-add-to-paths-htable class-name paths-key path-htable))))
      (list (if lib-table-p "lib-" "sys-") ""))))

(defun br-real-delete-class (class-name)
  "Delete class CLASS-NAME from current Environment.
No error occurs if the class is undefined in the Environment."
  (require 'set)
  (let ((paths-key (br-class-path class-name))
	htable)
    (setq class-name
	  (br-first-match (concat "^" class-name "$")
			  (hash-get paths-key (br-get-paths-htable))))
    (if class-name
	(progn (mapcar
		 (function
		   (lambda (type)
		    (hash-delete class-name 
				 (br-get-htable (concat type "parents")))
		    (setq htable (br-get-htable (concat type "paths")))
		    (if (hash-key-p paths-key htable)
			(hash-replace
			 (set:remove
			  class-name
			  (hash-get paths-key htable))
			 paths-key htable))))
		 '("lib-" "sys-" ""))
	       (hash-delete class-name (br-get-children-htable))))))

(defun br-real-build-children-htable ()
  "Build and return Environment parent to child lookup table."
  (let* ((par-ht (br-get-parents-htable))
	 (htable (hash-make (hash-size par-ht)))
	 (child))
    (hash-map
      (function
	(lambda (par-child-cns)
	  (setq child (cdr par-child-cns))
	  (mapcar
	    (function
	      (lambda (parent)
		(hash-add
		  (cons child (hash-get parent htable))
		  parent htable)))
	    (car par-child-cns))))
      par-ht)
    (hash-map (function
		(lambda (children-parent-cns)
		  (hash-replace (sort (car children-parent-cns) 'string-lessp)
				(cdr children-parent-cns) htable)))
	      htable)
    htable))

(defun br-real-get-children (class-name)
  "Return list of child classes of CLASS-NAME listed in Environment parents htable."
  (delq nil (hash-map
	      (function
		(lambda (cns)
		  (if (and (consp cns)
			   (br-member class-name (car cns)))
		      (cdr cns))))
	      (br-get-parents-htable))))

(defun br-real-build-alists (search-dirs)
  "Use SEARCH-DIRS to build 'br-paths-alist' and 'br-parents-alist'."
  (setq br-paths-alist nil br-parents-alist nil)
  (br-feature-tags-init)
  (br-real-build-al search-dirs)
  (setq br-paths-alist br-paths-alist)
  (br-feature-tags-save)
  br-paths-alist)

(defvar br-paths-alist nil)
(defvar br-parents-alist nil)

(defun br-skip-dir-p (dir-name)
  "Returns non-nil iff DIR-NAME is matched by a member of 'br-skip-dir-regexps'."
  (delq nil
	(mapcar (function
		  (lambda (dir-regexp)
		    (string-match dir-regexp
				  (file-name-nondirectory
				    (directory-file-name dir-name)))))
		br-skip-dir-regexps)))

;;; If abbreviate-file-name is not defined, just make it return the same
;;; string.
(or (fboundp 'abbreviate-file-name)
    (fset 'abbreviate-file-name 'identity))

(defun br-real-build-al (search-dirs)
  "Descend SEARCH-DIRS and build 'br-paths-alist' and 'br-parents-alist'.
Does not initialize 'br-paths-alist' or 'br-parents-alist' to nil."
  (let ((inhibit-local-variables nil)
	(enable-local-variables t)
	(files)
	;; These are used in the 'br-search-directory' function.
	classes parents paths-parents-cons)
    (mapcar 
      (function
	(lambda (dir)
	  (if (or (null dir) (equal dir "")
		  (progn (setq dir (file-name-as-directory dir))
			 (br-skip-dir-p dir)))
	      nil
	    (setq files (if (and (file-directory-p dir)
				  (file-readable-p dir))
			    (directory-files dir t br-file-dir-regexp)))
	    ;; Extract all class/parent names in all source files in a
	    ;; particular directory.
	    (if files
		(progn (message "Scanning %s in %s ..."
				(file-name-nondirectory
				 (directory-file-name dir))
				(abbreviate-file-name
				 (or (file-name-directory
				      (directory-file-name dir))
				     "")))
		       (br-search-directory dir files)
		       ;; Call same function on all the directories below
		       ;; this one.
		       (br-real-build-al
			(mapcar (function (lambda (f)
					    (if (file-directory-p f) f)))
				files)))))))
      search-dirs)))

(defun br-search-directory (dir files)
  (mapcar
    (function
      (lambda (f)
	(if (file-readable-p f)
	    (setq paths-parents-cons
		  (let ((br-view-file-function 'br-insert-file-contents))
		    (message "Scanning %s in %s ..."
			     (file-name-nondirectory f)
			     (abbreviate-file-name
			      (or (file-name-directory f) default-directory)))
		    (br-get-classes-from-source f nil t))
		  classes (car paths-parents-cons)
		  parents (cdr paths-parents-cons)
		  br-paths-alist (if classes
				     (cons (cons (sort classes
						       'string-lessp) f)
					   br-paths-alist)
				   br-paths-alist)
		  br-parents-alist (if parents
				       (append br-parents-alist
					       parents)
				     br-parents-alist))
	  (message "(OO-Browser): Unreadable file: %s in %s"
		   (file-name-nondirectory f)
		   (abbreviate-file-name
		    (or (file-name-directory f) default-directory)))
	  (sit-for 1))))
    ;; List of files potentially containing classes.
    (delq nil
	  (mapcar
	    (function
	      (lambda (f)
		(and (string-match br-src-file-regexp f)
		     (not (file-directory-p f))
		     f)))
	    files))))

(defun br-real-build-parents-alist (paths-htable)
  "Build and return 'br-parents-alist' of (parent-list . class) elements built from PATHS-HTABLE.
Initializes 'br-parents-alist' to nil."
  (let ((inhibit-local-variables nil)
	(enable-local-variables t)
	(br-view-file-function 'br-insert-file-contents))
    (setq br-parents-alist nil)
    (mapcar
      (function
	(lambda (cl-dir-list)
	  (mapcar (function
		    (lambda (class-dir-cons)
		      (let ((dir (cdr class-dir-cons)))
			(mapcar
			  (function
			    (lambda (class-name)
			      (setq br-parents-alist
				    (cons (cons
					   (and (stringp dir)
						(file-exists-p dir)
						(br-get-parents-from-source
						 dir class-name))
					   class-name)
					  br-parents-alist))))
			  (car class-dir-cons)))))
		  cl-dir-list)))
      paths-htable)
    br-parents-alist))

(defun br-set-lang-env (func sym-list val)
  "Use FUNC to set each element in SYM-LIST.
If VAL is non-nil, set 'br' element to value of current OO-Browser language
element with the same name, otherwise set to symbol."
  (let ((br) (lang))
    (mapcar (function
	     (lambda (nm)
	       (setq br   (intern (concat "br-" nm))
		     lang (intern-soft (concat br-lang-prefix nm)))
	       (funcall func br (if val
				    (symbol-value lang)
				  (or lang 'br-undefined-function)))))
	    sym-list)))

(defun br-undefined-function (&rest ignore)
  (interactive)
  (error "(OO-Browser): That command is not supported for this language."))

(defun br-setup-functions ()
  "Initialize appropriate function pointers for the current browser language."
  (br-set-lang-env 'fset
		   '("class-definition-regexp" "class-list-filter"
		     "get-classes-from-source" "get-parents-from-source"
		     "insert-class-info" "set-case" "set-case-type"
		     "to-class-end" "to-comments-begin" "to-definition"
		     "select-path"

		     "feature-implementors" "feature-locate-p"
		     "feature-name-to-regexp" "feature-signature-to-name"
		     "feature-signature-to-regexp" "feature-tag-class"
		     "feature-tree-command-p"
		     "list-categories" "list-features" "list-protocols"
		     "view-friend" "view-protocol")
		   nil))

(defun br-setup-constants ()
  "Initialize appropriate constant values for the current browser language."
  ;; Clear language-dependent hooks.
  (setq br-after-build-lib-hook nil
	br-after-build-sys-hook nil)
  ;; Set language-specific constants.
  (br-set-lang-env 'set '("class-def-regexp" "env-file"
			  "identifier" "identifier-chars"
			  "src-file-regexp" "narrow-view-to-class"
			  "type-tag-separator")
		   t))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar br-lib-search-dirs nil
  "List of directories below which library dirs and source files are found.
A library is a stable group of classes.  Value is language-specific.")
(defvar br-sys-search-dirs nil
  "List of directories below which system dirs and source files are found.
A system is a group of classes that are likely to change.  Value is
language-specific.")

(defvar br-lib-prev-search-dirs nil
  "Used to check if 'br-lib-paths-htable' must be regenerated.
Value is language-specific.")
(defvar br-sys-prev-search-dirs nil
  "Used to check if 'br-sys-paths-htable' must be regenerated.
Value is language-specific.")

(defun br-find-file (filename &optional other-win read-only)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME, creating one if none
already exists.  Optional OTHER-WIN means show in other window.
Optional READ-ONLY means make buffer read-only."
  (interactive "FFind file: ")
  (funcall (if other-win 'switch-to-buffer-other-window 'switch-to-buffer)
	   (find-file-noselect filename))
  (and read-only (setq buffer-read-only t)))

(defun br-find-file-read-only (filename &optional other-win)
  "Display file FILENAME read-only.
Switch to a buffer visiting file FILENAME, creating one if none
already exists.  Optional OTHER-WIN means show in other window."
  (interactive "FFind file read-only: ")
  (br-find-file filename other-win t))

(defvar br-edit-file-function 'br-find-file
  "*Function to call to edit a class file within the browser.")
(defvar br-view-file-function
  (if (eq br-edit-file-function 'br-find-file)
      'br-find-file-read-only
    br-edit-file-function)
  "*Function to call to view a class file within the browser.")

(defvar br-find-file-noselect-function 'find-file-noselect
  "Function to call to load a browser file but not select it.
The function must return the buffer containing the file's contents.")

(defvar *br-tmp-buffer* "*oobr-tmp*"
  "Name of temporary buffer used by the OO-Browser for parsing source files.")

(defun br-insert-file-contents (filename)
  "Insert FILENAME contents into a temporary buffer and select buffer.
Does not run any find-file hooks.  Marks buffer read-only to prevent
any accidental editing.

Set 'br-view-file-function' to this function when parsing OO-Browser source
files for fast loading of many files."
  (let ((buf (get-buffer-create *br-tmp-buffer*)))
    (switch-to-buffer buf)
    (buffer-disable-undo buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-file-contents filename t)))

(defvar br-lang-prefix nil
 "Prefix string that starts language-specific symbol names.")

(defvar br-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse class inheritance graph.  'br-build-children-htable' builds
this list.  Value is language-specific.")
(defvar br-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse class inheritance graph.  'br-build-parents-htable' builds
this list.  Value is language-specific.")
(defvar br-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . DIRECTORY).
DIRECTORY gives the location of classes found in LIST-OF-CLASS-NAMES.
'br-build-paths-htable' builds this list.  Value is language-specific.")

(defvar br-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.
Value is language-specific.")
(defvar br-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . DIRECTORY).
DIRECTORY gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.
Value is language-specific.")

(defvar br-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the
list.  Value is language-specific.")
(defvar br-sys-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . DIRECTORY).
DIRECTORY gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.  Value is language-specific.")

(defvar br-file-dir-regexp "\\`[^.~#]\\(.*[^.~#]\\)?\\'"
  "Regexp that matches only to files and directories that the OO-Browser should scan.
Others are ignored.")

(defvar br-src-file-regexp nil
  "Regular expression matching a unique part of source file names and no others.")

(defvar br-narrow-view-to-class nil
 "Non-nil means narrow buffer to just the matching class definition when displayed.
Don't set this, use the language specific variable instead.")

(provide 'br-lib)
