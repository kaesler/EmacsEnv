;;!emacs
;;
;; FILE:         br.el
;; SUMMARY:      Browse object-oriented code.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     matching, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    12-Dec-89
;; LAST-MOD:     21-Sep-95 at 12:39:17 by Bob Weiner
;;
;; Copyright (C) 1989-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-lib)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar br-c-tags-flag t
  "*Non-nil means add C constructs when building C-based language Environments.")

(defvar br-directory nil
  "Directory in which OO-Browser executable and help files are kept.")

(defconst br-feature-signature-regexp "[:|,]"
  "Regular expression that matches a feature signature but not a class name.")

(defvar br-inherited-features-flag t
  "*If non-nil (the default), feature/element listings include all inherited features.
If nil, only those features lexically included within a class are shown.")

(defvar br-inhibit-version nil
  "*Personal setting which if non-nil, skips version/credit information upon startup.
The default should be left as nil, since new users may find this helpful.")

(defvar br-invert-ancestors nil
  "*Personal setting which if non-nil makes ancestors appear as do other inheritance listings.
That is, parents appear above children, rather than the default, which is the
reverse.")

(defvar br-keep-viewed-classes nil
  "*Personal setting which if non-nil means leave all viewed classes around for later selection.  
Non-nil deletes last viewed class when a new one is displayed.   Note this
does not affect classes displayed for editing, all such classes are left
around.")

(defconst br-min-width-window 25
  "*Minimum width of a browser class list window.
This together with the frame width determines the number of such windows.")

;; -f treats upper and lower case the same in sorting, also makes 'a' sort
;; list before '[a]', so default classes appear at the end of the list,
;; typically.
;; -u leaves only unique elements in the sorted list
(defvar br-sort-options "-fu"
  "*String of options to send to the operating system `sort' command.
Use nil for none.  This is used by the OO-Browser (br-order) command only
under Emacs 18.")

;;; ************************************************************************
;;; Public macros
;;; ************************************************************************

(if (fboundp 'window-highest-p)
    (defun br-non-listing-window-p ()
      "Is the selected window a non-OO-Browser listing window?"
      ;; Top of window is not at top of frame.
      (not (window-highest-p (selected-window))))
  (defun br-non-listing-window-p ()
    "Is the selected window a non-OO-Browser listing window?"
    ;; Top of window is not at top of frame.
    (/= (nth 1 (window-edges)) br-top-of-frame)))

(if (fboundp 'window-highest-p)
    (defun br-listing-window-p ()
      "Is the selected window an OO-Browser listing window?"
      (window-highest-p (selected-window)))
  (defun br-listing-window-p ()
    "Is the selected window an OO-Browser listing window?"
    ;; Top of window is at top of frame.
    (= (nth 1 (window-edges)) br-top-of-frame)))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun br-browse ()
  "Internally invoke the OO-Browser, for browsing class hierarchies.
Use \\[br-help] and \\[br-help-ms] for help on browser usage."
  (interactive)
  ;; If not already in the browser, save window config.
  (if (br-in-browser)
      nil
    (setq *br-prev-wconfig* (current-window-configuration)
	  br-in-browser (selected-frame))
    ;; If were previously in the browser, restore its saved window config,
    ;; otherwise, set up from scratch.
    (if *br-save-wconfig*
	(set-window-configuration *br-save-wconfig*)
      (br-window-setup)
      (if br-inhibit-version
	  (br-top-classes t)
	(br-version)
	(message "Press {h} for for help.")
	;; Display all classes.
	(br-top-classes t)
	(message "Press {h} for for help.")
	;; Wait for 60 seconds or until a single key sequence is given.
	(sit-for 60)
	(message ""))
      (br-help))
    (run-hooks 'br-mode-hook
	       (intern (concat "br-" br-lang-prefix "mode-hook")))))

;;;###autoload
(defun br-add-class-file (&optional class-path lib-table-p save-file)
  "Add a file of classes to the current Environment.
Interactively or when optional CLASS-PATH is nil, CLASS-PATH defaults to the
current buffer file pathname.  If optional LIB-TABLE-P is non-nil, add to
Library Environment, otherwise add to System Environment.  If optional
SAVE-FILE is t, the Environment is then stored to the filename given by
'br-env-file'.  If SAVE-FILE is non-nil and not t, its string value is used
as the file to which to save the Environment."
  (interactive
    (list (read-file-name (concat "Class file name to add"
				  (if buffer-file-name
				      (concat " (default \""
					      (file-name-nondirectory
						buffer-file-name)
					      "\")"))
				  ": ")
			  nil buffer-file-name t)
	  (y-or-n-p "Add to Library, rather than System tables? ")
	  (y-or-n-p
	    (concat "Save tables after addition to " br-env-file "? "))))
  (or class-path (setq class-path buffer-file-name))
  (if (not (if class-path (file-readable-p class-path)))
      (error "(br-add-class-file): %s is not readable" class-path))
  (let* ((paths-parents-cons
	   (let ((br-view-file-function 'br-insert-file-contents))
	     (br-get-classes-from-source class-path)))
	 (classes (car paths-parents-cons))
	 (parents (cdr paths-parents-cons))
	 (paths-key class-path)
	 (path-htable (br-get-htable (if lib-table-p "lib-paths" "sys-paths")))
	 (par-htable (br-get-htable
		       (if lib-table-p "lib-parents" "sys-parents")))
	 (child-htable (br-get-children-htable)))
    (mapcar
      (function
	(lambda (class)
	  (br-add-to-paths-htable class paths-key path-htable)))
      classes)
    (mapcar
      (function
	(lambda (parent-cons)
	  (hash-add (car parent-cons) (cdr parent-cons) par-htable)))
      parents)
    (br-env-set-htables)
    (let ((child) (par-list) children)
      (mapcar
	(function
	  (lambda (parent-cons)
	    (setq child (cdr parent-cons)
		  par-list (car parent-cons))
	    (mapcar
	      (function
		(lambda (parent)
		  (setq children (hash-get parent child-htable))
		  (or (br-member child children)
		      (hash-add (cons child children) parent child-htable))))
	      par-list)))
	parents)))
  (cond ((eq save-file nil))
	((eq save-file t) (br-env-save))
	((br-env-save save-file))))

(defun br-ancestors (&optional arg features-flag)
  "Display ancestor tree whose root is the current class.
With optional prefix ARG, display all ancestor trees whose roots are in the
current listing.  If ARG = -1 or 'br-invert-ancestors' is t, the current
class ancestry tree is inverted.  That is, it shows branches going down
towards the root class, so that parents appear above children.  If ARG < -1 or
'br-invert-ancestors' is t and ARG > 1, then the ancestry trees of all
classes in the current listing are inverted.

Optional second argument, FEATURES-FLAG non-nil means display features under
each ancestor class."
  (interactive "p")
  (or arg (setq arg 1))
  (if br-invert-ancestors (setq arg (- arg)))
  (let* ((class-list
	  (if (and (/= arg 1) (/= arg -1))
	      (br-this-level-classes)
	    (list (br-find-class-name))))
	 (parents (delq nil (mapcar (function
				     (lambda (c) (br-get-parents c)))
				    class-list))))
    (cond ((or parents
	       (and features-flag
		    (if (/= 1 (length class-list))
			t ;; Assume some class will have features.
		      ;; This class must have features.
		      (br-list-features (car class-list)))))
	   (if (and (/= arg 1) (/= arg -1))
	       (message "Computing %s..."
			(if features-flag "features" "ancestors")))
	   (if features-flag
	       (progn
		 (br-add-level-hist)
		 (br-next-buffer))
	     (let ((child-level (br-buffer-level)))
	       (br-add-level-hist)
	       (br-next-listing-window -1)
	       (br-next-buffer (concat "p" child-level))))
	   (let (buffer-read-only)
	     (cond ((>= arg 0)
		    (br-ancestor-trees-inverted class-list))
		   (t
		    (br-ancestor-trees class-list))))
	   (goto-char (point-min))
	   (if (and (/= arg 1) (/= arg -1))
	       (message "Computing %s...Done"
			(if features-flag "features" "ancestors")))
	   t)
	  (t
	   (message "No %s." (if features-flag "features" "ancestors"))
	   (beep)))))

(defun br-at (&optional arg)
  "Display current class location in the inheritance graph.
The class is displayed among both its ancestors and descendants.
With optional prefix ARG, display location for all classes in the current
listing."
  (interactive "P")
  (let* ((parent)
	 (parent-list
	   (if arg
	       (br-this-level-classes)
	     (list (setq parent (br-find-class-name))))))
    (if arg 
	(message "Computing class locations...")
      (br-narrow-to-class))
    (br-add-level-hist)
    (br-next-buffer)
    (let (buffer-read-only)
      (br-descendant-trees (br-ancestor-roots parent-list))
      (goto-char (point-min))
      (if arg
	  (message "Computing class locations...Done")
	(re-search-forward (concat "\\(^\\|[ \t]+\\)" parent "$"))
	(goto-char (match-end 1))
	(recenter '(4))))))

(defun br-categories (&optional arg)
  "Display categories directly associated with the current class.
This does not include any categories which the class inherits.
With optional prefix ARG, display categories of all classes in the current
listing."
  (interactive "P")
  (let ((has-categories)
	class-list categories class-and-categories)
    (setq class-list (cond (arg
			    (message "Computing class categories...")
			    (br-this-level-classes))
			   (t 
			    (list (br-find-class-name))))
	  categories
	  (delq nil (mapcar
		     (function
		      (lambda (class)
			(setq class-and-categories (br-list-categories class)
			      has-categories (or has-categories
						 class-and-categories))
			(cons class class-and-categories)))
		     class-list)))
    (cond ((not class-list)
	   (message "(OO-Browser):  Apply 'br-categories' to a class.") (beep))
	  ((not has-categories)
	   (message "No class categories.") (beep))
	  (t
	   (br-add-level-hist)
	   (br-next-buffer nil)
	   (let (buffer-read-only done-set class)
	     (mapcar
	      (function
	       (lambda (class-and-categories)
		 (setq class (car class-and-categories))
		 (if (not (br-set-cons done-set class))
		     (insert class " ...\n")
		   ;; Class successfully added to set, so it has not been
		   ;; listed before.
		   (insert class "\n")
		   (br-insert-features (cdr class-and-categories) 2))))
	      categories))
	   (message "Computing class categories...Done")
	   (goto-char (point-min))
	   t))))

(defun br-children (&optional arg)
  "Display children of current class.
With optional prefix ARG, display children of all the classes in the current
listing."
  (interactive "P")
  (let ((class-list (cond (arg
			   (message "Computing children...")
			   (br-this-level-classes))
			  (t
			   (list (br-find-class-name)))))
	(has-children)
	children children-list)
    (setq children-list (delq nil (mapcar
				   (function
				    (lambda (parent)
				      (setq children
					    (br-get-children parent)
					    has-children
					    (or has-children children))
				      (cons parent children)))
				   class-list)))
    (cond ((not children-list)
	   (message "(OO-Browser):  Apply 'br-children' to a class.")
	   (beep))
	  ((not has-children)
	   (message "No children.") (beep))
	  (t
	   (br-add-level-hist)
	   (br-next-buffer nil)
	   (let (buffer-read-only done-set parent)
	     (mapcar
	      (function
	       (lambda (parent-children-cons)
		 (setq parent (car parent-children-cons))
		 (if (not (br-set-cons done-set parent))
		     (insert parent " ...\n")
		   ;; Class successfully added to set, so it has not been
		   ;; listed before.
		   (insert parent "\n")
		   (br-insert-classes (cdr parent-children-cons) 2))))
	      children-list))
	   (if arg (message "Computing children...Done"))
	   (goto-char (point-min))
	   t))))

(defun br-class-stats (&optional prompt)
  "Display statistics summary for current class.
Optional prefix arg PROMPT means prompt for class name."
  (interactive "P")
  (let ((class-name (if prompt (br-complete-class-name) (br-find-class-name))))
    (if class-name
	(message "Class %s:  Parents: %d; Children: %d"
		 class-name (length (br-get-parents class-name))
		 (length (br-get-children class-name)))
      (error "No class name at point."))))

(defun br-cmd-help (key &optional full)
  "Show first line of doc for OO-Browser KEY in minibuffer.
With optional FULL, display full documentation for command."
  (interactive "kOO-Browser key binding: \nP")
  (let* ((cmd (let ((cmd (if (eq major-mode 'br-mode)
			     (lookup-key br-mode-map key)
			   (key-binding key))))
		(if (not (integerp cmd)) cmd)))
	 (doc (and cmd (documentation cmd)))
	 (end-line))
    (if doc
	(or full
	    (setq end-line (string-match "[\n]" doc)
		  doc (substitute-command-keys (substring doc 0 end-line))))
      (setq doc (format "No documentation for {%s} %s" key (or cmd ""))))
    (if (and cmd doc)
	(if full
	    (progn (br-to-view-window)
		   (other-window -1)
		   (describe-function cmd))
	  (message doc)))))

(defun br-count ()
  "Count number of entries visible in current listing buffer.
Print text result in minibuffer when called interactively."
  (interactive)
  (let ((cnt (count-lines (point-min) (point-max))))
    (if (interactive-p)
	(message "%s contains %d entries." (buffer-name) cnt)
      cnt)))

(defun br-copyright ()
  "Display browser copyright information in viewer window."
  (interactive)
  (br-file-to-viewer "BR-COPY"))

(defun br-delete (&optional prompt)
  "Delete class from current Environment.
Does not alter descendency relations.
Optional prefix arg PROMPT means prompt for class name."
  (interactive "P")
  (let ((class (if prompt (br-complete-class-name) (br-find-class-name))))
    (and class
	 (if (interactive-p)
	     (y-or-n-p (concat "Delete class " class " from Environment? "))
	   t)
	 (progn (br-real-delete-class class)
		;; Delete class name at point in listing window
		(or prompt (let (buffer-read-only)
			     (progn (beginning-of-line)
				    (delete-region
				     (point) (progn (forward-line 1)
						    (point))))))
		(message "Class " class " deleted.")))))

(defun br-descendants (&optional arg)
  "Display descendant tree whose root is the current class.
With optional prefix ARG, display all descendant trees whose roots are
the classes in the current listing."
  (interactive "P")
  (let ((parent-list
	 (if arg
	     (br-this-level-classes)
	   (list (br-find-class-name)))))
    (cond ((delq nil (mapcar
		      (function (lambda (parent)
				  (br-get-children parent)))
		      parent-list))
	   (if arg (message "Computing descendants..."))
	   (br-add-level-hist)
	   (br-next-buffer)
	   (let (buffer-read-only)
	     (br-descendant-trees parent-list))
	   (goto-char (point-min))
	   (if arg (message "Computing descendants...Done"))
	   t)
	  (t
	   (message "No descendants.") (beep)))))

(defun br-edit-entry (&optional prompt)
  "Edits source for any browser listing entry, such as a class or a feature.
Optional prefix arg PROMPT means prompt for entry name."
  (interactive "P")
  (let ((entry) (sig))
    (if prompt
	(cond ((and (setq entry (br-complete-entry))
		    (string-match br-feature-signature-regexp entry))
	       (if (setq sig (car (br-feature-signature-and-file entry)))
		   (br-feature nil nil sig)
		 (error "(br-feature-signature-and-file): Couldn't find match for: '%s'" entry)))
	      (entry  ;; class name
		(br-edit nil entry))
	      (t (error "(br-complete-entry): Exited without selecting a match")))
      (cond ((br-find-feature-entry)
	     (br-feature))
	    ((and (setq entry (br-find-class-name))
		  (br-class-in-table-p entry))
	     (br-edit nil entry))
	    (t (error "(OO-Browser): No entry for current line in current Environment"))))))

(defun br-edit (&optional prompt class)
  "Edit a class in the viewer window.
Select viewer window.  With optional prefix arg PROMPT, prompt for class
name.  Optional CLASS is the one to edit."
  (interactive "P")
  (or br-editor-cmd
      (br-in-view-window-p)
      (setq *br-prev-listing-window* (selected-window)))
  (br-view prompt t class))

(defun br-edit-ext (editor-cmd file)
  "Invoke a non-standard EDITOR-CMD on FILE.
See also 'br-editor-cmd'."
  (interactive "fFile to edit: ")
  (or editor-cmd (setq editor-cmd br-editor-cmd))
  (if (not (stringp editor-cmd)) ;; must be a Lisp function that takes a
      ;; single, file arg
      (funcall editor-cmd file)
    (setq delete-exited-processes t)
    (let ((proc)
	  (name (concat br-ed-name br-ed-num))
	  )
      (setq br-ed-num (1+ br-ed-num)
	    proc (br-edit-ext-start editor-cmd name file))
      (if proc
	  (process-kill-without-query proc)
	(beep)
	(message "(OO-Browser):  Could not start external edit process: %s"
		 editor-cmd)))))

(defun br-editor-kill ()
  "Kill all current external editor sub-processes."
  (interactive)
  (if (br-kill-process-group br-ed-name br-ed-num "external editors")
      (setq br-ed-num 0)))

(defun br-entry-info ()
  "Display attributes of the current entry in the viewer window."
  (interactive)
  (if (fboundp 'br-insert-class-info)
      (let ((class-name (br-find-class-name)))
	(if class-name
	    (progn
	      (message "Building '%s' class info..." class-name)
	      (sit-for 2)
	      (br-store-class-info class-name)
	      (message "Building '%s' class info...Done" class-name)
	      (br-funcall-in-view-window
	       (concat br-buffer-prefix-info "Info")
	       'br-insert-class-info))
	  (error "Move point to a class name line.")))
    (beep)
    (message "No class information function for this language.")))

(defun br-exit-level (arg)
  "Return to prefix ARGth previous inheritance level listing.
The command is ignored with ARG < 1."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((prev-wind-buf-line))
    (if (null *br-level-hist*)
	(and (> arg 0)
	     (message "No previous level to which to exit.")
	     (beep))
      (while (and (> arg 0) *br-level-hist*)
	(br-next-buffer (br-listing-window-num) br-buffer-prefix-blank)
	(setq prev-wind-buf-line (car *br-level-hist*)
	      *br-level-hist* (cdr *br-level-hist*)
	      arg (1- arg))
	(select-window (car prev-wind-buf-line))
	(switch-to-buffer (car (cdr prev-wind-buf-line))))
      (widen)
      ;; Position window lines exactly as before.
      (recenter (car (cdr (cdr prev-wind-buf-line)))))))

(defun br-feature (&optional arg view-only ftr-sig)
  "Edit a feature in the viewer window.  Select viewer window.
With optional prefix ARG, prompt for feature name.
Optional VIEW-ONLY non-nil means view rather than edit feature.
Optional FTR-SIG is signature of feature to edit."
  (interactive "P")
  (or ftr-sig
      (setq ftr-sig (if arg
			(br-feature-complete 'must-match)
		      ;; Get current feature signature
		      (br-feature-get-signature))))
  (if (null ftr-sig)
      (error "(br-feature): No definition for this entry")
    (br-to-view-window)
    (if (br-feature-found-p (br-feature-file ftr-sig) ftr-sig)
	(if view-only
	    (progn (setq buffer-read-only t)
		   (br-to-from-viewer))
	  (if (file-writable-p (buffer-file-name))
	      (setq buffer-read-only nil)))
      ;; Feature not found.  Return to original window and signal an error.
      (br-to-from-viewer)
      (error "(br-feature): Can't find definition of: '%s'" ftr-sig))))

(defun br-features (arg)
  "Display features/elements of the current class (prefix ARG = 1) or of the current listing if ARG is other than 0 or 1.

With ARG = 0, the value of the variable, 'br-inherited-features-flag', is
toggled and no other action is taken.

If 'br-inherited-features-flag' is t, all features of each class are shown.
If nil, only lexically included features are shown and if the features of a
single class are requested and none are defined, the class definition is
displayed so that its feature declarations may be browsed."
  (interactive "p")
  (cond ((and (integerp arg) (= arg 0))
	 (setq br-inherited-features-flag
	       (not br-inherited-features-flag))
	 (message "Inherited features/elements will %sbe shown."
		  (if br-inherited-features-flag "" "not ")))
	(br-inherited-features-flag
	 (br-inherited-features arg))
	(t (br-lexical-features arg))))

(defun br-find (element)
  "Interactively complete class or ELEMENT name and jump to its definition.
Return ELEMENT or signal an error."
  (interactive (list (br-complete-entry)))
  (if (and element
	   (progn
	     (if (not (br-in-view-window-p)) (br-to-from-viewer))
	     (if (string-match br-feature-signature-regexp element)
		 (br-find-feature element)
	       (br-find-class element))))
      element
    (error "(OO-Browser): '%s' definition not found." element)))

(defun br-help (&optional file)
  "Display browser operation help information in viewer window."
  (interactive)
  (or file (setq file "br-help"))
  (br-file-to-viewer file)
  (save-window-excursion
    (br-to-view-window)
    (br-mode)
    (use-local-map nil))
  (message ""))

(defun br-help-ms ()
  "Display browser mouse usage help information in viewer window."
  (interactive)
  (br-help "br-help-ms"))

(defun br-implementors (&optional arg)
  "Display hierarchy of classes that define current element.
Ignore inherited elements.  With optional prefix ARG, display implementors of
all elements in the current listing."
  (interactive "P")
  (let
      ((child-level (br-buffer-level))
       (ftr-list (if arg (br-set-of-strings
			  (sort (br-this-level-features) 'string-lessp))
		   ;; Need this check to avoid trying to find implementors of
		   ;; a class which happens to have an attached element tag,
		   ;; e.g. in an implementors listing buffer.
		   (save-excursion
		     (beginning-of-line)
		     (skip-chars-forward " \t")
		     (if (looking-at br-feature-entry)
			 (list (br-find-feature-entry)))))))
    (if (or (null ftr-list) (null (car ftr-list)))
	(error
	  "(OO-Browser):  'br-implementors' must be applied to a feature.")
      (message "Computing implementors...")
      (br-add-level-hist)
      (br-next-listing-window -1)
      (br-next-buffer (concat "p" child-level))
      (let ((buffer-read-only) (implementor-tags) (classes)
	    start)
	(widen)
	(erase-buffer)
	(mapcar (function
		  (lambda (ftr-entry)
		    (setq implementor-tags
			  (sort
			   (br-feature-implementors
			    (br-feature-name ftr-entry))
			   'string-lessp)
			  classes (mapcar 'br-feature-tag-class
					  implementor-tags))
		    (insert ftr-entry "\n")
		    (setq start (point))
		    (br-insert-classes classes 4)
		    (save-excursion
		      (goto-char start)
		      (br-feature-put-signatures implementor-tags))))
		ftr-list))
      (goto-char 1)
      (message "Computing implementors...Done"))))

(defun br-inherited-features (arg)
  "Display class features, including those from ancestors.
With optional prefix ARG, display features of all classes in the current
listing."
  (interactive "p")
  (let ((br-ancestor-function
	 (function
	  (lambda (class repeated-class indent)
	    (if repeated-class
		nil
	      (br-insert-features (br-list-features class indent) indent))))))
    (br-ancestors arg t)))

(defun br-kill ()
  "Kill buffer in viewer window and redisplay help text."
  (interactive)
  (br-do-in-view-window '(progn (kill-buffer nil) (br-help))))

(defun br-lexical-features (arg)
  "Display class features lexically defined within current class.
With numeric prefix ARG, display features of all classes in the current
listing.

If the features of a single class are requested and there are no feature
definitions for the class, display the class definition so that its feature
declarations may be browsed."
  (interactive "p")
  (let ((has-features)
	class-list features class-and-features)
    (setq class-list (cond ((and (integerp arg) (/= arg 1))
			    (message "Computing class features...")
			    (br-this-level-classes))
			   (t 
			    (list (br-find-class-name))))
	  features
	  (delq nil (mapcar
		     (function
		      (lambda (class)
			(setq class-and-features (br-list-features class)
			      has-features (or has-features
					       class-and-features))
			(cons class class-and-features)))
		     class-list)))
    (cond ((not class-list)
	   (beep)
	   (message "(OO-Browser):  Apply 'br-features' to a class."))
	  ((not has-features)
	   (if (and (= (length class-list) 1)
		    (br-class-path (car class-list)))
	       (if (br-view nil nil (car class-list))
		   (message
		    "No feature definitions, browse declarations instead."))
	     (message "No class features.") (beep)))
	  (t
	   (br-add-level-hist)
	   (br-next-buffer nil)
	   (let (buffer-read-only done-set class)
	     (mapcar
	      (function
	       (lambda (class-and-features)
		 (setq class (car class-and-features))
		 (if (not (br-set-cons done-set class))
		     (insert class " ...\n")
		   ;; Class successfully added to set, so it has not been
		   ;; listed before.
		   (insert class "\n")
		   (br-insert-features (cdr class-and-features) 2))))
	      features)
	     (message "Computing class features...Done")
	     (goto-char (point-min)))))))

(defun br-lib-rebuild ()
  "Rescan Library components of the current Environment."
  (interactive)
  (if (call-interactively 'br-build-lib-htable)
      (br-top-classes t)))

(defun br-lib-top-classes (&optional arg)
  "Display list of top level Library classes.
With prefix ARG, display all Library classes."
  (interactive "P")
  (and (or (not (interactive-p))
	   (br-in-top-buffer-p)
	   (y-or-n-p "Exit to top-level class listing buffer? "))
       (cond (arg
	      (br-show-top-classes
	       (function (lambda () (br-all-classes "lib")))
	       'uniq)
	      (message "Listing of all Library classes"))
	     (t
	      (br-show-top-classes 'br-get-lib-top-classes 'uniq)
	      (message "Listing of top-level Library classes")))
       (setq *br-level-hist* nil)))

(defun br-match (&optional expr arg again matched)
  "Show all class names in current Environment that contain optional EXPR.
Nil value of EXPR means prompt for a value.  With optional prefix ARG, EXPR
is treated as a string.  By default, it is treated as a regular expresion.
AGAIN non-nil shows the number of classes MATCHED from the last search,
allowing repeated narrowing of the search set.  Empty EXPR when AGAIN is nil
matches to all classes in the Environment."
  (interactive (list nil current-prefix-arg))
  (or expr (setq expr (read-string
		       (concat (if again (format "(%s matches)  " matched))
			       (if arg
				   "Find Environment class string matches"
				 "Find Environment class regular expression matches")
			       (if again " (RTN to end): " ": ")))))
  (if (and again (equal expr ""))
      nil
    (let* ((match-expr (if arg (regexp-quote expr) expr))
	   (classes
	    (delq nil (mapcar
		       (function
			(lambda (cl)
			  (if (string-match match-expr cl) cl)))
		       (if again
			   (sort (br-this-level-classes) 'string-lessp)
			 (br-all-classes))))))
      (setq classes (br-class-list-filter classes))
      (if classes
	  (progn (let (buffer-read-only)
		   (br-feature-clear-signatures)
		   (erase-buffer)
		   (br-insert-classes classes 0))
		 (goto-char (point-min))
		 (br-match nil arg t (br-count)))
	(beep)
	(message "No matches for \"%s\"." expr)))))

(defun br-match-entries (&optional expr arg again matched)
  "Show all entries in current listing that contain optional EXPR.
Nil value of EXPR means prompt for a value.  With optional prefix ARG, EXPR
is treated as a string.  By default, it is treated as a regular expresion.
AGAIN non-nil means show the number of entries MATCHED from last search,
allowing repeated narrowing of the search set.  Empty EXPR when AGAIN is nil
matches to all entries in the listing."
  (interactive (list nil current-prefix-arg))
  (or expr (setq expr (read-string
			(concat (if again (format "(%s matches)  " matched))
				(if arg
				    "Find string matches in listing"
				  "Find regular expression matches in listing")
				(if again " (RTN to end): " ": ")))))
  (if (and again (equal expr ""))
      nil
    (let* ((match-expr (if arg (regexp-quote expr) expr))
	   (buffer-read-only))
      (goto-char (point-min))
      (if (not (re-search-forward match-expr nil t))
	  (progn (beep)
		 (message "No matches for \"%s\"." expr))
	(goto-char (point-min))
	(delete-non-matching-lines match-expr)
	(goto-char (point-min))
	(br-match-entries nil arg t (br-count))))))

(defun br-next-entry (arg)
  "Move point vertically down prefix ARG number of lines in listing buffer."
  (interactive "p")
  (let ((end))
    (setq end (= (forward-line arg) arg))
    (and (looking-at "^$") (forward-line -1) (setq end t))
    (and end (message "No next entry.") (beep))))

(defun br-order (arg)
  "Order current browser listing window entries.
With prefix ARG other than 1 (the default), don't remove leading space from
entry lines before ordering.  Negative ARG means order in descending Ascii
sequence, otherwise order in ascending sequence."
  (interactive "p")
  (setq arg (or arg 1))
  (message "Ordering entries...")
  (let ((buffer-read-only)
	sort-args)
    (and (= arg 1) (progn (goto-char (point-min))
			  (while (re-search-forward "^[ \t]+" nil t)
			    (replace-match ""))))
    (if (string-match "^19\\." emacs-version)
	(progn
	  ;; Emacs 19: This slower than calling an external sort but it
	  ;; maintains the element tags in a listing, allowing further browsing
	  ;; from this buffer.
	  (sort-lines (< arg 0) (point-min) (point-max))
	  ;; Move [default] classes to the end of the sorted list.
	  (goto-char (point-min))
	  (if (re-search-forward "^[ \t]*\\[" nil t)
	      (let (start end)
		(beginning-of-line)
		(setq start (point))
		(goto-char (point-max))
		(re-search-backward "^[ \t]*\\[" nil t)
		(forward-line 1)
		(setq end (point))
		(goto-char (point-max))
		(append-to-buffer (current-buffer) start end)
		(delete-region start end))))
      ;;
      ;; Emacs 18: We can't maintain the buffer tags, so we just use a fast
      ;; external sort.
      (setq sort-args (list (point-min) (point-max) "sort" t t nil)
	    sort-args (if (< arg 0)
			  (if (stringp br-sort-options)
			      (nconc sort-args (list "-r" br-sort-options))
			    (nconc sort-args (list "-r")))
			(if (stringp br-sort-options)
			    (nconc sort-args (list br-sort-options))
			  sort-args)))
      (apply 'call-process-region sort-args)))
  (goto-char (point-min))
  (message "Ordering entries...Done"))

(defun br-parents (&optional arg)
  "Display parents of current class.
With optional prefix ARG, display parents of all the classes in the current
listing."
  (interactive "P")
  (let ((class-list (cond (arg
			   (message "Computing parents...")
			   (br-this-level-classes))
			  (t
			   (list (br-find-class-name)))))
	(has-parents)
	parents parents-list)
    (setq parents-list
	  (delq nil (mapcar (function
			     (lambda (class)
			       (setq parents (br-get-parents class)
				     has-parents (or has-parents parents))
			       (cons class parents)))
			    class-list)))
    (cond ((not parents-list)
	   (message "(OO-Browser):  Apply 'br-parents' to a class.") (beep))
	  ((not has-parents)
	   (message "No parents.") (beep))
	  (t
	   (let ((child-level (br-buffer-level)))
	     (br-add-level-hist)
	     (br-next-listing-window -1)
	     (br-next-buffer (concat "p" child-level)))
	   (let (buffer-read-only done-set class)
	     (mapcar
	      (function
	       (lambda (class-parents-cons)
		 (setq class (car class-parents-cons))
		 (if (not (br-set-cons done-set class))
		     (insert class " ...\n")
		   ;; Class successfully added to set, so it has not been
		   ;; listed before.
		   (insert class "\n")
		   (br-insert-classes (cdr class-parents-cons) 2))))
	      parents-list))
	   (if arg (message "Computing parents...Done"))
	   (goto-char (point-min))
	   t))))

(defun br-prev-entry (arg)
  "Move point vertically up prefix ARG number of lines in listing buffer."
  (interactive "p")
  (setq arg (- arg))
  (and (= (forward-line arg) arg)
       (message "No previous entry.")
       (beep)))

(defun br-protocols (&optional arg)
  "Display protocols to which the current class conforms.
This does not include any protocols which the class inherits from its
ancestors but it does include protocols which conform to other protocols.
With optional prefix ARG, display protocols of all classes in the current
listing."
  (interactive "P")
  (let ((has-protocols)
	class-list protocols class-and-protocols)
    (setq class-list (cond (arg
			    (message "Computing class protocols...")
			    (br-this-level-classes))
			   (t 
			    (list (br-find-class-name)))))
    (if (and (= (length class-list) 1)
	     (br-protocol-entry-p))
	;; If on a protocol entry, display its definition.
	(br-view-protocol (car class-list))
      ;; Otherwise, list protocols for all elements of class-list.
      (setq protocols
	    (delq nil (mapcar
		       (function
			(lambda (class)
			  (setq class-and-protocols (br-list-protocols class)
				has-protocols (or has-protocols
						  class-and-protocols))
			  (cons class class-and-protocols)))
		       class-list)))
      (cond ((not class-list)
	     (beep)
	     (message "(OO-Browser):  Apply 'br-protocols' to a class."))
	    ((not has-protocols)
	     (message "No class protocols.") (beep))
	    (t
	     (br-add-level-hist)
	     (br-next-buffer nil)
	     (let (buffer-read-only done-set class)
	       (mapcar
		(function
		 (lambda (class-and-protocols)
		   (setq class (car class-and-protocols))
		   (if (not (br-set-cons done-set class))
		       (insert class " ...\n")
		     ;; Class successfully added to set, so it has not been
		     ;; listed before.
		     (insert class "\n")
		     (br-insert-features (cdr class-and-protocols) 2))))
		protocols))
	     (message "Computing class protocols...Done")
	     (goto-char (point-min)))))))

(defun br-quit (&optional arg)
  "Quit browser.
With optional prefix ARG, delete window configurations and listing
buffers associated with the browser."
  (interactive "P")
  (if (not (br-in-browser))
      (br-interrupt arg)
    (if (null arg)
	(setq *br-save-wconfig* (current-window-configuration))
      (if (featurep 'br-tree) (br-tree-kill))
      (br-viewer-kill)
      ;; Too dangerous to include (br-editor-kill) here.
      ;; The user can invoke it manually if desired.
      )
    (and *br-prev-wconfig* (set-window-configuration *br-prev-wconfig*))
    (br-interrupt arg)))

(defun br-refresh ()
  "Restore OO-Browser to its state upon startup."
  (interactive)
  (br-window-setup)
  (br-top-classes t)
  (br-help)
  (setq br-in-browser (selected-frame)))

(defun br-report-bug ()
  "Send a message to the OO-Browser discussion list."
  (interactive)
  (if (br-in-browser) (br-to-view-window))
  (hmail:compose "oo-browser@hub.ucsb.edu" '(hypb:configuration)))

(defun br-sys-rebuild ()
  "Rescan System components of the current Environment."
  (interactive)
  (if (call-interactively 'br-build-sys-htable)
      (br-top-classes t)))

(defun br-sys-top-classes (&optional arg)
  "Display list of top level System classes.
With prefix ARG, display all System classes."
  (interactive "P")
  (and (or (not (interactive-p))
	   (br-in-top-buffer-p)
	   (y-or-n-p "Exit to top-level class listing buffer? "))
       (cond (arg
	      (br-show-top-classes
	       (function (lambda () (br-all-classes "sys")))
	       'uniq)
	      (message "Listing of all System classes"))
	     (t
	      (br-show-top-classes 'br-get-sys-top-classes 'uniq)
	      (message "Listing of top-level System classes")))
       (setq *br-level-hist* nil)))

;;;###autoload
(defun br-to-from-viewer ()
  "Move point to viewer window or back to last recorded listing window."
  (interactive)
  (if (br-in-view-window-p)
      (progn (if *br-prev-listing-window*
		 (select-window *br-prev-listing-window*)
	       (other-window 1))
	     (setq *br-prev-listing-window* nil))
    (br-to-view-window)))

(defun br-toggle-c-tags ()
  "Toggle the value of the 'br-c-tags-flag' flag."
  (interactive)
  (setq br-c-tags-flag (not br-c-tags-flag))
  (message "C constructs will %sbe added to C-based language Environments."
	   (if br-c-tags-flag "" "not ")))

(defun br-toggle-keep-viewed ()
  "Toggle the value of the 'br-keep-viewed-classes' flag."
  (interactive)
  (setq br-keep-viewed-classes (not br-keep-viewed-classes))
  (message "Viewed classes will no%s be kept after use."
	   (if br-keep-viewed-classes "w" "t")))

(defun br-top-classes (&optional arg)
  "Display list of top level classes.
With prefix ARG, display all Environment classes."
  (interactive "P")
  (and (or (not (interactive-p))
	   (br-in-top-buffer-p)
	   (y-or-n-p "Exit to top-level class listing buffer? "))
       (cond (arg
	      (br-show-top-classes 'br-all-classes 'uniq)
	      (message "Listing of all Environment classes"))
	     (t
	      (br-show-top-classes 'br-get-top-classes 'uniq)
	      (message "Listing of top-level classes")))
       (setq *br-level-hist* nil)))

(defun br-unique ()
  "Eliminate adjacent duplicate entry names from the current listing window.
If two adjacent entries look the same one is eliminated, even if they refer
to different class elements."
  (interactive)
  (let ((buffer-read-only)
	(again t)
	first second)
    (goto-char (point-min))
    (setq first (br-feature-current))
    (while again
      (setq again (= (forward-line 1) 0)
	    second (br-feature-current))
      (if (not (string-equal first second))
	  (setq first second)
	(beginning-of-line)
	(delete-region (point) (progn (forward-line 1) (point)))
	;; back up to first line again
	(forward-line -1)))
    (goto-char (point-min))))

(defun br-version ()
  "Display browser version number and credits."
  (interactive)
  (br-file-to-viewer "BR-VERSION")
  (br-funcall-in-view-window
   (concat br-buffer-prefix-info "Help")
   (function (lambda ()
	       (if (re-search-forward "<VERSION>" nil t)
		   (replace-match br-version t t))
	       (center-line)
	       (set-buffer-modified-p nil)))
   t))

(defun br-view-entry (&optional prompt)
  "Displays source for any browser listing entry.
Optional prefix arg PROMPT means prompt for entry name."
  (interactive "P")
  (let ((entry) (sig))
    (if prompt
	(cond ((and (setq entry (br-complete-entry))
		    (string-match br-feature-signature-regexp entry))
	       (if (setq sig (car (br-feature-signature-and-file entry)))
		   (br-feature nil 'view sig)
		 (error "(br-feature-signature-and-file): Couldn't find match for: '%s'" entry)))
	      (entry ;; class name
	       (br-view nil nil entry))
	      (t (error "(br-complete-entry): Exited without selecting a match")))
      (cond ((br-find-feature-entry)
	     (br-feature nil 'view))
	    ((and (setq entry (br-find-class-name))
		  (br-class-in-table-p entry))
	     (br-view nil nil entry))
	    (t (error "(OO-Browser): Entry may be referenced but not defined in the Environment."))))))

(defun br-view (&optional prompt writable class)
  "Displays class file in viewer window.
Optional prefix arg PROMPT means prompt for class name.  Non-nil WRITABLE means
allow editing, otherwise display in read-only mode.  Non-nil CLASS is class to
display.

Return t if class is displayed or sent to an external viewer, else nil."
  (interactive "P")
  (or class (setq class (if prompt (br-complete-class-name)
			  (br-find-class-name))))
  (cond ((null class)
	 (beep)
	 (message "(OO-Browser):  Select a class to view.")
	 nil)
	((not (br-class-defined-p class)) nil)
	((and hyperb:window-system
	      (cond ((and br-editor-cmd writable)
		    (br-edit-ext br-editor-cmd (br-class-path class))
		    t)
		   (br-viewer-cmd
		    (br-view-ext br-viewer-cmd (br-class-path class))
		    t))))
	;; Support custom Lisp-based edit/view cmds on any display type
	((and br-editor-cmd writable (not (stringp br-editor-cmd)))
	 (br-edit-ext br-editor-cmd (br-class-path class))
	 t)
	((and br-viewer-cmd (not (stringp br-viewer-cmd)))
	 (br-view-ext br-viewer-cmd (br-class-path class))
	 t)
	(t (let ((owind (selected-window)))
	     (unwind-protect
		 (progn (br-to-view-window)
			(if (and (not br-keep-viewed-classes) buffer-read-only
				 (null (buffer-modified-p)))
			    (kill-buffer (current-buffer)))
			(if (br-find-class class (not writable))
			    (progn (br-major-mode)
				   (if writable
				       (if (file-writable-p (buffer-file-name))
					   (setq buffer-read-only nil))
				     (setq buffer-read-only t)
				     (select-window owind))
				   t)))
	       (or writable (select-window owind)))))))

(defun br-view-ext (viewer-cmd file)
  "Invoke a non-standard VIEWER-CMD on FILE.
See also 'br-viewer-cmd'."
  (interactive "fFile to view: ")
  (or viewer-cmd (setq viewer-cmd br-viewer-cmd))
  (if (not (stringp viewer-cmd)) ;; must be a Lisp function that takes a
      ;; single, file arg
      (funcall viewer-cmd file)
    (setq delete-exited-processes t)
    (let ((proc)
	  (name (concat br-vw-name br-vw-num))
	  )
      (setq br-vw-num (1+ br-vw-num)
	    proc (br-view-ext-start viewer-cmd name file))
      (if proc
	  (process-kill-without-query proc)
	(beep)
	(message "(OO-Browser):  Could not start external view process: %s"
		  viewer-cmd)))))

(defun br-view-full-frame ()
  "Delete all windows in the selected frame except for the viewer window."
  (interactive)
  (setq *br-save-wconfig* (current-window-configuration))
  (br-to-view-window)
  (let ((buf (current-buffer)))
    (br-interrupt)
    (delete-other-windows)
    (switch-to-buffer buf))
  (let* ((cmd (concat br-lang-prefix "browse"))
	 (key (car (where-is-internal (intern-soft cmd)))))
    (message "Recall OO-Browser with: {%s}"
	     (if key
		 (key-description key)
	       (concat (key-description
			(or (car (where-is-internal
				  'execute-extended-command))
			    "\M-x"))
		       " " cmd)))))

(defun br-viewer-kill ()
  "Kill all current external viewer sub-processes."
  (interactive)
  (if (br-kill-process-group br-vw-name br-vw-num "external viewers")
      (setq br-vw-num 0)))

(defun br-viewer-scroll-down (&optional arg)
  "Scroll viewer window downward ARG lines or a windowful if no ARG."
  (interactive "P")
  (let ((owind (selected-window)))
    (unwind-protect
	(progn (br-to-view-window)
	       (scroll-down arg))
      (select-window owind))))

(defun br-viewer-scroll-up (&optional arg)
  "Scroll viewer window upward ARG lines or a windowful if no ARG."
  (interactive "P")
  (let ((owind (selected-window)))
    (unwind-protect
	(progn (br-to-view-window)
	       (scroll-up arg))
      (select-window owind))))

(defun br-where (&optional prompt)
  "Display in minibuffer and return full path of a browser listing entry.
Optional prefix arg PROMPT means prompt for entry name."
  (interactive "P")
  (let ((entry) (path))
    (if prompt
	(cond ((and (setq entry (br-complete-entry))
		    (string-match br-feature-signature-regexp entry))
	       (setq path (cdr (br-feature-signature-and-file entry))))
	      (entry ;; class name
	       (setq path (br-class-defined-p entry)))
	      (t (error "(br-complete-entry): Exited without selecting a match")))
      (cond ((setq entry (br-find-feature-entry))
	     (setq path (cdr (br-feature-signature-and-file entry))))
	    ((setq entry (br-find-class-name))
	     (or (setq path (br-class-path entry))
		 (error "(OO-Browser): No path for this class in current Environment")))
	    (t (error "(OO-Browser): No entry for current line in current Environment"))))
    (and path (message (concat entry ":  " "\"" path "\""))
	 path)))

(defun br-write-buffer (file)
  "Write narrowed portion of current browser buffer to a file."
  (interactive "FFile to write buffer to: ")
  (write-region (point-min) (point-max) file))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun br-add-level-hist ()
  ;; Even though this next line looks useless, it cures a problem with
  ;; window buffer correspondences when the OO-Browser is started, so don't
  ;; remove it.
  (set-buffer (window-buffer (selected-window)))
  (setq *br-level-hist*
	(cons (list (selected-window) (buffer-name) (br-wind-line-at-point))
	      *br-level-hist*)))

(defun br-ancestor-roots (class-list)
  "Return list of CLASS-LIST's unique ancestors which do not inherit from any other class.
This list may include elements from CLASS-LIST itself."
  (let ((rtn) (parents) func)
    (setq func (function
		(lambda (class-list)
		  (mapcar
		   (function
		    (lambda (class)
		      (if (not (setq parents (br-get-parents class)))
			  (setq rtn (cons class rtn))
			(funcall func parents))))
		   class-list))))
    (funcall func class-list)
    (br-set-of-strings (sort rtn 'string-lessp))))

(defun br-ancestor-trees-inverted (class-list &optional depth offset)
  "Insert ancestor trees starting with classes from CLASS-LIST.
Ancestor trees are inverted, i.e. parents appear below children, not above.
Indent each class in CLASS-LIST by optional DEPTH spaces (default is 0 in
order to ensure proper initialization).  Offset each child level by optional
OFFSET spaces from its parent (which must be greater than zero, default 2)."
  (or offset (setq offset 2))
  (or depth (setq depth 0))
  (if (= depth 0) (setq br-tmp-class-set nil))
  (let ((prev-expansion-str " ...")
	parents expand-subtree)
    (mapcar
      (function
	(lambda (class)
	  (setq expand-subtree (br-set-cons br-tmp-class-set class)
		parents (if expand-subtree (br-get-parents class)))
	  (indent-to depth)
	  (insert class)
	  (and (not expand-subtree) (br-has-children-p class)
	       (insert prev-expansion-str))
	  (insert "\n")
	  (if br-ancestor-function
	      (funcall br-ancestor-function
		       class (not expand-subtree) (+ depth offset)))
	  (if parents
	      (br-ancestor-trees-inverted parents (+ depth offset) offset))))
      class-list))
  (if (= depth 0) (setq br-tmp-class-set nil)))

(defun br-ancestor-trees (class-list &optional depth offset)
  "Insert ancestor trees starting with classes from CLASS-LIST.
Ancestor trees are not inverted, parents appear above children as in other
browser listing windows.  Indent each class in CLASS-LIST by optional DEPTH
spaces (default is 0 in order to ensure proper initialization).  Offset each
child level by optional OFFSET spaces from its parent (which must be greater
than zero, default 2)."
  (or offset (setq offset 2))
  (or depth (setq depth 0 br-tmp-depth 0))
  (if (= depth 0) (setq br-tmp-class-set nil))
  (let ((prev-expansion-str " ...")
	parents expand-subtree)
    (mapcar (function
	      (lambda (class)
		(setq expand-subtree (br-set-cons br-tmp-class-set class)
		      parents (if expand-subtree (br-get-parents class)))
		(if parents
		    (progn (setq br-tmp-depth
				 (max (+ depth offset) br-tmp-depth))
			   (br-ancestor-trees
			    parents (+ depth offset) offset)))
		(indent-to (- br-tmp-depth depth))
		(insert class)
		(and (not expand-subtree) (br-has-parents-p class)
		     (insert prev-expansion-str))
		(insert "\n")
		(if br-ancestor-function
		    (funcall br-ancestor-function
			     class (not expand-subtree) (+ depth offset)))
		(if (= depth 0) (setq br-tmp-depth 0))))
	    class-list))
  (if (= depth 0) (setq br-tmp-class-set nil)))

(defun br-browser-buffer-p (&optional buffer)
  "Returns t iff optional BUFFER or current buffer is an OO-Browser specific buffer."
  (equal 0 (string-match (concat br-buffer-prefix-inher
				 "\\|" br-buffer-prefix-categ
				 "\\|" br-buffer-prefix-blank
				 "\\|" br-buffer-prefix-info)
			 (buffer-name buffer))))

(defun br-buffer-level ()
  "Returns current listing buffer level as a string."
  (let* ((name (buffer-name))
	 (pos (string-match "-[p]*[0-9]+$" name)))
    (and pos (substring name (1+ pos)))))

(defun br-class-level ()
  "Returns current class hierarchy level as an integer.
1 is the top level."
  (let* ((name (buffer-name))
	 (pos (string-match "[0-9]" name)))
    (and pos (string-to-int (substring name pos)))))

(defun br-listing-window-num ()
  "Return listing window number, lefmost is 1, non-listing window = 0."
  (let ((wind (selected-window))
	(ctr 0))
    (br-to-view-window)
    (while (not (eq wind (selected-window)))
      (other-window 1)
      (setq ctr (1+ ctr)))
    ctr))

(defun br-cleanup ()
  "Cleanup and free browser Environment data structures."
  (setq br-lang-prefix nil
	br-sys-paths-htable nil
	br-lib-paths-htable nil
	br-paths-htable nil
	br-sys-parents-htable nil
	br-lib-parents-htable nil
	br-parents-htable nil
	br-children-htable nil
	br-lib-prev-search-dirs nil
	br-sys-prev-search-dirs nil
	))

(defun br-clear ()
  "Re-initialize all browser listing buffer displays.
Leave point in browser top-level class listing buffer."
  (let ((n (max 1 (/ (frame-width) br-min-width-window))))
    (br-to-view-window)
    (other-window 1)
    (br-next-buffer 1)
    (while (> n 1)
      (setq n (1- n))
      (br-next-buffer nil br-buffer-prefix-blank))
    (br-to-view-window)
    (other-window 1)))

(defun br-descendant-trees (class-list &optional indent offset)
  "Insert descendant trees starting with classes from CLASS-LIST.
Indent each class in CLASS-LIST by optional INDENT spaces (default is 0 in
order to ensure proper initialization).  Offset each child level by optional
OFFSET spaces from its parent (which must be greater than zero, default 2)."
  (or indent (setq indent 0))
  (or offset (setq offset 2))
  (if (= indent 0) (setq br-tmp-class-set nil))
  (let ((prev-expansion-str " ...")
	children expand-subtree)
    (mapcar (function
	      (lambda (class)
		(setq expand-subtree (br-set-cons br-tmp-class-set class)
		      children (if expand-subtree (br-get-children class)))
		(indent-to indent)
		(insert class)
		(and (not expand-subtree) (br-has-children-p class)
		     (insert prev-expansion-str))
		(insert "\n")
		(if children
		    (br-descendant-trees children (+ indent offset) offset))))
	    class-list))
  (if (= indent 0) (setq br-tmp-class-set nil)))

(defun br-display-buffer (suffix)
  "Displays browser buffer ending in SUFFIX in current window."
  (let ((buf (get-buffer (concat br-buffer-prefix suffix))))
    (if buf (progn (set-window-buffer (selected-window) buf)))
    buf))

(defun br-do-in-view-window (form)
  "Evaluate FORM in viewer window and then return to current window."
  (interactive)
  (let ((wind (selected-window)))
    (unwind-protect
	(progn (br-to-view-window)
	       (eval form))
      (select-window wind))))

(defun br-edit-ext-start (editor-cmd name file)
  "Start an external viewer given by EDITOR-CMD using NAME applied to FILE."
  ;; Conditionalized code is necessary because of silly (start-process) calling
  ;; protocol.
  (cond (br-ed9
	 (start-process name name editor-cmd br-ed1 br-ed2 br-ed3 br-ed4
			br-ed5 br-ed6 br-ed7 br-ed8 br-ed9 file))
	(br-ed8
	 (start-process name name editor-cmd br-ed1 br-ed2 br-ed3 br-ed4
			br-ed5 br-ed6 br-ed7 br-ed8 file))
	(br-ed7
	 (start-process name name editor-cmd br-ed1 br-ed2 br-ed3 br-ed4
			br-ed5 br-ed6 br-ed7 file))
	(br-ed6
	 (start-process name name editor-cmd br-ed1 br-ed2 br-ed3 br-ed4
			br-ed5 br-ed6 file))
	(br-ed5
	 (start-process name name editor-cmd br-ed1 br-ed2 br-ed3 br-ed4
			br-ed5 file))
	(br-ed4
	 (start-process name name editor-cmd br-ed1 br-ed2 br-ed3 br-ed4
			file))
	(br-ed3
	 (start-process name name editor-cmd br-ed1 br-ed2 br-ed3 file))
	(br-ed2
	 (start-process name name editor-cmd br-ed1 br-ed2 file))
	(br-ed1
	 (start-process name name editor-cmd br-ed1 file))
	(t
	 (start-process name name editor-cmd file))
	))

(defun br-funcall-in-view-window (buffer function &optional no-erase)
  "Clear out BUFFER and display return value from invocation of FUNCTION in viewer window.
Move point to beginning of buffer and then return to current window.  BUFFER
may be a buffer name.
With optional NO-ERASE, buffer is not erased before function is called."
  (interactive)
  (let ((wind (selected-window)))
    (unwind-protect
	(progn (br-to-view-window)
	       (set-window-buffer (selected-window) (get-buffer-create buffer))
	       (let (buffer-read-only)
		 (if no-erase
		     (goto-char (point-min))
		   (erase-buffer))
		 (funcall function))
	       (goto-char (point-min)))
      (select-window wind))))

(defun br-file-to-viewer (filename)
  "Display FILENAME from OO-Browser source directory in browser viewer window.
FILENAME should not contain any path information."
  (br-funcall-in-view-window
   (concat br-buffer-prefix-info "Help")
   (function (lambda ()
	       (insert-file-contents (br-pathname filename))
	       (set-buffer-modified-p nil)))))

(defun br-in-browser ()
  "Return selected frame if the OO-Browser is active in it, else return nil."
  (cond ((not (eq br-in-browser (selected-frame))) nil)
	((one-window-p 'nomini)
	 (setq br-in-browser nil))
	(t br-in-browser)))


(defun br-in-top-buffer-p ()
  "Return t if point is in the top class listing buffer, else nil."
  (string-equal (br-buffer-level) "1"))

(defun br-in-view-window-p ()
  "Is point in a viewer window?"
  (br-non-listing-window-p))

(defun br-init ()
  "Initialization common to all OO-Browser invocations."
  (br-feature-tags-init))

(defun br-insert-classes (class-list &optional indent)
  "Insert CLASS-LIST in current buffer indented INDENT columns."
  (mapcar (function
	    (lambda (class-name)
	      (and indent (indent-to indent))
	      (and class-name (insert class-name "\n"))))
	  class-list))

(defun br-interrupt (&optional arg)
  (if (null arg)
      (mapcar
       (function
	(lambda (buf)
	  (set-buffer buf)
	  (if (or (eq major-mode 'br-mode) (br-browser-buffer-p))
	      (bury-buffer nil))))
       (buffer-list))
    (setq *br-save-wconfig* nil
	  *br-prev-wconfig* nil
	  *br-prev-listing-window* nil)
    (mapcar
     (function
      (lambda (buf)
	(set-buffer buf)
	(if (or (eq major-mode 'br-mode)
		(br-browser-buffer-p))
	    (progn (br-feature-clear-signatures)
		   (set-buffer-modified-p nil)
		   (kill-buffer (current-buffer))))))
     (buffer-list))
    (br-cleanup))
  (setq br-in-browser nil))

(defun br-mode ()
  "The major mode used by OO-Browser listing windows.
See the file \"br-help\" for browser usage information.
It provides the following keys: \\{br-mode-map}"
  (interactive)
  (use-local-map br-mode-map)
  (setq major-mode 'br-mode)
  (setq mode-name "OO-Browse")
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq case-fold-search t)
  (setq buffer-read-only t)
  (run-hooks 'br-class-list-hook)
  (run-hooks 'br-mode-hook))

(defun br-narrow-to-class ()
  (cond ((= (point-min) (point-max)) nil)
	((br-find-class-name)
	 (narrow-to-region (match-beginning 0) (match-end 0)))
	(t (error
	    "(OO-Browser):  'br-narrow-to-class', current entry is not a class"))))

(defun br-narrow-to-feature ()
  "Narrow buffer to current feature entry."
  (if (br-feature-at-p)
      (narrow-to-region (match-beginning 0) (match-end 0))
    (error
     "(OO-Browser):  'br-narrow-to-feature' no current feature.")))

(defun br-feature-at-p ()
  "Returns t iff point is on a feature listing line."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "[ \t]*" br-feature-entry))))

(defun br-next-buffer (&optional special alt-prefix)
  "Returns next sequential browser buffer or special one if optional SPECIAL is non-nil.
Non-nil ALT-PREFIX is used as prefix in buffer name."
  (let* ((suffix (or special (1+ (or (br-class-level) 0))))
	 (buf (get-buffer-create
	       (concat (or alt-prefix br-buffer-prefix)
		      (if (integerp suffix)
			  (int-to-string suffix)
			suffix)))))
    (if buf (progn
	      (or special (br-next-listing-window))
	      (set-window-buffer (selected-window) buf)
	      (let (buffer-read-only)
		(erase-buffer)
		(kill-all-local-variables)
		;; Clear out any feature tags that may have been associated
		;; with this buffer, so we don't mistakenly reference them.
		(br-feature-clear-signatures))
	      (setq mode-line-format (list "  %17b --" '(-3 . "%p") "-%-"))
	      (br-mode)
	      (br-set-mode-line)
	      (set-buffer-modified-p nil)))
    buf))

(defun br-next-listing-window (&optional prev)
  "Move to next browser listing window (non-viewer window).
Optional PREV means to previous window."
  (let ((owind (selected-window)))
    (while (progn (other-window (if prev -1 1))
		  (if (br-non-listing-window-p)
		      (not (eq (selected-window) owind)))))))

(defun br-pathname (filename)
  "Return full pathname for FILENAME in browser Elisp directory."
  (if br-directory
      (expand-file-name filename br-directory)
    (error "The 'br-directory' variable must be set to a string value.")))

(defun br-protocol-entry-p ()
  "Return non-nil if point is within a protocol listing entry line."
  (and (string-equal br-lang-prefix "objc-")
       (save-excursion
	 (beginning-of-line)
	 (looking-at "[ \t]*@ <[^\>]*>"))))

(defun br-resize (min-width)
  "Resize browser listing windows to have MIN-WIDTH."
  (interactive)
  (let* ((window-min-width 3)
	 (oldn (1- (length (br-window-list))))
	 (n (max 1 (/ (frame-width) min-width)))
	 (numw n)
	 (diff (- numw oldn))
	 (width (/ (frame-width) numw))
	 (obuf (current-buffer)))
    (br-to-first-list-window)
    (cond ((= diff 0)
	   (br-resize-windows numw width))
	  ((> diff 0)
	   (setq n oldn)
	   (while (> n 1)
	     (setq n (1- n))
	     (shrink-window-horizontally (max 0 (- (window-width)
						   min-width)))
	     (br-next-listing-window))
	   (setq n diff)
	   (while (> n 0)
	     (setq n (1- n))
	     (split-window-horizontally (max window-min-width
					     (- (window-width)
						min-width)))) 
	   (setq n oldn)
	   (while (< n numw)
	     (setq n (1+ n))
	     (br-next-listing-window)
	     (br-next-buffer n br-buffer-prefix-blank))
	   (br-to-first-list-window)
	   (br-resize-windows numw width)
	   )
	  (t  ;; (< diff 0)
	   (while (> n 0)
	     (setq n (1- n))
	     (br-next-listing-window))
	   (setq n (- diff))
	   (while (> n 0)
	     (setq n (1- n))
	     (delete-window))
	   (br-to-first-list-window)
	   (br-resize-windows numw width)
	   ))
    (setq br-min-width-window min-width)
    (let ((owind (get-buffer-window obuf)))
      (if owind
	  (select-window owind)
	(br-to-view-window)
	(br-next-listing-window)))))

(defun br-resize-narrow ()
  "Resize listing windows so are narrower by 10 characters."
  (interactive)
  (if (<= window-min-width (- br-min-width-window 10))
      (br-resize (max window-min-width (- br-min-width-window 10)))
    (beep)))

(defun br-resize-widen ()
  "Resize listing windows so are wider by 10 characters."
  (interactive)
  (if (and (>= (frame-width) (+ br-min-width-window 10))
	   (> (length (br-window-list)) 2))
      (br-resize (min (frame-width) (+ br-min-width-window 10)))
    (beep)))

(defun br-resize-windows (n width)
  (while (> n 1)
    (setq n (1- n))
    (shrink-window-horizontally (- (window-width) width))
    (br-next-listing-window)))

(defun br-set-mode-line ()
  "Set mode line string."
  (setq mode-line-buffer-identification (list (buffer-name)))
  (set-buffer-modified-p t))

(defun br-show-top-classes (func &optional uniq)
  "Display list of top level classes generated by calling FUNC.
Optional UNIQ means sort and eliminate duplicates."
  (message "Ordering classes...")
  (let ((classes (funcall func)))
    (setq classes (br-class-list-filter classes))
    (br-clear)
    (let (buffer-read-only)
      (erase-buffer)
      (br-insert-classes classes)
      (if uniq
	  (progn
	    (if (stringp br-sort-options)
		(call-process-region (point-min) (point-max) "sort" t t nil
				     br-sort-options)
	      (call-process-region (point-min) (point-max) "sort" t t nil))
	    (if (and (stringp br-sort-options)
		     (string-match "u" br-sort-options))
		;; Then sort made the list of elements unique, so do nothing.
		nil
	      (call-process-region (point-min) (point-max) "uniq" t t))))))
  (goto-char (point-min))
  (message "Ordering classes...Done"))

(defun br-this-level-classes (&optional keep-indent)
  "Return list of the classes in the current listing.
Optional KEEP-INDENT non-nil means keep indentation preceding class name."
  (let ((classes))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (looking-at "^[ \t]*$"))
		  (if (looking-at (format "^[ \t]*%s "
					  br-feature-type-regexp)) ;; a feature
		      t ;; skip this entry
		    ;; assume is a class
		    (setq classes (cons (br-find-class-name keep-indent)
					classes)))
		  (= (forward-line 1) 0))))
    (nreverse (delq nil classes))))

(defun br-this-level-entries ()
  "Return list of all entries in the current listing."
  (let ((entries))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (looking-at "^[ \t]*$"))
		  (if (looking-at (format "^[ \t]*%s "
					  br-feature-type-regexp)) ;; a feature
		      (setq entries
			    (cons (br-find-feature-entry) entries))
		    ;; assume is a class
		    (setq entries (cons (br-find-class-name) entries)))
		  (= (forward-line 1) 0))))
    (nreverse (delq nil entries))))

(defun br-this-level-features ()
  "Return list of features in the current listing."
  (let ((feature-regexp (concat "[ \t]*" br-feature-entry))
	(features))
    (save-excursion
      (goto-char (point-min))
      (while (progn (if (looking-at feature-regexp)
			(setq features
			      (cons (br-find-feature-entry) features)))
		    (= (forward-line 1) 0))))
    (nreverse (delq nil features))))

(defun br-to-first-list-window ()
  (br-to-view-window)
  (br-next-listing-window))

(defun br-to-tree ()
  "If point is within ... move to inher/ancestry expansion for the current class."
  (if (save-excursion
	(skip-chars-backward ".")
	(looking-at "\\.\\.\\."))
      (progn (beginning-of-line)
	     (let ((class-expr (concat "^[ \t]*"
				       (br-find-class-name)
				       "$")))
	       (if (re-search-backward class-expr nil t)
		   (progn (skip-chars-forward " \t")
			  (recenter '(4))
			  t))))))

(defun br-to-view-window ()
  "Move to viewer window."
  (if (br-in-view-window-p)
      nil
    (setq *br-prev-listing-window* (selected-window))
    (while (and (not (br-in-view-window-p))
		(progn (other-window 1)
		       (not (eq (selected-window)
				*br-prev-listing-window*)))))))

(defun br-window-setup ()
  (and (fboundp 'modify-frame-parameters)
       (cdr (assq 'unsplittable (frame-parameters)))
       (modify-frame-parameters (selected-frame) '((unsplittable))))
  (delete-other-windows)
  ;; Set top of frame line in case it is not 0.
  (or (fboundp 'window-highest-p)
      (setq br-top-of-frame (nth 1 (window-edges))))
  (split-window-vertically nil)
  (let* ((n (max 1 (/ (frame-width) br-min-width-window)))
	 (width (/ (frame-width) n)))
    (br-next-buffer 1)
    (while (> n 1)
      (setq n (1- n))
      (split-window-horizontally width)
      (br-next-buffer nil br-buffer-prefix-blank))))

(defun br-view-ext-start (viewer-cmd name file)
  "Start an external viewer given by VIEWER-CMD using NAME applied to FILE."
  ;; Conditionalized code is necessary because of silly (start-process) calling
  ;; protocol.
  (cond (br-vw9
	 (start-process name name viewer-cmd br-vw1 br-vw2 br-vw3 br-vw4
			br-vw5 br-vw6 br-vw7 br-vw8 br-vw9 file))
	(br-vw8
	 (start-process name name viewer-cmd br-vw1 br-vw2 br-vw3 br-vw4
			br-vw5 br-vw6 br-vw7 br-vw8 file))
	(br-vw7
	 (start-process name name viewer-cmd br-vw1 br-vw2 br-vw3 br-vw4
			br-vw5 br-vw6 br-vw7 file))
	(br-vw6
	 (start-process name name viewer-cmd br-vw1 br-vw2 br-vw3 br-vw4
			br-vw5 br-vw6 file))
	(br-vw5
	 (start-process name name viewer-cmd br-vw1 br-vw2 br-vw3 br-vw4
			br-vw5 file))
	(br-vw4
	 (start-process name name viewer-cmd br-vw1 br-vw2 br-vw3 br-vw4
			file))
	(br-vw3
	 (start-process name name viewer-cmd br-vw1 br-vw2 br-vw3 file))
	(br-vw2
	 (start-process name name viewer-cmd br-vw1 br-vw2 file))
	(br-vw1
	 (start-process name name viewer-cmd br-vw1 file))
	(t
	 (start-process name name viewer-cmd file))
	))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar br-ancestor-function nil
  "If non-nil, a function of 3 arguments called after each ancestor class is inserted into an ancestry listing.
First argument is the class just inserted, second argument is a flag
indicating whether class has previously been displayed within the listing and
third argument is the number of spaces to indent each feature entry for this
class.")

(defvar br-top-of-frame 0
  "Frame line number of windows at top of the OO-Browser frame.")

(defvar br-ed-num 0)
(defvar br-ed-name "extEd")
(defvar br-vw-num 0)
(defvar br-vw-name "extVw")

(defvar br-in-browser nil
  "Equal to the frame displaying the OO-Browser when in use, else nil.")

(defvar br-lib-search-dirs nil
  "List of directories below which OO source files and other library
directories are found.  A library is a stable group of OO classes.  Do not
set this variable directly.  Each OO language library which invokes
'br-browse' should set it.")

(defvar br-sys-search-dirs nil
  "List of directories below which OO source files and other system
directories are found.  A system is a group of OO classes that are likely to
change.  Do not set this variable directly.  Each OO language library which
invokes 'br-browse' should set it.")

(defvar *br-level-hist* nil
  "Internal history of visited listing windows and buffers.")

(defvar *br-prev-listing-window* nil
  "Saves listing window used prior to viewer window entry.
Allows return to previous listing window when done with the viewer.")

(defvar *br-prev-wconfig* nil
  "Saves window configuration prior to browser entry.")

(defvar *br-save-wconfig* nil
  "Saves window configuration between invocations of the browser.")

(defconst br-buffer-prefix-categ "Categ-Lvl-")
(defconst br-buffer-prefix-inher "Inher-Lvl-")
(defconst br-buffer-prefix-blank "Blank-")
(defconst br-buffer-prefix-info "OO-Browser ")
(defvar br-buffer-prefix br-buffer-prefix-inher
  "Browser buffer name prefix.")


(defvar br-mode-map nil
  "Keymap containing OO-Browser commands.")
(if br-mode-map
    nil
  (setq br-mode-map (make-keymap))
  (suppress-keymap br-mode-map)
  (define-key br-mode-map "@"        'br-at)
  (define-key br-mode-map "1"        'br-view-full-frame)
  (define-key br-mode-map "\C-c^"    'br-add-class-file)
  (define-key br-mode-map "a"        'br-ancestors)
  (define-key br-mode-map "b"        'br-buffer-menu)
  (define-key br-mode-map "\C-c\C-b" 'br-report-bug)
  (define-key br-mode-map "c"        'br-children)
  (define-key br-mode-map "C"        'br-categories)
  (define-key br-mode-map "\M-c"     'br-class-stats)
  (define-key br-mode-map "\C-c\C-c" 'br-env-create)
  (define-key br-mode-map "d"        'br-descendants)
  (define-key br-mode-map "\C-c\C-d" 'br-delete)
  ;; {M-d} is used down below for 'br-tree'
  (define-key br-mode-map "e"        'br-edit-entry)
  (define-key br-mode-map "\M-e"     'br-env-stats)
  (define-key br-mode-map "\C-c\C-e" 'br-env-rebuild)
  (define-key br-mode-map "f"        'br-features)
  (define-key br-mode-map "F"        'br-feature-signature)
  ;; {M-f} is used down below for 'br-tree-features-toggle'
  ;; {M-g} is used down below for 'br-tree-graph'
  (define-key br-mode-map "?"        'br-help)
  (define-key br-mode-map "h"        'br-help)
  (define-key br-mode-map "H"        'br-help-ms) ;; mouse help
  (define-key br-mode-map "i"        'br-entry-info)
  (define-key br-mode-map "I"        'br-implementors)
  (define-key br-mode-map "\C-c\C-k" 'br-kill)
  ;; {M-k} is used down below for 'br-tree-kill'
  (define-key br-mode-map "l"        'br-lib-top-classes)
  (define-key br-mode-map "L"        'br-lib-rebuild)
  (define-key br-mode-map "\C-c\C-l" 'br-env-load)
  (define-key br-mode-map "m"        'br-match)
  (define-key br-mode-map "M"        'br-match-entries)
  ;; "\C-c\C-m" is reserved for future use.
  (define-key br-mode-map "\C-n"     'br-next-entry)
  (define-key br-mode-map "o"        'br-order)
  (define-key br-mode-map "p"        'br-parents)
  (define-key br-mode-map "P"        'br-protocols)
  (define-key br-mode-map "\C-p"     'br-prev-entry)
  (define-key br-mode-map "q"        'br-quit)
  ;; {r} does the same thing as {f} and is for backward compatibility
  ;; with prior OO-Browser releases.  It may be rebound in the future, so
  ;; learn to use {f} instead.
  (define-key br-mode-map "r"        'br-features)
  (define-key br-mode-map "\C-c\C-r" 'br-refresh)
  (define-key br-mode-map "s"        'br-sys-top-classes)
  (define-key br-mode-map "S"        'br-sys-rebuild)
  (define-key br-mode-map "\C-c\C-s" 'br-env-save)
  (define-key br-mode-map "t"        'br-top-classes)
  (define-key br-mode-map "u"        'br-unique)
  (define-key br-mode-map "v"        'br-view-entry)
  (define-key br-mode-map "V"        'br-view-friend)
  (define-key br-mode-map "\C-c\C-v" 'br-to-from-viewer)
  (define-key br-mode-map "\C-c\C-w" 'br-write-buffer)
  (define-key br-mode-map "w"        'br-where)
  (define-key br-mode-map "x"        'br-exit-level)
  (define-key br-mode-map "\C-x-"    'br-resize-narrow)
  (define-key br-mode-map "\C-x+"    'br-resize-widen)
  (define-key br-mode-map "#"        'br-count)
  (define-key br-mode-map "\C-c#"    'br-version)
  (define-key br-mode-map " "        'br-viewer-scroll-up)
  (define-key br-mode-map "\177"     'br-viewer-scroll-down)
  ;;
  ;; Define graphical browser keys if a window system is available.
  (if hyperb:window-system
      (progn (require 'br-tree)
	     (define-key br-mode-map "\M-d" 'br-tree)
	     (define-key br-mode-map "\M-f" 'br-tree-features-toggle)
	     (define-key br-mode-map "\M-g" 'br-tree-graph)
	     (define-key br-mode-map "\M-k" 'br-tree-kill))))

(defvar br-tmp-class-set nil
  "Set of classes created for temporary use by br-*-trees functions.")
(defvar br-tmp-depth 0
  "Temporary variable indicating inheritance depth of class in 'br-ancestor-trees'.")

(provide 'br)
