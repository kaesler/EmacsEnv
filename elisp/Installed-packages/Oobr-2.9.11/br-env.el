;;!emacs
;;
;; FILE:         br-env.el
;; SUMMARY:      OO-Browser Environment support functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:     8-Jun-90
;; LAST-MOD:     20-Sep-95 at 14:59:03 by Bob Weiner
;;
;; Copyright (C) 1989-1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hasht)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar br-env-default-file "OOBR"
  "*Standard file name for OO-Browser Environment storage.")

(defvar br-env-file nil
  "Default file into which to save a class Environment.
Value is language-specific.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(if (fboundp 'file-relative-name)
    nil
  ;; For V18 Emacs
  (defun file-relative-name (filename &optional directory)
    "Convert FILENAME to be relative to DIRECTORY (default: default-directory)."
    (setq filename (expand-file-name filename)
	  directory (file-name-as-directory (if directory
						(expand-file-name directory)
					      default-directory)))
    (while directory
      (let ((up (file-name-directory (directory-file-name directory))))
	(cond ((and (string-equal directory up)
		    (file-name-absolute-p directory))
	       ;; "/"
	       (setq directory nil))
	      ((string-match (concat "\\`" (regexp-quote directory))
			     filename)
	       (setq filename (substring filename (match-end 0)))
	       (setq directory nil))
	      (t
	       ;; go up one level
	       (setq directory up)))))
    filename))

;;;###autoload
(defun br-env-browse (env-file)
  "Invoke the OO-Browser on an existing or to be created Environment ENV-FILE."
  (interactive
   (list (read-file-name "Load/Create OO-Browser Environment: "
			 nil (or br-env-file br-env-default-file))))
  (if (stringp env-file)
	(setq env-file (expand-file-name env-file))
    (error "(br-env-browse): Invalid env file: '%s'" env-file))
  (if (string-match "-FTR$" env-file)
      (setq env-file (substring env-file 0 (match-beginning 0))))
  (cond ((and (file-exists-p env-file)
	      (not (file-readable-p env-file)))
	 (error "(br-env-browse): Env file '%s' is unreadable." env-file))
	((not (file-exists-p env-file))
	 ;; Specify a new Environment
	 (funcall (intern-soft (concat (br-env-select-lang) "browse"))
		  env-file))
	(t ;; Existing Environment
	 (let ((lang-string))
	   (save-excursion
	     (set-buffer (find-file-noselect env-file))
	     (save-restriction
	       (widen)
	       (goto-char (point-min))
	       (if (search-forward "br-lang-prefix" nil t)
		   (progn (forward-line 1)
			  ;; Eval removes quote from in front of lang-string
			  ;; value which is read from the Env file.
			  (setq lang-string (eval (read (current-buffer))))))))
	   (if lang-string
	       (funcall (intern-soft (concat lang-string "browse"))
			env-file)
	     (error "(br-env-browse): Invalid env file: '%s'" env-file))))))

(defun br-env-build (&optional env-file background-flag)
  "Build Environment from spec given by optional ENV-FILE or 'br-env-file'.
If optional 2nd argument BACKGROUND-FLAG is t, build the Environment
using a background process.  If it is nil, build in foreground.  Any other
value prompts for whether to build in the background."
  (interactive
   (let ((env-file (br-env-default-file)))
     (list (read-file-name
	    (format "Build Environment (default \"%s\"): "
		    (br-relative-path env-file))
	    (file-name-directory env-file)
	    env-file t)
	   'prompt)))
  (cond ((or (null background-flag) (eq background-flag t)))
	(noninteractive
	 (setq background-flag nil))
	(t (setq background-flag
		 (y-or-n-p "Build Environment in a background process? "))))
  (if (or (not (stringp env-file)) (equal env-file ""))
      (setq env-file br-env-file))
  (setq env-file (expand-file-name env-file))
  (or (not (file-exists-p env-file)) (file-readable-p env-file)
      (error (format "Non-readable Environment file, %s" env-file)))
  (or (file-writable-p env-file)
      (error (format "Non-writable Environment file, %s" env-file)))
  (if background-flag
      (progn (setenv "OOBR_DIR" br-directory)
	     (setenv "OOBR_ENV" env-file)
	     (compile (format
		       "make -f %s %s oobr-env"
		       (expand-file-name "Makefile" br-directory)
		       (if (and (boundp 'invocation-directory)
				(boundp 'invocation-name)
				(stringp invocation-directory)
				(stringp invocation-name)
				(file-directory-p invocation-directory)
				(file-name-absolute-p invocation-directory))
			   (concat "EMACS="
				   (expand-file-name
				    invocation-name invocation-directory))
			 ""))))
    (br-env-load env-file nil t)
    ;; Detach unneeded data so can be garbage collected.
    (br-env-create-alists)
    (br-env-create-htables)
    (if (and (boundp 'br-feature-tags-file) (stringp br-feature-tags-file))
	(progn
	  (if (not (file-writable-p br-feature-tags-file))
	      (error
	       "(br-env-build): %s is not writable" br-feature-tags-file))
	  (set-buffer (find-file-noselect br-feature-tags-file))
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (set-buffer-modified-p nil)))
    (br-build-sys-htable)
    (br-build-lib-htable)
    (setq br-env-spec nil)
    (br-env-save)
    ;; Detach unneeded data so can be garbage collected.
    (br-env-create-alists)
    (br-env-load env-file nil t)))

(defun br-env-rebuild ()
  "Rescan System and Library sources associated with the current Environment."
  (interactive)
  (cond ((interactive-p)
	 (if (y-or-n-p "Rebuild current Environment? ")
	     (br-env-build nil 'prompt)))
	(t (error "(br-env-rebuild): This must be called interactively."))))

(defun br-env-create (&optional env-file lang-prefix)
  "Create and save the specification of a new OO-Browser Environment.
Interactively prompt for the Environment file name or use optional ENV-FILE.
Interactively prompt for the Environment language to use or use optional
LANG-PREFIX as language indicator.

If called non-interactively, do not build the Environment.
If called interactively and presently in the OO-Browser and the current
Environment is the one that has been re-specified, automatically rebuild it.
Otherwise, prompt for whether to build the Environment. 

Return the name of the Environment specification file that was created."
  (interactive)
  (if env-file
      (read-string
	(format "Please specify the \"%s\" Environment (Hit RTN to begin)."
		(file-name-nondirectory env-file)))
    (setq env-file (br-env-default-file)
	  env-file (read-file-name
		    (format "Create Env spec file (default \"%s\"): "
			    (br-relative-path env-file))
		    (file-name-directory env-file)
		    env-file nil)))
  (setq env-file (expand-file-name env-file))
  ;; Display Env spec if previous one existed
  (and (equal env-file br-env-file) (file-readable-p env-file) (br-env-stats))
  (let ((prompt "System search dir #%d (RTN to end): ")
	(br-env-spec t)
	br-sys-search-dirs br-lib-search-dirs
	br-lang-prefix
	br-children-htable
	br-sys-paths-htable
	br-sys-parents-htable
	br-lib-paths-htable
	br-lib-parents-htable
	br-paths-htable
	br-parents-htable)
    (br-env-create-htables)
    (setq br-lang-prefix (or lang-prefix (br-env-select-lang))
	  br-sys-search-dirs (br-env-get-dirs prompt)
	  prompt "Library search dir #%d (RTN to end): "
	  br-lib-search-dirs (br-env-get-dirs prompt))
    ;; Now since user has not aborted, set real variables
    (setq br-env-spec t)
    (br-env-save env-file)
    ;; If called interactively and re-specifying current Env, then also
    ;; rebuild it.
    (if (interactive-p)
	(if (equal env-file br-env-file)
	    (if (br-in-browser)
		;; auto-build
		(br-env-build
		 nil (y-or-n-p "Environment will now be built.  Build in background? "))
	      (call-interactively 'br-env-build))))
    env-file))

;;;###autoload
(defun br-env-load (&optional env-file prompt no-build)
  "Load browser Environment or spec from optional ENV-FILE or 'br-env-file'.
Non-nil PROMPT means prompt user before building tables.
Non-nil NO-BUILD means skip build of Environment entirely.
Return t if load is successful, else nil."
  (interactive
   (let ((env-file (br-env-default-file)))
     (list (read-file-name
	    (format "Environment file to load (default \"%s\"): "
		    (br-relative-path env-file))
	    (file-name-directory env-file)
	    env-file t))))
  (setq env-file (or (and (not (equal env-file "")) env-file)
		     (br-env-default-file))
	env-file (expand-file-name env-file)
	br-env-file env-file)
  (let ((buf (get-file-buffer env-file)))
    (and buf (kill-buffer buf)))
  (let ((br-loaded))
    (if (file-readable-p env-file)
	(unwind-protect
	    (progn
	      (message "Loading Environment...")
	      (sit-for 1)
	      ;; Ensure spec and version values are nil for old
	      ;; Environment files that do not contain a setting for
	      ;; these variables.
	      (setq br-env-spec nil br-env-version nil)
	      (load-file env-file)

	      (if br-env-spec
		  nil
		(setq br-children-htable (hash-make br-children-alist)
		      br-sys-paths-htable (hash-make br-sys-paths-alist)
		      br-lib-paths-htable (hash-make br-lib-paths-alist)
		      br-sys-parents-htable
		      (hash-make br-sys-parents-alist)
		      br-lib-parents-htable
		      (hash-make br-lib-parents-alist)
		      )
		(br-env-set-htables))

	      ;; Prevent rebuilding of Environment
	      (setq br-lib-prev-search-dirs br-lib-search-dirs
		    br-sys-prev-search-dirs br-sys-search-dirs)
	      (setq br-loaded t)
	      (message "Loading Environment...Done")
	      (cond
	       ((and br-env-spec (not no-build))
		(setq br-loaded
		      (br-env-cond-build
		       env-file
		       (if prompt "Build Environment from spec in file, \"%s\"? "))))
	       ;; If Environment was built with a version of the OO-Browser
	       ;; which did not add a version number to each Environment,
	       ;; then it may use an obsolete format.  Offer to rebuild it.
	       ((and (not no-build) (null br-env-version)
		     (br-member br-lang-prefix '("c++-" "objc-" "eif-")))
		(br-env-stats)
		(br-env-cond-build
		 env-file
		 (if prompt
		     "Environment file format is obsolete, rebuild it? ")))))
	  nil)
      (if (file-exists-p env-file)
	  (progn (beep)
		 (message "No read rights for Envir file, \"%s\"" env-file)
		 (sit-for 4))
	(message "\"%s\", no such file." env-file)
	(sit-for 2)
	(setq br-loaded (br-env-load
			 (br-env-create env-file br-lang-prefix) t))))
    br-loaded))

(defun br-env-save (&optional save-file)
  "Save changed Environment to file given by optional SAVE-FILE or 'br-env-file'."
  (interactive
   (let ((env-file (br-env-default-file)))
     (list (read-file-name
	    (format "Save Environment to (default \"%s\"): "
		    (br-relative-path env-file))
	    (file-name-directory env-file)
	    env-file nil))))
  (if (and (stringp save-file)
	   (not (equal save-file br-env-file))
	   (stringp br-feature-tags-file)
	   (file-exists-p br-feature-tags-file))
      ;; Copy feature tags file to new file name.
      (copy-file br-feature-tags-file (br-feature-tags-file-name save-file)
		 t t))
  (if (or (not (stringp save-file)) (equal save-file ""))
      (setq save-file br-env-file))
  (setq save-file (expand-file-name save-file))
  (or (file-writable-p save-file)
      (error (format "Non-writable Environment file, \"%s\""
		     save-file)))
  (let ((buf (get-file-buffer save-file)))
    (and buf (kill-buffer buf)))
  (let ((dir (or (file-name-directory save-file)
		 default-directory)))
    (or (file-writable-p dir)
	(error (format "Non-writable Environment directory, \"%s\"" dir))))
  (save-window-excursion
    (let ((standard-output
	    (set-buffer (funcall br-find-file-noselect-function
				 save-file)))
	  (buffer-read-only)
	  br-sym)
      (erase-buffer)
      (princ "\n(setq\nbr-env-version")
      (print br-version)
      (br-env-save-mult-vars (cons (car br-env-mult-vars) nil))
      (mapcar (function
		(lambda (nm)
		  (setq br-sym (intern-soft (concat "br-" nm)))
		  (let ((nm-mid (string-match "-htable$" nm)))
		    (if nm-mid
			(progn (princ "\nbr-") (princ (substring nm 0 nm-mid))
			       (princ "-alist\n'")
			       (hash-prin1 (symbol-value br-sym)))
		      (princ "\n") (princ br-sym) (princ "\n'")
		      (prin1 (symbol-value br-sym)) (princ "\n")))))
	      br-env-single-vars)
      (br-env-save-mult-vars (cdr br-env-mult-vars))
      (princ ")\n")
      (save-buffer)
      (kill-buffer standard-output))))

(defun br-env-stats (&optional arg)
  "Display summary for current Environment in viewer window.
With optional prefix ARG, display class totals in minibuffer."
  (interactive "P")
  (let ((env-file (abbreviate-file-name br-env-file)))
    (if arg
	(message "Envir \"%s\": %s" env-file (br-env-totals))
      (br-funcall-in-view-window
       (concat br-buffer-prefix-info "Info")
       (function
	(lambda ()
	  (insert (format "Environment: \"%s\"" env-file))
	  (center-line)
	  (insert "\n\n")
	  (if (null br-env-spec)
	      (insert (format "Built by version %s of the OO-Browser.\n\n"
			      (or br-env-version "earlier than 02.09.03"))))
	  (insert (br-env-totals) "\n\n")
	  (let ((undefined (br-undefined-classes)))
	    (if undefined
		(insert (format "Undefined classes: %s\n\n" undefined))))
	  (mapcar
	   (function
	    (lambda (sys-lib)
	      (insert (format "Directories to search for %s classes:\n"
			      (car sys-lib)))
	      (if (cdr sys-lib)
		  (progn (mapcar
			  (function
			   (lambda (dir)
			     (or (equal dir "")
				 (insert
				  (format "\t%s\n"
					  (abbreviate-file-name dir))))))
				 (cdr sys-lib))
			 (insert "\n"))
		(insert "\t<None>\n\n"))))
	   (list (cons "System" br-sys-search-dirs)
		 (cons "Library"  br-lib-search-dirs)))
	  (insert "Flag Settings:"
		  "\n\tEnvironment built from specification: "
		  (if br-env-spec "no" "yes")
		  "\n")
	  (set-buffer-modified-p nil)))))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun br-env-add-ref-classes (&optional htable-type)
  "Add classes to Environment which are referenced in it but not defined.
With optional HTABLE-TYPE, affect only that part of the Environment.
HTABLE-TYPE may be \"sys\"or \"lib\".  By default, add to both Library and
whole Environment tables."
  ;;
  ;; This function must NOT call any 'get-htable' type functions or it will
  ;; cause an infinite loop.
  (let ((classes (br-all-classes
		  (symbol-value
		   (intern-soft (concat "br-" htable-type
					(if htable-type "-")
					"paths-htable")))))
	(pars (br-env-all-parents
	       (symbol-value
		(intern-soft (concat "br-" htable-type
				     (if htable-type "-")
				     "parents-htable")))))
	(class))
    (while pars
      (setq class (car pars)
	    pars (cdr pars))
      (if (or (null class) (br-member class classes))
	  nil
	(setq classes (cons class classes))
	(if (null htable-type) (setq htable-type "lib"))
	(br-env-add-to-htables class (concat htable-type "-parents"))
	(br-add-to-paths-htable
	 class br-null-path
	 (br-get-htable (concat htable-type "-paths")))))))

(defun br-env-add-to-htables (class parents)
  "Add CLASS to hash tables referenced by PARENTS name.
PARENTS may be \"parents\", \"sys-parents\", or \"lib-parents\"."
  (if (null class)
      nil
    (setq parents
	  (symbol-value (intern-soft (concat "br-" parents "-htable"))))
    (if parents (hash-add nil class parents))))

(defun br-env-all-parents (&optional htable-type)
  "Return list of all parent names in Environment or optional HTABLE-TYPE.
HTABLE-TYPE may be \"sys\" or \"lib\". or an actual hash table."
  (apply 'append
	 (hash-map 'car
		   (cond ((and (stringp htable-type)
			       (not (string-equal htable-type "")))
			  (br-get-htable (concat htable-type "-parents")))
			 ((hashp htable-type) htable-type)
			 (t (br-get-parents-htable))))))

(defun br-env-batch-build ()
  "Build Environments from specifications while running Emacs in batch mode.
Invoke via a shell command line of the following form:
emacs -batch -l <BR-DIR>/br-start.el <OO-Browser Env Spec File> ... <Spec File> -f br-env-batch-build"
  (br-init-autoloads)
  (if (or (not (boundp 'br-directory)) (null br-directory)
	  (not (file-exists-p br-directory)))
      (error "br-env-batch-build: Set 'br-directory' properly before use.")
    (let ((spec-file)
	  (files (delq nil (mapcar 'buffer-file-name (buffer-list)))))
      (while (setq spec-file (car files))
	(setq files (cdr files))
	(load spec-file)
	(or (featurep (intern-soft (concat br-lang-prefix "browse")))
	    (featurep (intern-soft (concat br-lang-prefix "brows")))
	    (load (expand-file-name
		   (concat br-lang-prefix "browse") br-directory)
		  t)
	    (load (expand-file-name
		   (concat br-lang-prefix "brows") br-directory)))
	(funcall (intern (concat br-lang-prefix "browse-setup")))
	(kill-buffer nil)
	(br-env-build spec-file nil)))))

;;; The following function is called by the compilation sentinel whenever a
;;; compilation finishes under versions of Emacs 19.  (If you use Emacs 18,
;;; you would have to edit compilation-sentinel to call the function stored
;;; in 'compilation-finish-function' as Emacs 19, compile.el does.
;;;
;;; If there already is a compilation-finish-function, save it and use it
;;; when not in a batch environment build.
(setq compilation-original-finish-function
      (and (boundp 'compilation-finish-function)
	   (not (eq compilation-finish-function 'br-env-batch-build-browse))
	   compilation-finish-function)
      compilation-finish-function 'br-env-batch-build-browse)

(defun br-env-batch-build-browse (&rest args)
  ;; This is only called when we are in the compilation buffer already.
  (cond ((not (string-match "oobr-env" compile-command))
	 ;; Some other type of build.
	 (if compilation-original-finish-function
	     (apply compilation-original-finish-function args)))
	((not (and (stringp mode-line-process)
		   (string-match "OK" mode-line-process)))
	 ;; Build failed.
	 nil)
	(t ;; Environment build was successful.
	 (beep)
	 (let* ((env-file (getenv "OOBR_ENV"))
		(prompt
		 (format
		  "(OO-Browser): Environment \"%s\" is built; browse it now? "
		  (file-name-nondirectory env-file))))
	   (if (y-or-n-p prompt)
	       (br-env-browse env-file))))))

(defun br-env-cond-build (env-file prompt)
  "Build current Environment from its specification and save it in ENV-FILE.
Non-nil PROMPT is used to prompt user before building Environment.  Return t
iff current Environment gets built from specification."
  (let ((dir (or (file-name-directory env-file)
		 default-directory)))
    (if (not (file-writable-p dir))
	(progn (beep)
	       (message "Unwritable Environment directory, \"%s\"" dir)
	       (sit-for 4) nil)
      (if (or (not prompt)
	      (y-or-n-p (format prompt env-file)))
	  (progn (br-env-build env-file 'prompt) t)))))

(defun br-env-copy (to-br)
  "Copy 'br-' Environment to or from 'br-lang-prefix' language variables.
If TO-BR is non-nil, copy from language-specific variables to browser
variables.  Otherwise, do copy in the reverse direction."
  (let* ((var1) (var2)
	 (copy-func
	  (if to-br (function (lambda () (set var1 (symbol-value var2))))
	    (function (lambda () (set var2 (symbol-value var1)))))))
    (mapcar (function
	      (lambda (nm)
	       (setq var1 (intern (concat "br-" nm))
		     var2 (intern (concat br-lang-prefix nm)))
	       (funcall copy-func)))
	    (append
	      '("env-file" "env-version" "lib-search-dirs"
		"lib-prev-search-dirs" "lib-parents-htable"
		"lib-paths-htable" "sys-search-dirs"
		"sys-prev-search-dirs" "sys-parents-htable"
		"sys-paths-htable" "paths-htable" "parents-htable")
	      br-env-single-vars))))

(defun br-env-create-alists ()
  "Create all empty Environment association lists."
  (setq br-children-alist    nil
	br-sys-paths-alist   nil  br-lib-paths-alist nil
	br-sys-parents-alist nil  br-lib-parents-alist nil
	br-paths-alist       nil  br-parents-alist nil))

(defun br-env-create-htables ()
  "Create all empty Environment hash tables."
  (setq br-children-htable (hash-make 0)
	br-sys-paths-htable (hash-make 0)
	br-sys-parents-htable (hash-make 0)
	br-lib-paths-htable (hash-make 0)
	br-lib-parents-htable (hash-make 0)
	br-paths-htable (hash-make 0)
	br-parents-htable (hash-make 0)))

(defun br-env-default-file (&optional directory)
  "Search up current or optional DIRECTORY tree for an OO-Browser environment file.
Return file name found, the value of 'br-env-file' if non-nil, or else the
value of 'br-env-default-file'.  All return values are expanded to absolute
paths before being returned."
  (let ((path directory)
	(oobr-file))
    (while (and (stringp path)
		(setq path (file-name-directory path))
		(setq path (directory-file-name path))
		;; Not at root directory
		(not (string-match ":?/\\'" path))
		;; No environment file
		(not (file-exists-p
		      (setq oobr-file (expand-file-name
				       br-env-default-file path)))))
      (setq oobr-file nil))
    (expand-file-name (or oobr-file br-env-file br-env-default-file))))

(defun br-env-file-sym-val (symbol-name)
  "Given a SYMBOL-NAME, a string, find its value in the current Environment file.
Assume the Environment file to use is attached to the current buffer.
Only search for the SYMBOL-NAME from the current point in the buffer.
Return cons whose car is t iff SYMBOL-NAME was found and then whose cdr is the
non-quoted value found."
  (set-buffer (funcall br-find-file-noselect-function br-env-file))
  (save-excursion
    (if (search-forward symbol-name nil t)
	(let ((standard-input (current-buffer)))
	  (cons t (eval (read)))))))

(defun br-env-try-load (env-file default-file)
  "Try to load a complete Environment, initially given by ENV-FILE.
If an Environment specification is selected, the user will be prompted
whether or not to build it.  If ENV-FILE is not a string, the function will
prompt for an Environment to load.  DEFAULT-FILE is the default file to use
when an empty value is given at the Environment file prompt.

Return the name of the Environment file that was loaded or nil."
  (if (br-env-load
       (if (stringp env-file)
	   env-file
	 (or (stringp default-file)
	     (setq default-file (br-env-default-file)))
	 (setq env-file
	       (read-file-name
		(format
		 "OO-Browser Environment file (default \"%s\"): "
		 (br-relative-path default-file))
		nil
		default-file nil)))
       'prompt)
      (if (stringp env-file)
	  (setq br-env-file (expand-file-name env-file)))))

(defun br-env-get-dirs (prompt)
  "PROMPT for and return list of directory names.
PROMPT must contain a %d somewhere in it, so dir # may be inserted."
  (let ((dir) (dirs) (num 1) (default ""))
    (while (not (string-equal "" (setq dir (read-file-name
				       (format prompt num) default "" t))))
      (if (file-directory-p dir)
	  (setq dirs (cons dir dirs)
		num (1+ num)
		default "")
	(beep)
	(setq default dir)))
    (nreverse dirs)))

(defun br-env-init (env-file same-lang same-env)
  "Load or build ENV-FILE if non-nil.
Otherwise, use 'br-env-file' if non-nil or if not, interactively prompt for
Environment name.  SAME-LANG should be non-nil if invoking the OO-Browser on
the same language again.  SAME-ENV should be non-nil if invoking the
OO-Browser on the same Environment again.  br-sys/lib-search-dirs variables
should be set before this function is called.

Return the name of the current Environment file unless load attempt fails,
then return nil."
  (cond 

   ;; Specific environment requested
   (env-file
    ;; Create or load spec and load or build Environment
    (setq env-file (br-env-try-load env-file br-env-file)))
    
   ;; First invocation on this lang
   ((and (null br-sys-search-dirs) (null br-lib-search-dirs))
    ;; Create or load spec and load or build Environment
    (setq env-file
	  (br-env-try-load (or br-env-file (br-env-create)) br-env-file)))
    
   ;; Non-first invocation, search paths have been set, possibly default Env
   (t
    (setq env-file br-env-file)
    (cond
     ;; Continue browsing an Environment
     (same-env nil)
     (same-lang
      ;; But search paths have changed, so rebuild Env
      (progn (or (eq br-sys-search-dirs br-sys-prev-search-dirs)
		 (br-build-sys-htable))
	     (or (eq br-lib-search-dirs br-lib-prev-search-dirs)
		 (br-build-lib-htable))))
     ;; Request to browse a different language Env
     (t
      (setq env-file (br-env-try-load
		      (or br-env-file (br-env-create)) br-env-file))))))
  ;; Return current Env file name unless load attempt failed, then return nil.
  env-file)

(defun *br-env-internal-structures* ()
  "Display values of internal data structures in viewer buffer."
  (interactive)
  (br-funcall-in-view-window
   (concat br-buffer-prefix-info "Info")
   (function
    (lambda ()
      (let ((standard-output (current-buffer)))
	(mapcar
	 (function
	  (lambda (sym)
	    (mapcar
	     (function (lambda (obj)
			 (princ obj)))
	     (list "!!! " (symbol-name sym) " !!!\n\n" 
		   (symbol-value sym) "\n\n"))
	    ))
	 '(br-children-htable
	   br-parents-htable
	   br-paths-htable
	   br-sys-search-dirs
	   br-sys-paths-htable
	   br-sys-parents-htable
	   br-lib-search-dirs
	   br-lib-paths-htable
	   br-lib-parents-htable
	   br-lang-prefix
	   br-env-spec)))))))

(defun br-env-lang-dialog-box (dialog-box)
  "Prompt user with DIALOG-BOX and return selected value.
Assumes caller has checked that 'dialog-box' function exists."
  (let ((echo-keystrokes 0)
	event-obj
	event)	 
    ;; Add a cancel button to dialog box.
    (setq dialog-box (append dialog-box (list nil '["Cancel" abort t])))
    (popup-dialog-box dialog-box)
    (catch 'br-env-done
      (while t
	(setq event (next-command-event event)
	      event-obj (event-object event))
	(cond ((and (menu-event-p event)
		    (memq event-obj '(abort menu-no-selection-hook)))
	       (signal 'quit nil))
	      ((button-release-event-p event) ;; don't beep twice
	       nil)
	      ((menu-event-p event)
	       (throw 'br-env-done (eval event-obj)))
	      (t
	       (beep)
	       (message "Please answer the dialog box.")))))))

(defun br-env-lang-var (lang-prefix)
  "Create language-specific Environment variables for LANG-PREFIX."
  (eval (list 'defvar (intern (concat lang-prefix "env-version"))
	      nil
	      "Version of the OO-Browser used to build the current Environment or nil."))
  (eval (list 'defvar (intern (concat lang-prefix "env-file"))
	      br-env-default-file
	      "*File in which to save Environment.")))

(defun br-env-load-matching-htables (changed-types-list)
  (let ((still-changed-types))
    (if (file-readable-p br-env-file)
	(unwind-protect
	    (progn
	      (let ((buf (get-file-buffer br-env-file)))
		(and buf (kill-buffer buf)))
	      (set-buffer (funcall br-find-file-noselect-function br-env-file))
	      (goto-char (point-min))
	      (mapcar
		(function
		  (lambda (type)
		    (let* ((search-dirs (concat "br-" type "-search-dirs"))
			   (prev-dirs (concat "br-" type "-prev-search-dirs"))
			   (paths (concat "br-" type "-paths-htable"))
			   (parents (concat "br-" type "-parents-htable"))
			   (dirs-val (cdr (br-env-file-sym-val search-dirs))))
		      (if (equal dirs-val (symbol-value (intern search-dirs)))
			  (and (br-member type changed-types-list)
			       (progn (set (intern paths)
					   (cdr (br-env-file-sym-val paths)))
				      (set (intern parents)
					   (cdr (br-env-file-sym-val parents)))
				      (set (intern prev-dirs)
					   (symbol-value
					     (intern search-dirs)))))
			(setq still-changed-types
			      (cons type still-changed-types)))))) 
		'("sys" "lib"))
	      )
	  nil))
    (nreverse still-changed-types)))

(defun br-env-save-mult-vars (mult-vars)
  (let ((br-sym))
    (mapcar
      (function
	(lambda (suffix)
	  (mapcar
	    (function
	      (lambda (type-str)
		(setq br-sym (intern-soft
			       (concat "br-" type-str suffix)))
		(if (and br-sym (boundp br-sym))
		    (let* ((nm (symbol-name br-sym))
			   (nm-mid (string-match "-htable$" nm)))
		      (if nm-mid
			  (progn (princ "\n") (princ (substring nm 0 nm-mid))
				 (princ "-alist\n'")
				 (hash-prin1 (symbol-value br-sym)))
			(princ "\n") (princ br-sym) (princ "\n'")
			(prin1 (symbol-value br-sym))
			(princ "\n"))))))
	    '("sys-" "lib-"))))
      mult-vars)))

(defun br-env-set-htables ()
  (br-env-add-ref-classes "lib")
  (br-env-add-ref-classes "sys")
  ;; Make System entries override Library entries which they duplicate, since
  ;; this is generally more desireable than merging the two.  Don't do this
  ;; for the paths-htable, however, since the value is the union of both
  ;; values.
  (setq br-paths-htable (hash-merge br-sys-paths-htable br-lib-paths-htable))
  (let ((hash-merge-values-function (function (lambda (val1 val2) val1))))
    (setq br-parents-htable (hash-merge br-sys-parents-htable
					br-lib-parents-htable))))

(defun br-env-select-lang ()
  "Interactively select and return value for 'br-lang-prefix'."
  (let ((n 0) (nlangs (length br-env-lang-avector))
	(lang-prompt)
	;; Use dialog box if last user event involved the mouse.
	(use-dialog-box (and (fboundp 'popup-dialog-box)
			     (fboundp 'button-press-event-p)
			     (or (button-press-event-p last-command-event)
				 (button-release-event-p last-command-event)
				 (menu-event-p last-command-event)))))
    ;; Create a prompt numbering each OO-Browser language available.
    (setq lang-prompt
	  (if use-dialog-box
	      (mapcar
	       (function (lambda (lang)
			   (setq n (1+ n))
			   (vector lang (list 'identity n) 't)))
	       (mapcar 'car br-env-lang-avector))
	    (mapconcat
	     (function (lambda (lang)
			 (setq n (1+ n))
			 (format "%d\) %s" n lang)))
	     (mapcar 'car br-env-lang-avector)
	     "; ")))
    ;; Prompt user.
    (while (progn
	     (setq n (if use-dialog-box
			 (br-env-lang-dialog-box
			  (cons "Choose language to browse: " lang-prompt))
		       ;; Otherwise, prompt in the minibuffer.
		       (string-to-int
			(read-string (concat "Choose: " lang-prompt ": ") ""))))
	     (or (< n 1) (> n nlangs)))
      (beep))
    (cdr (aref br-env-lang-avector (1- n)))))

(defun br-env-totals ()
  "Return string of Environment class totals."
  (let ((sys (length (br-all-classes "sys")))
	(lib (length (br-all-classes "lib")))
	(duplicates (car (br-all-classes nil t)))
	count)
    (format "%sTotal unique classes: %d; System: %d; Library: %d"
	    (if (null duplicates)
		""
	      (setq count (length duplicates))
	      (format "%d DUPLICATE CLASS%s TO CONSIDER ELIMINATING:\n\t%s\n\n"
		      count (if (= count 1) "" "ES") duplicates))
	    (+ sys lib) sys lib)))

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defvar br-env-version nil
  "Version of the OO-Browser used to build the current Environment or nil.")

(defconst br-env-mult-vars
  '("search-dirs" "paths-htable" "parents-htable")
  "Descriptors of multiple copy variables saved as part of an Environment.")
(defconst br-env-single-vars
  '("lang-prefix" "env-spec" "children-htable")
  "Descriptors of singular variables saved as part of an Environment.")

(defvar br-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require
updating. Value is language-specific.")

(defvar br-env-lang-avector
  '[("C++"     . "c++-")
    ("Eiffel"  . "eif-")
    ("Info"    . "info-")
    ("Java"    . "java-")
    ("Lisp"    . "clos-")
    ("Obj-C"   . "objc-")
    ("Smalltalk" . "smt-")]
  "Association vector of (LANGUAGE-NAME . LANGUAGE-PREFIX-STRING) elements of OO-Browser languages.")

(mapcar 'br-env-lang-var (mapcar 'cdr br-env-lang-avector))

(provide 'br-env)
