;;!emacs
;;
;; FILE:         hpath.el
;; SUMMARY:      Hyperbole support routines for handling UNIX paths.  
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     comm, hypermedia, unix
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Brown U.
;;
;; ORIG-DATE:     1-Nov-91 at 00:44:23
;; LAST-MOD:     10-Oct-95 at 23:31:56 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1991-1995, Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hpath:rfc "/anonymous@ds.internic.net:rfc/rfc%s.txt"
  "*String to be used in the call: (hpath:rfc rfc-num)
to create an path to the RFC document for 'rfc-num'.")

(defvar hpath:suffixes '(".gz" ".Z")
  "*List of filename suffixes to add or remove within (hpath:exists-p) calls.")

(defvar hpath:tmp-prefix "/tmp/remote-"
  "*Pathname prefix to attach to remote files copied locally for use with external viewers.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hpath:absolute-to (path &optional default-dirs)
  "Returns PATH as an absolute path relative to one directory from optional DEFAULT-DIRS or 'default-directory'.
Returns PATH unchanged when it is not a valid path or when DEFAULT-DIRS
is invalid.  DEFAULT-DIRS when non-nil may be a single directory or a list of
directories.  The first one in which PATH is found is used."
  (if (not (and (stringp path) (hpath:is-p path nil t)))
      path
    (if (not (cond ((null default-dirs)
		    (setq default-dirs (cons default-directory nil)))
		   ((stringp default-dirs)
		    (setq default-dirs (cons default-dirs nil)))
		   ((listp default-dirs))
		   (t nil)))
	path
      (let ((rtn) dir)
	(while (and default-dirs (null rtn))
	  (setq dir (expand-file-name
		     (file-name-as-directory (car default-dirs)))
		rtn (expand-file-name path dir)
		default-dirs (cdr default-dirs))
	  (or (file-exists-p rtn) (setq rtn nil)))
	(or rtn path)))))

(defun hpath:ange-ftp-at-p ()
  "Returns an ange-ftp pathname that point is within or nil.
See the 'ange-ftp' or 'efs' Elisp packages for pathname format details.
Always returns nil if (hpath:ange-ftp-available-p) returns nil."
  (if (hpath:ange-ftp-available-p)
      (let ((user (hpath:ange-ftp-default-user))
	    path)
	(setq path
	      (save-excursion
		(skip-chars-backward "^[ \t\n\"`'\(\{<")
		(cond
		  ((hpath:url-at-p)
		   (if (string-equal
			 (buffer-substring (match-beginning 1) (match-end 1))
			 "ftp")
		       (concat
			"/"
			;; user
			(if (match-beginning 2)
			    (buffer-substring
			     (match-beginning 2) (match-end 2))
			  (concat user "@"))
			;; domain
			(hpath:delete-trailer
			 (buffer-substring (match-beginning 3) (match-end 3)))
			":"
			;; path
			(if (match-beginning 5)
			    (buffer-substring (match-beginning 5)
					      (match-end 5))))
		     ;; else ignore this other type of WWW path
		     ))
		  ;; user, domain and path
		  ((looking-at "/?[^/:@ \t\n\^M\"`']+@[^/:@ \t\n\^M\"`']+:[^]@ \t\n\^M\"`'\)\}]*")
		   (buffer-substring (match-beginning 0) (match-end 0)))
		  ;; @domain and path
		  ((looking-at "@[^/:@ \t\n\^M\"`']+:[^]@ \t\n\^M\"`'\)\}]*")
		   (concat "/" user (buffer-substring
				      (match-beginning 0) (match-end 0))))
		  ;; domain and path
		  ((and (looking-at
			  "/?\\(\\([^/:@ \t\n\^M\"`']+\\):[^]@:, \t\n\^M\"`'\)\}]*\\)[] \t\n\^M,.\"`'\)\}]")
			(setq path (buffer-substring
				     (match-beginning 1) (match-end 1)))
			(string-match "[^.]\\.[^.]"
				      (buffer-substring (match-beginning 2)
							(match-end 2))))
		   (concat "/" user "@" path))
		  ;; host and path
		  ((and (looking-at
			 "/\\([^/:@ \t\n\^M\"`']+:[^]@:, \t\n\^M\"`'\)\}]*\\)")
			(setq path (buffer-substring
				     (match-beginning 1) (match-end 1))))
		   (concat "/" user "@" path))
		  )))
	(hpath:delete-trailer path))))

(defun hpath:ange-ftp-p (path)
  "Returns non-nil iff PATH is an ange-ftp pathname.
See the 'ange-ftp' or 'efs' Elisp package for pathname format details.
Always returns nil if (hpath:ange-ftp-available-p) returns nil."
  (and (stringp path)
       (or (featurep 'ange-ftp) (featurep 'efs))
       (let ((user (hpath:ange-ftp-default-user))
	     result)
	 (setq result
	       (cond
		 ((hpath:url-p path)
		  (if (string-equal
			(substring path (match-beginning 1) (match-end 1))
			"ftp")
		      (concat
			"/"
			;; user
			(if (match-beginning 2)
			    (substring path (match-beginning 2) (match-end 2))
			  (concat user "@"))
			;; domain
			(hpath:delete-trailer
			 (substring path (match-beginning 3) (match-end 3)))
			":"
			;; path
			(if (match-beginning 5)
			    (substring path (match-beginning 5)
				       (match-end 5))))
		    ;; else ignore this other type of WWW path
		    ))
		 ;; user, domain and path
		 ((string-match "/?[^/:@ \t\n\^M\"`']+@[^/:@ \t\n\^M\"`']+:[^]@ \t\n\^M\"`'\)\}]*"
				path)
		  (substring path (match-beginning 0) (match-end 0)))
		 ;; @domain and path
		 ((string-match "@[^/:@ \t\n\^M\"`']+:[^]@ \t\n\^M\"`'\)\}]*"
				path)
		  (concat "/" user
			  (substring path (match-beginning 0) (match-end 0))))
		 ;; domain and path
		 ((and (string-match
			 "/?\\(\\([^/:@ \t\n\^M\"`']+\\):[^]@:, \t\n\^M\"`'\)\}]*\\)"
			 path)
		       (setq result (substring path
					       (match-beginning 1) (match-end 1)))
		       (string-match "[^.]\\.[^.]"
				     (substring path (match-beginning 2)
						(match-end 2))))
		  (concat "/" user "@" result))
		 ;; host and path
		 ((and (string-match
			 "/\\([^/:@ \t\n\^M\"`']+:[^]@:, \t\n\^M\"`'\)\}]*\\)"
			 path)
		       (setq result (substring
				      path
				      (match-beginning 1) (match-end 1))))
		  (concat "/" user "@" result))
		 ))
	(hpath:delete-trailer result))))

(defun hpath:at-p (&optional type non-exist)
  "Returns delimited path or non-delimited ange-ftp path at point, if any.
World wide web urls are treated as non-paths so they are handled elsewhere.
Delimiters may be:  double quotes, open and close single quote, or
Texinfo file references.
If optional TYPE is the symbol 'file or 'directory, then only that path type is
accepted as a match.  Only locally reachable paths are checked for existence.
With optional NON-EXIST, nonexistent local paths are allowed.
Absolute pathnames must begin with a '/' or '~'.  Relative pathnames
must begin with a './' or '../' to be recognized."
  (cond	(;; Local file URLs
	 (hpath:is-p (hargs:delimited
		      "file://localhost" "[ \t\n\^M\"\'\}]" nil t)))
	((hpath:ange-ftp-at-p))
	((hpath:www-at-p) nil)
	((hpath:is-p (or (hargs:delimited "\"" "\"") 
			 ;; Filenames in Info docs
			 (hargs:delimited "\`" "\'")
			 ;; Filenames in TexInfo docs
			 (hargs:delimited "@file{" "}"))
		     type non-exist))))

(defun hpath:find (filename &optional other-window-p)
  "Edit file FILENAME using program from hpath:find-alist if available.
Otherwise, switch to a buffer visiting file FILENAME, creating one if none
already exists.

FILENAME may start with a special prefix character which is
handled as follows:
  !filename  - execute as a non-windowed program within a shell;
  &filename  - execute as a windowed program;
  -filename  - load as an Emacs Lisp program.

Return non-nil iff file is displayed within a buffer (not with an external
program)."
  (interactive "FFind file: ")
  (let (modifier)
    (if (string-match hpath:prefix-regexp filename)
	(setq modifier (aref filename 0)
	      filename (substring filename (match-end 0))))
    (setq filename (hpath:substitute-value filename))
    (cond (modifier (cond ((eq modifier ?!)
			   (hact 'exec-shell-cmd filename))
			  ((eq modifier ?&)
			   (hact 'exec-window-cmd filename))
			  ((eq modifier ?-)
			   (load filename)))
		    nil)
	  (t (let ((find-program (hpath:find-program filename)))
	       (cond ((stringp find-program)
		      (hact 'exec-window-cmd find-program)
		      nil)
		     ((hypb:functionp find-program)
		      (funcall find-program filename)
		      nil)
		     (t (setq filename (hpath:validate filename))
			(funcall (if (and other-window-p
					  (not (br-in-browser)))
				     'switch-to-buffer-other-window
				   'switch-to-buffer)
				 (find-file-noselect filename))
			t)))))))

(defun hpath:find-other-window (filename)
  "Edit file FILENAME, in another window or using program from hpath:find-alist.
May create a new window, or reuse an existing one; see the function display-buffer.

Alternatively, FILENAME may start with a prefix character to indicate special
handling.  See documentation of `hpath:find' for details.

Return non-nil iff file is displayed within a buffer."
  (interactive "FFind file in other window: ")
  (hpath:find filename t))

(defun hpath:is-p (path &optional type non-exist)
  "Returns PATH if PATH is a Unix path, else nil.
If optional TYPE is the symbol 'file or 'directory, then only that path type
is accepted as a match.  The existence of the path is checked only for
locally reachable paths (Info paths are not checked).  Single spaces are
permitted in middle of existing pathnames, but not at the start or end.  Tabs
and newlines are converted to space before the pathname is checked, this
normalized path form is what is returned for PATH.  With optional NON-EXIST,
nonexistent local paths are allowed."
  (let ((rtn-path path)
	(suffix))
    (and (stringp path)
	 ;; Path may be a link reference with other components other than a
	 ;; pathname.  These components always follow a comma, so strip them,
	 ;; if any, before checking path.
	 (if (string-match "[ \t\n\^M]*," path)
	     (setq rtn-path (concat (substring path 0 (match-beginning 0))
				     "%s" (substring path (match-beginning 0)))
		   path (substring path 0 (match-beginning 0)))
	   (setq rtn-path (concat rtn-path "%s")))
	 (if (string-match hpath:prefix-regexp path)
	     (setq path (substring path (match-end 0)))
	   t)
	 (not (or (string= path "")
		  (string-match "\\`\\s \\|\\s \\'" path)))
	 ;; Convert tabs and newlines to space.
	 (setq path (hbut:key-to-label (hbut:label-to-key path)))
	 (or (not (string-match "[()]" path))
	     (string-match "\\`([^ \t\n\^M\)]+)[ *A-Za-z0-9]" path))
	 (if (string-match "\\${[^}]+}" path)
	     (setq path (hpath:substitute-value path))
	   t)
	 (not (string-match "[\t\n\^M\"`'{}|\\]" path))
	 (or (not (hpath:www-p path))
	     (string-match "^ftp:" path))
	 (let ((remote-path (string-match "@.+:\\|^/.+:\\|.+:/" path)))
	   (if (cond (remote-path
		      (cond ((eq type 'file)
			     (not (string-equal "/" (substring path -1))))
			    ((eq type 'directory)
			     (string-equal "/" (substring path -1)))
			    (t)))
		     ((or (and non-exist
			       (or
				;; Info or ange-ftp path, so don't check for.
				(string-match "[()]" path)
				(hpath:ange-ftp-p path)
				(setq suffix (hpath:exists-p path t))
				;; Don't allow spaces in non-existent
				;; pathnames.
				(not (string-match " " path))))
			  (setq suffix (hpath:exists-p path t)))
		      (cond ((eq type 'file)
			     (not (file-directory-p path)))
			    ((eq type 'directory)
			     (file-directory-p path))
			    (t)))
		     )
	       ;; Return path if non-nil return value
	       (if (stringp suffix) ;; suffix could = t, which we ignore
		   (if (string-match
			(concat (regexp-quote suffix) "%s") rtn-path)
		       ;; remove suffix
		       (concat (substring rtn-path 0 (match-beginning 0))
			       (substring rtn-path (match-end 0)))
		     ;; add suffix
		     (format rtn-path suffix))
		 (format rtn-path "")))))))

(defun hpath:relative-to (path &optional default-dir)
  "Returns PATH relative to optional DEFAULT-DIR or 'default-directory'.
Returns PATH unchanged when it is not a valid path."
  (if (not (and (stringp path) (hpath:is-p path)))
      path
    (setq default-dir
	  (expand-file-name
	   (file-name-as-directory (or default-dir default-directory)))
	  path (expand-file-name path))
    (and path default-dir
	 (if (string-equal "/" default-dir)
	     path
	   (let ((end-dir (min (length path) (length default-dir))))
	     (cond
	      ((string-equal (substring path 0 end-dir) default-dir)
	       (concat "./" (substring path end-dir)))
	      ((progn (setq default-dir (file-name-directory (directory-file-name default-dir))
			    end-dir (min (length path) (length default-dir)))
		      (string-equal (substring path 0 end-dir) default-dir))
	       (concat "../" (substring path end-dir)))
	      ((progn (setq default-dir (file-name-directory (directory-file-name default-dir))
			    end-dir (min (length path) (length default-dir)))
		      (string-equal (substring path 0 end-dir) default-dir))
	       (concat "../../" (substring path end-dir)))
	      (t path)))))))

(defun hpath:rfc (rfc-num)
  "Return pathname to textual rfc document indexed by RFC-NUM.
See the documentation of the 'hpath:rfc' variable."
  (format hpath:rfc rfc-num))

(defun hpath:substitute-value (path)
  "Substitutes matching value for Emacs Lisp variables and environment variables in PATH.
Returns path with variable values substituted."
  (substitute-in-file-name
    (hypb:replace-match-string
      "\\${[^}]+}"
      path
      (function
	(lambda (str)
	  (let* ((var-group (substring path match start))
		 (var-name (substring path (+ match 2) (1- start)))
		 (rest-of-path (substring path start))
		 (sym (intern-soft var-name)))
	    (if (file-name-absolute-p rest-of-path)
		(setq rest-of-path (substring rest-of-path 1)))
	    (if (and sym (boundp sym))
		(directory-file-name
		 (hpath:substitute-dir var-name rest-of-path))
	      var-group))))
      t)))

(defun hpath:substitute-var (path)
  "Replaces up to one match in PATH with first matching variable from 'hpath:variables'."
  (if (not (and (stringp path) (string-match "/" path) (hpath:is-p path)))
      path
    (setq path (hpath:symlink-referent path))
    (let ((new-path)
	  (vars hpath:variables)	  
	  result var val)
      (while (and vars (null new-path))
	(setq var (car vars) vars (cdr vars))
	(if (boundp var)
	    (progn (setq val (symbol-value var))
		   (cond ((stringp val)
			  (if (setq result
				    (hpath:substitute-var-name var val path))
			      (setq new-path result)))
			 ((null val))
			 ((listp val)
			  (while (and val (null new-path))
			    (if (setq result
				    (hpath:substitute-var-name var (car val) path))
				(setq new-path result))
			    (setq val (cdr val))))
			 (t (error "(hpath:substitute-var): '%s' has invalid value for hpath:variables" var))))))
      (or new-path path)
      )))

;;
;; The following function recursively resolves all UNIX links to their
;; final referents.
;; Works with Apollo's variant and other strange links like:
;; /usr/local -> $(SERVER_LOCAL)/usr/local, /usr/bin ->
;; ../$(SYSTYPE)/usr/bin and /tmp -> `node_data/tmp.  It also handles
;; relative links properly as in /usr/local/emacs -> gnu/emacs which must
;; be resolved relative to the '/usr/local' directory.
;; It will fail on Apollos if the '../' notation is used to move just
;; above the '/' directory level.  This is fairly uncommon and so the
;; problem has not been fixed.
;;
(defun hpath:symlink-referent (linkname)
  "Returns expanded file or directory referent of LINKNAME.
LINKNAME should not end with a directory delimiter.
Returns nil if LINKNAME is not a string.
Returns LINKNAME unchanged if it is not a symbolic link but is a pathname."
  (if (stringp linkname)
      (or (file-symlink-p linkname) linkname)))

(defun hpath:symlink-expand (referent dirname)
  "Returns expanded file or directory REFERENT relative to DIRNAME."
  (let ((var-link)
	(dir dirname))
    (while (string-match "\\$(\\([^\)]*\\))" referent)
      (setq var-link (getenv (substring referent (match-beginning 1)
					(match-end 1)))
	    referent (concat (substring referent 0 (match-beginning 0))
			     var-link
			     (substring referent (match-end 0)))))
    ;; If referent is not an absolute path
    (let ((nd-abbrev (string-match "`node_data" referent)))
      (if (and nd-abbrev (= nd-abbrev 0))
	  (setq referent (concat
			   ;; Prepend node name given in dirname, if any
			   (and (string-match "^//[^/]+" dirname)
				(substring dirname 0 (match-end 0)))
			   "/sys/" (substring referent 1)))))
    (while (string-match "\\(^\\|/\\)\\.\\.\\(/\\|$\\)" referent)
      ;; Match to "//.." or "/.." at the start of link referent
      (while (string-match "^\\(//\\.\\.\\|/\\.\\.\\)\\(/\\|$\\)" referent)
	(setq referent (substring referent (match-end 1))))
      ;; Match to "../" or ".." at the start of link referent
      (while (string-match "^\\.\\.\\(/\\|$\\)" referent)
	(setq dir (file-name-directory (directory-file-name dir))
	      referent (concat dir (substring referent (match-end 0)))))
      ;; Match to rest of "../" in link referent
      (while (string-match "[^/]+/\\.\\./" referent)
	(setq referent (concat (substring referent 0 (match-beginning 0))
			       (substring referent (match-end 0))))))
    (and (/= (aref referent 0) ?~)
	 (/= (aref referent 0) ?/)
	 (setq referent (expand-file-name referent dirname))))
  referent)

(defun hpath:validate (path)
  "Returns PATH if PATH is a valid, readable path, else signals error.
Info and ange-ftp remote pathnames are considered readable without any
validation checks.
Default-directory should be equal to current Hyperbole button source
directory when called, so that PATH is expanded relative to it." 
  (cond ((not (stringp path))
	 (error "(hpath:validate): \"%s\" is not a pathname." path))
	((or (string-match "[()]" path) (hpath:ange-ftp-p path))
	 ;; info or ange-ftp path, so don't validate
	 path)
	((if (not (hpath:www-p path))
	     ;; Otherwise, must not be a WWW link ref and must be a readable
	     ;; path.
	     (let ((return-path (hpath:exists-p path)))
	       (and return-path (file-readable-p return-path)
		    return-path))))
	(t (error "(hpath:validate): \"%s\" is not readable." path))))

(defun hpath:url-at-p ()
  "Return world-wide-web universal resource locator (url) that point immediately precedes or nil.
Use buffer-substring with match-beginning and match-end on the following
groupings:
  1 = access protocol
  2 = optional username
  3 = host and domain to connect to
  4 = optional port number to use
  5 = pathname to access."
  ;; WWW URL format:  <protocol>:/[<user>@]<domain>[:<port>]/<path>
  ;;             or   <protocol>://[<user>@]<domain>[:<port>]<path>
  ;; Avoid [a-z]:/path patterns since these may be disk paths on OS/2, DOS or
  ;; Windows.
  (if (looking-at "\\([a-zA-Z][a-zA-Z]+\\)://?\\([^@/: \t\n\^M]+@\\)?\\([^/:@ \t\n\^M\"`']+\\)\\(:[0-9]+\\)?\\([/~][^]@ \t\n\^M\"`'\)\}>]*\\)?")
      (save-excursion
	(goto-char (match-end 0))
	(skip-chars-backward ".?#!*()")
	(buffer-substring (match-beginning 0) (point)))))

(defun hpath:url-p (obj)
  "Return t if OBJ is a world-wide-web universal resource locator (url) string, else nil.
Use string-match with match-beginning and match-end on the following groupings:
  1 = access protocol
  2 = optional username
  3 = host and domain to connect to
  4 = optional port number to use
  5 = pathname to access."
  ;; WWW URL format:  <protocol>:/[<user>@]<domain>[:<port>]/<path>
  ;;             or   <protocol>://[<user>@]<domain>[:<port>]<path>
  ;; Avoid [a-z]:/path patterns since these may be disk paths on OS/2, DOS or
  ;; Windows.
  (and (stringp obj)
       (string-match "\\`<?\\([a-zA-Z][a-zA-Z]+\\)://?\\([^@/: \t\n\^M]+@\\)?\\([^/:@ \t\n\^M\"`']+\\)\\(:[0-9]+\\)?\\([/~][^]@ \t\n\^M\"`'\)\}>]*\\)?>?\\'"
		     obj)
       t))

(defun hpath:www-at-p (&optional include-start-and-end-p)
  "Returns a world-wide-web link reference that point is within or nil.
With optional INCLUDE-START-AND-END-P non-nil, returns list of:
  (link-string begin-position end-position)."
  (save-excursion
    (skip-chars-backward "^[ \t\n\"`'\(\{<")
    (cond ((not include-start-and-end-p)
	   (hpath:url-at-p))
	  ((hpath:url-at-p)
	   (list (buffer-substring (match-beginning 0) (match-end 0))
		 (match-beginning 0)
		 (match-end 0))))))

(defun hpath:www-p (path)
  "Returns non-nil iff PATH is a world-wide-web link reference."
  (and (stringp path) (hpath:url-p path) path))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hpath:ange-ftp-available-p ()
  "Return t if the ange-ftp or efs package is available, nil otherwise.
Either the package must have been loaded already or under versions of Emacs
19, it must be set for autoloading via 'file-name-handler-alist'."
  (or (featurep 'ange-ftp) (featurep 'efs)
      (and (boundp 'file-name-handler-alist) ; v19
	   (or (rassq 'ange-ftp-hook-function file-name-handler-alist)
	       (rassq 'efs-file-handler-function file-name-handler-alist))
	   t)))



(defun hpath:ange-ftp-default-user ()
  "Return default user account for remote file access with ange-ftp or efs.
Returns \"anonymous\" if neither ange-ftp-default-user nor efs-default-user
is set."
  (cond ((and (boundp 'ange-ftp-default-user)
	      (stringp ange-ftp-default-user))
	 ange-ftp-default-user)
	((and (boundp 'efs-default-user)
	      (stringp efs-default-user))
	 efs-default-user)
	(t "anonymous")))

(defun hpath:delete-trailer (string)
  "Return string minus any trailing .?#!*() characters."
  (save-match-data
    (if (and (stringp string) (> (length string) 0)
	     (string-match "[.?#!*()]+\\'" string))
	(substring string 0 (match-beginning 0))
      string)))

(defun hpath:exists-p (path &optional suffix-flag)
  "Return PATH if it exists.  (This does not mean you can read it.)
If PATH exists with or without a suffix from hpath:suffixes, then that
pathname is returned.

With optional SUFFIX-FLAG and PATH exists, return suffix added or removed
from path or t."
  (let ((return-path)
	(suffix) suffixes)
    (if (file-exists-p path)
	(setq return-path path)
      (setq suffixes hpath:suffixes)
      (while suffixes
	(setq suffix (car suffixes))
	(if (string-match (concat (regexp-quote suffix) "\\'") path)
	    ;; Remove suffix
	    (setq return-path (substring path 0 (match-beginning 0)))
	  ;; Add suffix
	  (setq return-path (concat path suffix)))
	(if (file-exists-p return-path)
	    (setq suffixes nil);; found a match
	  (setq suffix nil
		suffixes (cdr suffixes)
		return-path nil))))
    (if return-path
	(if suffix-flag
	    (or suffix t)
	  return-path))))

(defun hpath:find-program (filename)
  "Return shell or Lisp command to execute to display FILENAME or nil.
Return nil if FILENAME is a directory name.
See also documentation for 'hpath:find-alist' and 'hpath:display-alist'."
  (let ((cmd))
    (cond ((and (stringp filename) (file-directory-p filename))
	   nil)
	  ((stringp (setq cmd (hpath:match filename hpath:find-alist)))
	   (let ((orig-path filename))
	     ;; If filename is a remote path, we have to copy it to a
	     ;; temporary local file and then display that.
	     (if (hpath:ange-ftp-p filename)
		 (copy-file orig-path
			    (setq filename
				  (concat hpath:tmp-prefix
					  (file-name-nondirectory orig-path)))
			    t t))
	     ;;
	     ;; Permit %s substitution of filename within program.
	     (if (string-match "[^%]%s" cmd)
		 (format cmd filename)
	       (concat cmd " " filename))))
	  ((null cmd)
	   (hpath:match filename hpath:display-alist))
	  (t cmd))))

(defun hpath:match (filename regexp-alist)
  "If FILENAME matches the car of any element in REGEXP-ALIST, return its cdr.
REGEXP-ALIST elements must be of the form (<filename-regexp>
. <command-to-display-file>).  <command-to-display-file> may be a string
representing an external window-system command to run or it may be a Lisp
function to call with FILENAME as its single argument."
  (let ((cmd)
	elt)
    (while (and (not cmd) regexp-alist)
      (if (string-match (car (setq elt (car regexp-alist))) filename)
	  (setq cmd (cdr elt)))
      (setq regexp-alist (cdr regexp-alist)))
    cmd))

(defun hpath:substitute-dir (var-name rest-of-path)
  "Returns a dir for VAR-NAME using REST-OF-PATH to find match or triggers an error when no match.
VAR-NAME's value may be a directory or a list of directories.  If it is a
list, the first directory prepended to REST-OF-PATH which produces a valid
local pathname is returned."
  (let (sym val)
    (cond ((not (stringp var-name))
	   (error "(hpath:substitute-dir): VAR-NAME arg, '%s', must be a string" var-name))
	  ((not (and (setq sym (intern-soft var-name))
		     (boundp sym)))
	   (error "(hpath:substitute-dir): VAR-NAME arg, \"%s\", is not a bound variable"
		  var-name))
	  ((stringp (setq val (symbol-value sym)))
	   (if (hpath:validate (expand-file-name rest-of-path val))
	       val))
	  ((listp val)
	   (let ((dir))
	     (while (and val (not dir))
	       (setq dir (car val) val (cdr val))
	       (or (and (stringp dir)
			(file-name-absolute-p dir)
			(file-readable-p (expand-file-name rest-of-path dir)))
		   (setq dir nil)))
	     (if dir (hpath:validate (directory-file-name dir))
	       (error "(hpath:substitute-dir): Can't find match for \"%s\""
		      (concat "${" var-name "}/" rest-of-path))
	       )))
	  (t (error "(hpath:substitute-dir): Value of VAR-NAME, \"%s\", must be a string or list" var-name))
	  )))

(defun hpath:substitute-var-name (var-symbol var-dir-val path)
  "Replaces with VAR-SYMBOL any occurrences of VAR-DIR-VAL in PATH.
Replacement is done iff VAR-DIR-VAL is an absolute path.
If PATH is modified, returns PATH, otherwise returns nil."
  (if (and (stringp var-dir-val) (file-name-absolute-p var-dir-val))
      (let ((new-path (hypb:replace-match-string
			(regexp-quote (file-name-as-directory
					(or var-dir-val default-directory)))
			path (concat "${" (symbol-name var-symbol) "}/")
			t)))
	(if (equal new-path path) nil new-path))))


;;; ************************************************************************
;;; Private variables
;;; ************************************************************************


(defvar hpath:prefix-regexp "\\`[-!&][ ]*"
  "Regexp matching command characters which may precede a pathname.
These are used to indicate how to display or execute the pathname.
  - means evaluate it as Emacs Lisp;
  ! means execute it as a shell script
  & means run it under the current window system.")

(provide 'hpath)
