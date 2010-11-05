;;!emacs
;;
;; LCD-ENTRY:    hyperbole|Bob Weiner|hyperbole@hub.ucsb.edu|Everyday Info Manager|02-Nov-95|4.00|ftp.cs.uiuc.edu:/pub/xemacs/infodock/
;;
;; FILE:         hversion.el
;; SUMMARY:      Hyperbole version, system and load path information.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Brown U.
;;
;; ORIG-DATE:     1-Jan-94
;; LAST-MOD:      2-Nov-95 at 02:39:17 by Bob Weiner
;;
;; This file is part of Hyperbole.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1994, 1995  Free Software Foundation, Inc.
;; Developed with support from Motorola Inc.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst hyperb:version "04.00" "Hyperbole revision number.")

;;; Support button highlighting and flashing under XEmacs.
;;;
(defvar hyperb:xemacs-p
  (let ((case-fold-search t))
    (if (string-match "XEmacs" emacs-version)
	emacs-version))
  "Version string under XEmacs (not Lucid Emacs) or nil")

;;; Support button highlighting and flashing under obsolete Lucid Emacs.
;;;
(defvar hyperb:lemacs-p
  (let ((case-fold-search t))
    (if (string-match "XEmacs\\|Lucid" emacs-version)
	emacs-version))
  "Version string under XEmacs or Lucid Emacs or nil")

;;; Support mouse handling under GNU Emacs V19.
;;;
(defvar hyperb:emacs19-p
  (and (not hyperb:lemacs-p)
       (string-match "^19\\." emacs-version)
       emacs-version)
  "Version string under GNU Emacs 19 or nil")

;;; Support button highlighting and flashing under obsolete Epoch.
;;;
(defvar hyperb:epoch-p
  (if (and (boundp 'epoch::version)
	   (stringp epoch::version))
      (if (string< epoch::version "Epoch 4") "V3" "V4"))
  "Simplified version string under Epoch, e.g. \"V4\", or nil")

;; Koutlines work only with specific versions of Emacs 19 and XEmacs.
(defconst hyperb:kotl-p
  (if hyperb:lemacs-p
      ;; Only works for XEmacs 19.9 and above.
      (string-match "^19\\.9 \\|^19\\.[1-9][0-9]" emacs-version)
    hyperb:emacs19-p)
  "Non-nil iff this Emacs version supports the Hyperbole outliner.")

(defun sm-window-sys-term ()
  "Returns the first part of the term-type if running under a window system, else nil.
Where a part in the term-type is delimited by a '-' or  an '_'."
  (let ((term (cond ((memq window-system '(x ns dps pm))
		     ;; X11, NEXTSTEP (DPS), or OS/2 Presentation Manager (PM)
		     (cond (hyperb:emacs19-p "emacs19")
			   (hyperb:lemacs-p  "lemacs")
			   (hyperb:epoch-p   "epoch")
			   (t                "xterm")))
		    ((or (featurep 'eterm-fns)
			 (equal (getenv "TERM") "NeXT")
			 (equal (getenv "TERM") "eterm"))
		     ;; NEXTSTEP add-on support to Emacs
		     "next")
		    ((or window-system 
			 (featurep 'sun-mouse) (featurep 'apollo))
		     (getenv "TERM")))))
    (and term
	 (substring term 0 (string-match "[-_]" term)))))

(defconst hyperb:window-system (sm-window-sys-term)
  "String name for window system or term type under which Emacs was run.
If nil, no window system or mouse support is available.")

;;; ************************************************************************
;;; Public functions to dynamically compute Hyperbole directory.
;;; ************************************************************************

(defvar hyperb:automount-prefixes
  (if (and (boundp 'automount-dir-prefix) (stringp automount-dir-prefix))
      automount-dir-prefix
    "^/tmp_mnt/"
    "*Regexp to match any automounter prefix in a pathname."))

(defun hyperb:stack-frame (function-list &optional debug-flag)
  "Return the nearest Emacs Lisp stack frame which called any function symbol from FUNCTION-LIST or nil if no match.
If FUNCTION-LIST contains 'load, 'autoload or 'require, detect
autoloads not visible within the Lisp level stack frames.

With optional DEBUG-FLAG non-nil, if no matching frame is found, return list
of stack frames (from innermost to outermost)."
  (let ((count 0)
	(frame-list)
	(load-flag (or (memq 'load function-list)
		       (memq 'autoload function-list)
		       (memq 'require function-list)))
	fsymbol
	fbody
	frame)
    (or (catch 'hyperb:stack-frame
	  (while (setq frame (backtrace-frame count))
	    (if debug-flag (setq frame-list (cons frame frame-list)))
	    (setq count (1+ count)
		  fsymbol (nth 1 frame))
	    (and (eq fsymbol 'command-execute)
		 (not (memq 'command-execute function-list))
		 ;; Use command being executed instead because it might not
		 ;; show up in the stack anywhere else, e.g. if it is an
		 ;; autoload under Emacs 19.
		 (setq fsymbol (nth 2 frame)))
	    (cond ((and load-flag (symbolp fsymbol)
			(fboundp fsymbol)
			(listp (setq fbody (symbol-function fsymbol)))
			(eq (car fbody) 'autoload))
		   (setq frame (list (car frame) 'load
				     (car (cdr fbody))
				     nil noninteractive nil))
		   (throw 'hyperb:stack-frame frame))
		  ((memq fsymbol function-list)
		   (throw 'hyperb:stack-frame frame))))
	  nil)
	(if debug-flag (nreverse frame-list)))))

(defun hyperb:path-being-loaded ()
  "Return the full pathname used by the innermost `load' or 'require' call.
Removes any matches for `hyperb:automount-prefixes' before returning
the pathname."
  (let* ((frame (hyperb:stack-frame '(load require)))
	 (function (nth 1 frame))
	 file nosuffix)
    (cond ((eq function 'load)
	   (setq file (nth 2 frame)
		 nosuffix (nth 5 frame)))
	  ((eq function 'require)
	   (setq file (or (nth 3 frame) (symbol-name (nth 2 frame))))))
    (if (stringp file)
	(setq nosuffix (or nosuffix
			   (string-match
			    "\\.\\(elc?\\|elc?\\.gz\\|elc?\\.Z\\)$"
			    file))
	      file (substitute-in-file-name file)
	      file (locate-file file load-path
				(if nosuffix "" ".elc:.el:.el.gz:.el.Z:")
				;; accept any existing file
				0)
	      file (if (and (stringp file)
			    (string-match hyperb:automount-prefixes file))
		       (substring file (1- (match-end 0)))
		     file)))))

(if (fboundp 'locate-file)
    nil
  (defun locate-file (file dir-list &optional suffix-string unused)
    "Search for FILE in DIR-LIST.
If optional SUFFIX-STRING is provided, allow file to be followed by one of the
colon separated suffixes."
    (let ((suffix-list))
      (cond ((null suffix-string) (setq suffix-list '("")))
	    ((stringp suffix-string)
	     (let ((start 0)
		   (len  (length suffix-string)))
	       (while (and (< start len)
			   (string-match "[^:]+" suffix-string start))
		 (setq suffix-list
		       (cons (substring suffix-string
					(match-beginning 0)
					(match-end 0))
			     suffix-list)
		       start (1+ (match-end 0))))
	       (setq suffix-list (nconc (nreverse suffix-list) '("")))))
	    (t (error "(locate-file): Invalid third arg, '%s', use a colon separated string of file suffixes"
		      suffix-string)))
      (if (and (file-name-absolute-p file) (file-readable-p file))
	  file;; file exists without suffix addition, so return it
	(if (file-name-absolute-p file) (setq dir-list '(nil)))
	(if (equal file "") (error "(locate-file): Empty file argument"))
	(let (suffixes pathname)
	  ;; Search dir-list for a matching, readable file.
	  (catch 'found
	    (while dir-list
	      (setq suffixes suffix-list)
	      (while suffixes
		(setq pathname (expand-file-name
				(concat file (car suffixes))
				(car dir-list)))
		(if (file-readable-p pathname)
		    (throw 'found pathname))
		(setq suffixes (cdr suffixes)))
	      (setq dir-list (cdr dir-list)))))))))

;;; ************************************************************************
;;; Public functions used by pulldown and popup menus
;;; ************************************************************************

(if (not (fboundp 'id-browse-file))
    (fset 'id-browse-file 'find-file-read-only))

(if (not (fboundp 'id-info))
    (defun id-info (node)
      (if (br-in-browser) (br-to-view-window))
      (Info-goto-node node)))

(if (not (fboundp 'id-tool-quit)) (fset 'id-tool-quit 'eval))

(if (not (fboundp 'id-tool-invoke))
    (defun id-tool-invoke (sexp)
      (if (commandp sexp)
	  (call-interactively sexp)
	(funcall sexp))))

(provide 'hversion)
