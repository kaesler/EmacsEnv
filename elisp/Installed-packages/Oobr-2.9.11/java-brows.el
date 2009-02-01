;;!emacs
;;
;; FILE:         java-brows.el
;; SUMMARY:      Java source code browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     java, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola Inc.
;;
;; ORIG-DATE:    01-Aug-95
;; LAST-MOD:     20-Sep-95 at 14:18:44 by Bob Weiner
;;
;; Copyright (C) 1995  Free Software Foundation, Inc.
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;    Use 'java-browse' to invoke the java OO-Browser.  Prefix arg prompts for
;;    name of Environment file.
;;
;; DESCRIP-END.

;; ************************************************************************
;; Other required Elisp libraries
;; ************************************************************************

(mapcar 'require '(br-start br br-java-ft))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun java-browse (&optional env-file no-ui)
  "Invoke the Java OO-Browser.
This allows browsing through Java library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file to
use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"."
  (interactive "P")
  (let ((same-lang (equal br-lang-prefix java-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix java-lang-prefix
	    *br-save-wconfig* nil))
    (setq same-env (or (equal java-env-file env-file)
		       (and (null env-file)
			    (or java-lib-search-dirs java-sys-search-dirs))))
    (cond
     ;; Continue browsing an Environment
     ((and same-env same-lang))
     ((and same-env (not same-lang))
      (java-browse-setup) (br-env-copy t))
     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p java-env-file)
	    (br-env-create java-env-file java-lang-prefix))
	(or env-file (setq env-file java-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(java-browse-setup)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  java-env-file load-succeeded
		  java-sys-search-dirs br-sys-search-dirs
		  java-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (br-init)
	   (or no-ui (br-browse)))
	  (no-ui nil)
	  (t (message "(java-browse): You must build the Environment to browse it.")))))

;; Don't filter Environment classes when listed.
(fset 'java-class-list-filter 'identity)

(defun java-mode-setup ()
  "Load best available java major mode and set 'br-lang-mode' to the function that invokes it."
  (fset 'br-lang-mode
	(cond ((or (featurep 'java-mode)
		   (load "java-mode" 'missing-ok 'nomessage))
	       'java-mode)
	      ((featurep 'cc-mode)
	       'c++-mode)
	      ((load "cc-mode" 'missing-ok 'nomessage)
	       (provide 'c++-mode))
	      (t (error "(java-mode-setup): Can't load major mode for Java code.")))))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun java-browse-setup ()
  "Setup language-dependent functions for OO-Browser."
  (br-setup-functions)
  ;; Use this until an info function is implemented for the language.
  (fmakunbound 'br-insert-class-info)
  (fset 'br-store-class-info 'java-store-class-info)
  (java-mode-setup)
  (br-setup-constants))

(provide 'java-brows)
